{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module implements type checking on the 'Term' language.
module HM.Typecheck
  ( -- * Type checking operations
    NameSource(..)
  , TcError(..)
  , typecheck
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.EitherK
import           Control.Monad.Trans.Class
import           Control.Unification
import           Control.Unification.IntVar
import           Data.Functor.Fixedpoint
import           Data.List ((\\))
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           GHC.Generics

import           HM.Annotation
import           HM.Env
import           HM.Mono
import           HM.Poly
import           HM.Term

-- | Class for types that can be used as type variables. The
-- 'nameSource' list will be consulted during generalization
-- when picking names for the newly quantified variables.
class    NameSource a       where nameSource :: [a]
instance NameSource Integer where nameSource = [0..]

-- | Errors that can occur during type checking.
data TcError e t v
    -- | A cyclic term was encountered. See 'occursFailure'
  = OccursError IntVar (UMono t v)

    -- | The top-most level of the terms do not match. See 'mismatchFailure'
  | MismatchError (MonoF t v (UMono t v)) (MonoF t v (UMono t v))

    -- | A binder didn't exist in the environment
  | EnvironmentError e
  deriving (Show, Eq, Generic)

instance Fallible (MonoF t v) IntVar (TcError e t v) where
  occursFailure   = OccursError
  mismatchFailure x y = MismatchError x y

-- | Type used when running the type checker which provides
-- unification and exception handling
type M e t v = EitherKT (TcError e t v) (IntBindingT (MonoF t v) Identity)

-- | Unwrap 'M'
runM :: M e t v a -> Either (TcError e t v) a
runM = runIdentity . evalIntBindingT . runEitherKT

-- | Generate a fresh unification type variable.
freshType :: (Eq t, Eq v) => M e t v (UMono t v)
freshType = UVar <$> lift freeVar

-- | Infer the most general type for a given term within the given
-- environment. The returned type will be monomorphic but will
-- contain unification variables for the unconstrained parts.
inferType :: (NameSource v, Show e, TypeCon t, Ord e, Ord v) =>
  Env e t v          {- ^ Current typing environment -} ->
  (a -> Poly t v)    {- ^ typing function for atoms -} ->
  Annot (UMono t v) (TermF a e) {- ^ Term to type check -} ->
  M e t v (UMono t v)  {- ^ Computed type of given term -}
inferType env atomType (Annot u e) =
  case e of
    AtomF a ->
      do t <- instantiate (polyUnfreeze (atomType a))
         u =:= t

    VarF v  ->
      do s <- case view (at v) env of
                Nothing -> throwEitherKT (EnvironmentError v)
                Just a  -> return a
         t <- instantiate s
         u =:= t

    AppF e1 e2 ->
      do t1 <- inferType env atomType e1
         t2 <- inferType env atomType e2
         _  <- t1 =:= UMonoApp mkFunction [t2,u]
         return u

    AbsF x e ->
      do argType <- freshType
         let env' = set (at x) (Just (UPoly [] argType)) env
         t1 <- inferType env' atomType e
         u =:= UMonoApp mkFunction [argType,t1]

    LetF x e1 e2 ->
      do t  <- freshType
         t1 <- inferType (set (at x) (Just (UPoly [] t)) env) atomType e1
         t2 <- t =:= t1
         t3 <- generalize env t2
         let env' = set (at x) (Just t3) env
         t4 <- inferType env' atomType e2
         u =:= t4

-- | Generalize a type by universally quantifying the
-- unconstrainted unification variables in the given
-- type.
generalize :: (Eq t, Ord v, NameSource v) =>
  Env e t v         {- ^ typing environment -} ->
  UMono t v         {- ^ type to generalize -} ->
  M e t v (UPoly t v) {- ^ generalized type   -}
generalize env t = lift $
  do envVars <- getFreeVarsAll (toListOf (each . upolyMono) env)
     termVars <- getFreeVars t
     let freeVars = termVars \\ envVars
         usedVars = umonoFreeVars t
         availNames = filter (`Set.notMember` usedVars) nameSource
         pickedNames = zipWith const availNames freeVars
     zipWithM_ (\x y -> bindVar x (UMonoVar y)) freeVars pickedNames
     return (UPoly pickedNames t)

-- | Substitute all of the quantified types variables in a polymorphic
-- type with unification variables.
instantiate :: (Eq t, Ord v) => UPoly t v -> M e t v (UMono t v)
instantiate (UPoly vs t) =
  do subst <- Map.fromList <$> traverse aux vs
     substUMono subst <$> applyBindings t
  where
  aux v =
    do u <- lift freeVar
       return (v,u)

-- | Compute the most general type of a term assuming a given
-- environment.
typecheck' ::
  (Show e, NameSource v, Ord e, Ord v, TypeCon t, Eq t) =>
  Map e (Poly t v)   {- ^ typing environment -} ->
  (a -> Poly t v) ->
  Annot (UMono t v) (TermF a e)  {- ^ term to typecheck  -} ->
  M e t v (Poly t v, Annot (Mono t v) (TermF a e)) {- ^ most general type of term -}
typecheck' env atomType term =
  do let uenv = envFromMap env
     t           <- inferType uenv atomType term
     UPoly vs t1 <- generalize uenv t
     t2          <- applyBindings t1
     s <- case freeze t2 of
       Nothing -> fail "typecheck: implementation bug"
       Just t3 -> return (Poly vs t3)
     term' <- hmapM (traverseAnnot freezeM) term
     return (s, term')

freezeM :: (Eq t, Eq v) => UMono t v -> M e t v (Mono t v)
freezeM t =
  do t' <- applyBindings t
     let Just m = freeze t'
     return m

addTypes ::
  (Eq t, Ord v) =>
  Term a e ->
  M e t v (Annot (UMono t v) (TermF a e))
addTypes = cataM $ \t ->
  do u <- freshType
     return (Annot u t)

-- | Compute the most general type of a term assuming a given
-- environment.
typecheck ::
  (Show e, NameSource v, Ord e, Ord v, TypeCon t, Eq t) =>
  Map e (Poly t v)   {- ^ typing environment -} ->
  (a -> Poly t v)    {- ^ typing rule for primitives -} ->
  Term a e  {- ^ term to typecheck  -} ->
  Either (TcError e t v) (Poly t v, Annot (Mono t v) (TermF a e))
     {- ^ most general type of term or error explaining failure -}
typecheck env atomTypes term = runM (typecheck' env atomTypes =<< addTypes term)
