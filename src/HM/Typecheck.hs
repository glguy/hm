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
import           Data.Bifunctor
import           Data.List ((\\))
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           GHC.Generics

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
  Term (UPoly t v) e {- ^ Term to type check with atoms replace with their types -} ->
  M e t v (UMono t v)  {- ^ Computed type of given term -}
inferType env term =
   case term of
    Atom a -> instantiate a

    Var v  ->
      do s <- case view (at v) env of
                Nothing -> throwEitherKT (EnvironmentError v)
                Just a  -> return a
         instantiate s

    e1 :$ e2 ->
      do resultType <- freshType
         t1 <- inferType env e1
         t2 <- inferType env e2
         _  <- t1 =:= UMonoApp mkFunction [t2,resultType]
         return resultType

    Abs x e ->
      do argType <- freshType
         let env' = set (at x) (Just (UPoly [] argType)) env
         t1 <- inferType env' e
         return (UMonoApp mkFunction [argType,t1])

    Let x e1 e2 ->
      do t  <- freshType
         t1 <- inferType (set (at x) (Just (UPoly [] t)) env) e1
         t2 <- t =:= t1
         t3 <- generalize env t2
         let env' = set (at x) (Just t3) env
         inferType env' e2

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
  Term (Poly t v) e  {- ^ term to typecheck  -} ->
  M e t v (Poly t v)   {- ^ most general type of term -}
typecheck' env term =
  do let uenv = envFromMap env
     t           <- inferType uenv (first polyUnfreeze term)
     UPoly vs t1 <- generalize uenv t
     t2          <- applyBindings t1
     case freeze t2 of
       Nothing -> fail "typecheck: implementation bug"
       Just t3 -> return (Poly vs t3)

-- | Compute the most general type of a term assuming a given
-- environment.
typecheck ::
  (Show e, NameSource v, Ord e, Ord v, TypeCon t, Eq t) =>
  Map e (Poly t v)   {- ^ typing environment -} ->
  Term (Poly t v) e  {- ^ term to typecheck  -} ->
  Either (TcError e t v) (Poly t v)
     {- ^ most general type of term or error explaining failure -}
typecheck env term = runM (typecheck' env term)
