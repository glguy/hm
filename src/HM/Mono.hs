{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}

-- | This module provides parameterized monomorphic types.
module HM.Mono
  ( -- * Types shape
    MonoF(..)

  , -- * Without unification variables
    Mono
  , pattern MonoApp
  , pattern MonoVar

  , -- * With unification variables
    UMono
  , pattern UMonoApp
  , pattern UMonoVar

  , -- * Types with functions
    TypeCon(..)
  , (-->)

  , -- * Operations
    substUMono
  , umonoFreeVars
  ) where

import           Control.Lens
import           Control.Unification
import           Control.Unification.IntVar
import           Data.Functor.Fixedpoint
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics
import           Prelude.Extras

import           HM.Orphans()

-- | Monomorphic types
--
-- @
-- data 'Mono' t v
--   = 'MonoApp' t ['Mono' t v]
--   | 'MonoVar' v
-- @
type Mono t v = Fix (MonoF t v)

pattern MonoApp f xs = Fix (MonoAppF f xs)
pattern MonoVar v    = Fix (MonoVarF v)

-- | Monomorphic types with unification variables
--
-- @
-- data 'UMono' t v
--   = 'UMonoApp' t ['UMono' t v]
--   | 'UMonoVar' v
--   | 'UVar' 'IntVar'
-- @
type UMono t v = UTerm (MonoF t v) IntVar

pattern UMonoApp f xs = UTerm (MonoAppF f xs)
pattern UMonoVar v    = UTerm (MonoVarF v)

-- | Shape of monomorphic types
data MonoF t v r
  = MonoAppF t [r] -- ^ Primitive type applied to parameters
  | MonoVarF v     -- ^ Type variable
  deriving (Read, Show, Eq, Ord, Functor, Traversable, Foldable, Generic, Generic1)

instance (Eq   t, Eq   v) => Eq1   (MonoF t v)
instance (Ord  t, Ord  v) => Ord1  (MonoF t v)
instance (Show t, Show v) => Show1 (MonoF t v)
instance (Read t, Read v) => Read1 (MonoF t v)

instance (Eq t, Eq v) => Unifiable (MonoF t v) where
  zipMatch (MonoVarF u)    (MonoVarF v)    | u == v = Just (MonoVarF u)
  zipMatch (MonoAppF f xs) (MonoAppF g ys) | f == g = MonoAppF f <$> zipMatch xs ys
  zipMatch _              _                         = Nothing

-- | Compute the set of free type variables (not unification variables).
umonoFreeVars :: Ord v => UMono t v -> Set v
umonoFreeVars (UMonoVar v)    = Set.singleton v
umonoFreeVars (UMonoApp _ xs) = foldMap umonoFreeVars xs
umonoFreeVars _               = Set.empty

-- | Substitute type variables with unification variables.
substUMono :: Ord v => Map v IntVar -> UMono t v -> UMono t v
substUMono subst = transform $ \x ->
  case x of
    UMonoVar v | Just u <- Map.lookup v subst -> UVar u
    _                                         -> x

-- | Class for types that have a function type. This is used in the
-- type checker for typing functions and abstractions.
class Eq p => TypeCon p where mkFunction :: p

-- | Pretty syntax for constructing function types.
(-->) :: TypeCon t => Mono t v -> Mono t v -> Mono t v
a --> b = MonoApp mkFunction [a,b]

infixr 1 -->
