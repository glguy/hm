{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

-- | This module provides a term type that is parameterized on
-- a language's primitives and binders. The primitives and
-- binders are then extended to support functions and named
-- expressions.
module HM.Term
  ( -- * Types
    TermF(..)
  , Term
  , pattern Atom
  , pattern (:$)
  , pattern Var
  , pattern Abs
  , pattern Let

  , -- * Operations
    mapAtom
  ) where

import Control.Lens
import Data.Functor.Fixedpoint
import GHC.Generics
import Prelude.Extras

type Term t v = Fix (TermF t v)

-- | Terms parameterized by the type of primitive in the language
-- and the types of binders. The primitives are extended to add
-- support for abstraction, application, and naming.
data TermF t v r
  = AtomF t    -- ^ Primitive operation
  | AppF r r    -- ^ Function application
  | VarF v     -- ^ Variable
  | AbsF v r   -- ^ Abstraction
  | LetF v r r -- ^ Let binding
  deriving (Read, Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1)

pattern Atom :: t -> Term t v
pattern Atom t = Fix (AtomF t)

pattern (:$) :: Term t v -> Term t v -> Term t v
pattern f :$ x = Fix (AppF f x)

pattern Var :: v -> Term t v
pattern Var v  = Fix (VarF v)

pattern Abs :: v -> Term t v -> Term t v
pattern Abs v t = Fix (AbsF v t)

pattern Let :: v -> Term t v -> Term t v -> Term t v
pattern Let v t1 t2 = Fix (LetF v t1 t2)

instance (Eq t, Eq v) => Eq1 (TermF t v)
instance (Ord t, Ord v) => Ord1 (TermF t v)
instance (Show t, Show v) => Show1 (TermF t v)

infixl 1 :$

termAtomF :: Traversal (TermF t v r) (TermF t' v r) t t'
termAtomF f t =
  case t of
    AtomF a      -> AtomF <$> f a
    AppF t1 t2   -> pure (AppF t1 t2)
    VarF v       -> pure (VarF v)
    AbsF v r     -> pure (AbsF v r)
    LetF v t1 t2 -> pure (LetF v t1 t2)

mapAtom :: (a -> b) -> Term a v -> Term b v
mapAtom f = cata (Fix . over termAtomF f)
