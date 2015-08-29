{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}

-- | This module provides a term type that is parameterized on
-- a language's primitives and binders. The primitives and
-- binders are then extended to support functions and named
-- expressions.
module HM.Term where

import Control.Applicative
import Control.Lens
import GHC.Generics

-- | Terms parameterized by the type of primitive in the language
-- and the types of binders. The primitives are extended to add
-- support for abstraction, application, and naming.
data Term t v
  = Atom t               -- ^ Primitive operation
  | Term t v :$ Term t v -- ^ Function application
  | Var v                -- ^ Variable
  | Abs v (Term t v)     -- ^ Abstraction
  | Let v (Term t v) (Term t v) -- ^ Let binding
  deriving (Read, Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

infixl 1 :$

instance Bifunctor Term where
  bimap f _ (Atom t) = Atom (f t)
  bimap f g (x :$ y) = bimap f g x :$ bimap f g y
  bimap _ g (Var v) = Var (g v)
  bimap f g (Abs x y) = Abs (g x) (bimap f g y)
  bimap f g (Let x y z) = Let (g x) (bimap f g y) (bimap f g z)

instance Plated (Term t v) where
  plate f (x :$ y) = liftA2 (:$) (f x) (f y)
  plate f (Let x y z) = liftA2 (Let x) (f y) (f z)
  plate f (Abs x y) = fmap (Abs x) (f y)
  plate _ t@Atom{} = pure t
  plate _ t@Var{} = pure t
