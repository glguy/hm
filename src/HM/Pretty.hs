-- | This module provides pretty-printing functionality. It works like
-- the 'Show' class, but it doesn't presume to construct Haskell syntax.
module HM.Pretty
  ( -- * Classes
    Pretty(..)
  , Pretty1(..)

  , -- * Operations
    pretty
  ) where

import Data.Functor.Fixedpoint

import HM.Term
import HM.Poly
import HM.Mono
import HM.Sym

-- | Pretty-printing function for a term within a surrounding precedence.
class Pretty  a where
  prettyPrec :: Int {- ^ precedence -} -> a -> ShowS

-- | Pretty-printing function for types with kind @(* -> *)@
class Pretty1 f where
  prettyPrec1 :: Pretty a => Int {- ^ precedence -} -> f a -> ShowS

instance Pretty Int where prettyPrec = showsPrec
instance Pretty Integer where prettyPrec = showsPrec

-- | Pretty-print a term using the default precendence (0).
pretty :: Pretty a => a -> String
pretty x = prettyPrec 0 x ""

instance Pretty1 f => Pretty (Fix f) where
  prettyPrec p (Fix x) = prettyPrec1 p x

instance (Pretty v, Pretty t) => Pretty (Poly v t) where
  prettyPrec p (Poly [] t) = prettyPrec p t
  prettyPrec p (Poly vs t)
    = showParen (p > 0)
    $ showString "∀"
    . foldr (.) id (map (\x -> showChar ' ' . prettyPrec 11 x) vs)
    . showString ". "
    . prettyPrec 1 t

instance (Pretty v, Pretty t) => Pretty1 (MonoF v t) where
  prettyPrec1 = prettyPrec

instance (Pretty v, Pretty t, Pretty r) => Pretty (MonoF v t r) where
  prettyPrec p (MonoVarF v) = prettyPrec p v
  prettyPrec p (MonoAppF c []) = prettyPrec p c
  prettyPrec p (MonoAppF c ts)
    = showParen (p > 10)
    $ prettyPrec 10 c
    . foldr (.) id (map (\x -> showChar ' ' . prettyPrec 11 x) ts)

instance (Pretty t, Pretty v) => Pretty (Term t v) where
  prettyPrec p (Atom a)
    = prettyPrec p a
  prettyPrec p (x :$ y)
    = showParen (p > 10)
    $ prettyPrec 10 x
    . showChar ' '
    . prettyPrec 11 y
  prettyPrec p (Var v)
    = prettyPrec p v
  prettyPrec p (Abs v x)
    = showParen (p > 0)
    $ showString "λ "
    . prettyPrec 11 v
    . showString ". "
    . prettyPrec 0 x
  prettyPrec p (Let v x y)
    = showParen (p > 0)
    $ showString "let "
    . prettyPrec 11 v
    . showString " = "
    . prettyPrec 0 x
    . showString " in "
    . prettyPrec 0 y


instance Pretty Sym where
  prettyPrec _ (Sym x) = showString x
