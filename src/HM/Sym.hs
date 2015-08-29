-- | This module provides a symbol type for easy to read
-- binders and type variables.
module HM.Sym
  ( -- * Types
    Sym(..)
  ) where

import Control.Lens
import Data.String

import HM.Typecheck

newtype Sym = Sym String
  deriving (Eq, Ord)

instance Show Sym where
  show (Sym s) = show s

instance Read Sym where
  readsPrec p s = over (mapped . _1) Sym (readsPrec p s)

instance IsString Sym where
  fromString = Sym

-- | @nameSource = "a":"b".."z":"a0":"b0".."z0":"a1"...@
instance NameSource Sym where
  nameSource =
    do n <- "" : map show [(0::Integer) ..]
       v <- ['a'..'z']
       return (Sym (v : n))
