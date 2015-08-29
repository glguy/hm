{-# LANGUAGE DeriveGeneric #-}

-- | This module provides parameterized polymorphic types.
module HM.Poly
  ( -- * Types
    Poly(..)
  , UPoly(..)

  , -- * Operations
    polyUnfreeze

  , -- * Optics
    upolyBinds
  , upolyMono
  ) where

import Control.Lens
import Control.Unification
import GHC.Generics

import HM.Mono

-- | Polymorphics types
data Poly t v = Poly [v] (Mono t v)
  deriving (Read, Show, Eq, Generic)

-- | Polymorphics types with unification variables
data UPoly t v = UPoly [v] (UMono t v)
  deriving (Show, Eq, Generic)

-- | Lens for accessing the type-part of a 'UPoly'
upolyBinds :: Lens' (UPoly t v) [v]
upolyBinds f (UPoly xs t) = (`UPoly` t) <$> f xs

-- | Lens for accessing the type-part of a 'UPoly'
upolyMono :: Lens (UPoly t v) (UPoly t' v) (UMono t v) (UMono t' v)
upolyMono f (UPoly xs t) = UPoly xs <$> f t

-- | Trivially convert a polymorphic type to one with unification variables
polyUnfreeze :: Poly t v -> UPoly t v
polyUnfreeze (Poly vs t) = UPoly vs (unfreeze t)
