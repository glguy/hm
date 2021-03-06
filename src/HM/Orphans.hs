{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides instances that are missing from
-- other libraries
--
-- > instance Read1 f => Read (Fix f) where
-- > instance Eq1 t => Eq (UTerm t) where
-- > instance Unifiable [] where
-- > instance Traversable t => Plated (UTerm t v) where

module HM.Orphans () where

import Control.Lens
import Control.Unification
import Data.Functor.Fixedpoint
import Prelude.Extras

-- | Silently adds @Fix@ constructors
instance Read1 f => Read (Fix f) where
  readsPrec p s = over (mapped . _1) Fix (readsPrec1 p s)

-- | Unifies the pairwise elements of two lists and
-- requires lists to be the same length.
instance Unifiable [] where
  zipMatch []     []     = Just []
  zipMatch (x:xs) (y:ys) = (Right (x,y) :) <$> zipMatch xs ys
  zipMatch _      _      = Nothing

instance Traversable t => Plated (UTerm t v) where
  plate _ u@UVar{}  = pure u
  plate f (UTerm t) = UTerm <$> traverse f t

instance (Eq1 t, Eq v) => Eq (UTerm t v) where
  UVar  x == UVar  y = x ==  y
  UTerm x == UTerm y = x ==# y
  _       == _       = False
