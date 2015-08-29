{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module provides the typing environment used in the type checker.
module HM.Env
  ( -- * Types
    Env

  , -- * Operations
    emptyEnv
  , envFromMap
  ) where

import           Control.Lens
import           Data.Map (Map)
import qualified Data.Map as Map
import           GHC.Generics

import           HM.Poly

-- | The type of the environment mapping bindings to types.
-- Use the 'At', 'Ixed', and 'Each' instances to access the
-- contents of this container.
newtype Env e t v = Env (Map e (UPoly t v))
  deriving (Show, Eq, Generic)

-- | Empty environment.
emptyEnv :: Env e v t
emptyEnv = Env Map.empty

-- | Generate environment from a map of binders to polymorphic types.
envFromMap :: Map e (Poly t v) -> Env e t v
envFromMap = Env . fmap polyUnfreeze

type instance Index   (Env e t v) = e
type instance IxValue (Env e t v) = UPoly t v

instance Ord e => Ixed (Env e t v) where
  ix = ixAt

instance Ord e => At (Env e t v) where
  at i f (Env a) = Env <$> at i f a

instance Each (Env e t v) (Env e t' v') (UPoly t v) (UPoly t' v') where
  each f (Env a) = Env <$> each f a
