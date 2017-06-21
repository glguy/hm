{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module HM.Annotation where

import Control.Lens
import GHC.Generics
import Data.Functor.Fixedpoint

type Annot a f = Fix (AnnotF a f)

data AnnotF a f r = AnnotF a (f r)
  deriving (Show, Read, Functor, Generic)

pattern Annot :: a -> f (Annot a f) -> Annot a f
pattern Annot a b = Fix (AnnotF a b)

instance Foldable f => Foldable (AnnotF a f) where
  foldMap f (AnnotF _ x) = foldMap f x

instance Traversable f => Traversable (AnnotF a f) where
  traverse f (AnnotF a b) = AnnotF a <$> traverse f b

traverseAnnot :: Traversal (AnnotF a f r) (AnnotF b f r) a b
traverseAnnot f (AnnotF a r) = (`AnnotF` r) <$> f a
