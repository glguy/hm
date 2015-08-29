{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides an example use of the typechecking
-- machinery for a simple language with integers, functions,
-- maps, and logical types.
module HM.Example where

import           Data.Bifunctor
import qualified Data.Map as Map

import           HM.Term
import           HM.Mono
import           HM.Poly
import           HM.Sym
import           HM.Pretty
import           HM.Typecheck

------------------------------------------------------------------------
-- Example Primitive Types
------------------------------------------------------------------------

-- | Primitive type constructors
data MyType = LogicType | IntType | MapType | FunctionType
  deriving (Read,Show,Eq,Ord)

instance TypeCon MyType where
  mkFunction = FunctionType

-- | Construct int type
intType :: Mono MyType Sym
intType = MonoApp IntType []

-- | Construct logic type
logicType :: Mono MyType Sym
logicType = MonoApp LogicType []

-- | Construct map type
mapType :: Mono MyType Sym -> Mono MyType Sym -> Mono MyType Sym
mapType k v = MonoApp MapType [k,v]

------------------------------------------------------------------------
-- Example Primitive Terms
------------------------------------------------------------------------

-- | Primitive operations
data MyTerm
  = Add -- ^ addition :: int -> int -> int
  | And -- ^ logical conjunction :: logic -> logic -> logic
  | Equal -- ^ value equality :: a -> a -> logic
  | Select -- ^ map indexing :: map k v -> k -> v
  | LInt Int -- ^ literal integer :: int
  deriving (Read,Show,Eq,Ord)

-- | Compute the type for a primitive.
termType :: MyTerm -> Poly MyType Sym
termType LInt{} = Poly [] intType
termType Add = Poly [] (intType --> intType --> intType)
termType And = Poly [] (logicType --> logicType --> logicType)
termType Equal =
  let a = MonoVar "a"
  in Poly ["a"] (a --> a --> logicType)
termType Select =
  let k = MonoVar "k"
      v = MonoVar "v"
  in Poly ["k","v"] (mapType k v --> k --> v)

demoExpr :: Term MyTerm Sym
demoExpr
  = Abs "x"
  $ Abs "y"
  $ Let "z" (Atom Equal :$ (Atom Add :$ Var "x" :$ Var "x") :$ Atom (LInt 10))
  $ Var "z"

-- | Pretty-print and term and its type
demo :: Term MyTerm Sym -> IO ()
demo expr =
  do putStrLn $ pretty expr
     putStrLn "::"
     let res = typecheck Map.empty $ first termType expr
     case res of
       Left e -> print e
       Right r -> putStrLn (pretty r)


instance Pretty MyType where
  prettyPrec _ LogicType        = showString "logic"
  prettyPrec _ IntType          = showString "int"
  prettyPrec _ MapType          = showString "map"
  prettyPrec _ FunctionType     = showString "function"

instance Pretty MyTerm where
  prettyPrec _ Add              = showString "add"
  prettyPrec _ And              = showString "and"
  prettyPrec _ Equal            = showString "equal"
  prettyPrec _ Select           = showString "select"
  prettyPrec p (LInt x)         = prettyPrec p x

