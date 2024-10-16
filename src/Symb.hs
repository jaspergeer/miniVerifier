{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- Symbolic Expressions

module Symb where

type Name = String

data Expr
  = Lit {v :: Int}
  | Add {lhs :: Expr, rhs :: Expr}
  | Sub {lhs :: Expr, rhs :: Expr}
  | Select {arr :: SymbArr, index :: Expr}
  | Var {n :: Name}
  | QuantVar {n::Name}

sexpr :: [String] -> String
sexpr [] = "()"
sexpr [x] = "(" <> x <> ")"
sexpr (x : xs) = "(" <> x <> " " <> unwords xs <> ")"

instance Show Expr where
  show (Lit v) = show v
  show (Add lhs rhs) = sexpr ["+", show lhs, show rhs]
  show (Sub lhs rhs) = sexpr ["-", show lhs, show rhs]
  show (Select arr index) = sexpr ["select", show arr, show index]
  show (Var n) = n
  show (QuantVar n) = n

data LogExpr
  = Eq {lhs :: Expr, rhs :: Expr}
  | Lt {lhs :: Expr, rhs :: Expr}
  | Gt {lhs :: Expr, rhs :: Expr}
  | Not {le :: LogExpr}
  | And {lhsl :: LogExpr, rhsl :: LogExpr}
  | Or {lhsl :: LogExpr, rhsl :: LogExpr}
  | Impl {lhsl :: LogExpr, rhsl :: LogExpr}
  | Forall {vars :: [Name], body :: LogExpr}
  | True
  | False

instance Show LogExpr where
  show (Eq lhs rhs) = sexpr ["=", show lhs, show rhs]
  show (Lt lhs rhs) = sexpr ["<", show lhs, show rhs]
  show (Gt lhs rhs) = sexpr [">", show lhs, show rhs]
  show (Not le) = sexpr ["not", show le]
  show (And lhs rhs) = sexpr ["and", show lhs, show rhs]
  show (Or lhs rhs) = sexpr ["or", show lhs, show rhs]
  show (Impl lhs rhs) = sexpr ["=>", show lhs, show rhs]
  show (Forall vars body) =
    if null vars
      then show body
      else sexpr ["forall (" <> varDecls <> ")", show body]
   where
    varDecls = unwords $ map (\n -> sexpr [n, "Int"]) vars
  show Symb.True = "true"
  show Symb.False = "false"

data SymbArr = ArrVar {n :: Name} | Store {arr :: SymbArr, index :: Expr, v :: Expr}

instance Show SymbArr where
  show (ArrVar n) = n
  show (Store arr index v) = sexpr ["store", show arr, show index, show v]
