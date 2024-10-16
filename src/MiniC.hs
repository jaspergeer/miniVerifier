{-# LANGUAGE DuplicateRecordFields #-}

-- Object Language

module MiniC where
import qualified Data.Set as Set

type Name = String

data Expr
    = Lit {v :: Int}
    | Add {lhs :: Expr, rhs :: Expr}
    | Sub {lhs :: Expr, rhs :: Expr}
    | Select {index :: Expr}
    | Var {n :: Name}
    | QuantVar {n::Name}
    deriving Show

l .+ r = Add l r
l .- r = Sub l r
sel i = Select i
var n = Var n
qv n = QuantVar n
lit v = Lit v

data LogExpr
    = Eq {lhs :: Expr, rhs :: Expr}
    | Lt {lhs :: Expr, rhs :: Expr}
    | Gt {lhs :: Expr, rhs :: Expr}
    | Not {le :: LogExpr}
    | Or {lhsl :: LogExpr, rhsl :: LogExpr}
    | And {lhsl :: LogExpr, rhsl :: LogExpr}
    | Impl {lhsl :: LogExpr, rhsl :: LogExpr}
    | True
    | False
    deriving Show

l .== r = Eq l r
l .< r = Lt l r
l .> r = Gt l r
l .>= r = (l .> r) .|| (l .== r)
l .<= r = (l .< r) .|| (l .== r)
l .|| r = Or l r
l .&& r = And l r
l .=> r = Impl l r

data Forall = Forall {vars :: [Name], body :: LogExpr} deriving Show

forAll v b = Forall v b

data Stmt
    = Store {index :: Expr, e :: Expr}
    | Assign {v :: Name, e :: Expr}
    | While {cond :: LogExpr, inv :: Forall, body :: [Stmt]}
    | If {cond :: LogExpr, thn :: [Stmt], els :: [Stmt]}
    | Assert {fa :: Forall}
    | Assume {fa :: Forall}
    deriving Show

store i e = Store i e
v .:= e = Assign v e
while c i b = While c i b
if' c t e = If c t e
assert f = Assert f
assume f = Assume f

mutatable :: Stmt -> Set.Set Name
mutatable (Store index e) = Set.empty
mutatable (Assign v e) = Set.singleton v
mutatable (While cond inv body) = Set.unions $ map mutatable body
mutatable (If cond thn els) = Set.unions $ map mutatable (thn <> els)
mutatable (Assert fa) = Set.empty
mutatable (Assume fa) = Set.empty

data Prog = Prog
    { formals :: [Name]
    , vars :: [Name]
    , body :: [Stmt]
    }

prog f v b = Prog f v b
