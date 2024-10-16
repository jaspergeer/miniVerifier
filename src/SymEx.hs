{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- Symbolic Execution

module SymEx where

import Control.Monad.State.Lazy
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Unique (hashUnique, newUnique)
import Debug.Trace (trace)
import GHC.IO (unsafePerformIO)
import qualified MiniC as C
import qualified Symb as S
import Control.Monad (MonadPlus(mplus))

type Name = String

data ExecState = ExecState
    { pathCond :: S.LogExpr
    , assertions :: [S.LogExpr]
    , env :: Map.Map Name S.Expr
    , heap :: S.SymbArr
    , freshVars :: Set.Set Name
    , heaps :: Set.Set Name
    }

type SymEx a = StateT ExecState [] a

data SymExResult = SymExResult
    { formals :: [Name]
    , vars :: [Name]
    , freshVars :: [Name]
    , heaps :: [Name]
    , assertions :: [S.LogExpr]
    }

instance Show SymExResult where
    show seResult =
        unlines
            [ "; Config"
            , "(set-logic ALL)"
            , "(set-option :produce-models true)"
            , "(set-option :produce-unsat-cores true)"
            ]
            <> "; Formal Parameters\n"
            <> unlines (map (\n -> S.sexpr ["declare-const", n, "Int"]) seResult.formals)
            <> "; Variables\n"
            <> unlines (map (\n -> S.sexpr ["declare-const", n, "Int"]) seResult.vars)
            <> "; Havoc Variables\n"
            <> unlines (map (\n -> S.sexpr ["declare-const", n, "Int"]) seResult.freshVars)
            <> "; Initial Heap\n"
            <> "(declare-const initHeap (Array Int Int))\n"
            <> "; Havoc Heaps\n"
            <> unlines (map (\n -> S.sexpr ["declare-const", n, "(Array Int Int)"]) seResult.heaps)
            <> "; Assertions\n"
            <> "(assert (forall ("
            <> concatMap (\n -> S.sexpr [n, "Int"]) seResult.formals
            <> ")\n"
            <> "  (not (and\n"
            <> unlines (map (\a -> "    " <> show a) seResult.assertions)
            <> "))))\n"
            <> "(check-sat)\n"

symbolicExecution :: C.Prog -> SymExResult
symbolicExecution prog = SymExResult prog.formals prog.vars combinedFreshVars combinedHeaps combinedAssertions
  where
    initEnvBindings = Map.fromList $ map (\n -> (n, S.Var n)) prog.formals
    initState = ExecState S.True [] initEnvBindings (S.ArrVar "initHeap") Set.empty Set.empty
    runResult = map snd $ runStateT (traverse_ stmt prog.body) initState
    combinedAssertions = concatMap (\es -> es.assertions) runResult
    combinedFreshVars = Set.toList $ mconcat $ map (\es -> es.freshVars) runResult
    combinedHeaps = Set.toList $ mconcat $ map (\es -> es.heaps) runResult

getEnv :: Name -> SymEx S.Expr
getEnv n = do
    st <- get
    let (Just v) = Map.lookup n st.env
    return v

setEnv :: Name -> S.Expr -> SymEx ()
setEnv n v = do
    st <- get
    put $ st{env = Map.insert n v st.env}

selectHeap :: S.Expr -> SymEx S.Expr
selectHeap index = do
    st <- get
    return $ S.Select st.heap index

storeHeap :: S.Expr -> S.Expr -> SymEx ()
storeHeap index v = do
    st <- get
    put $ st{heap = S.Store st.heap index v}

{-# NOINLINE havoc #-}
havoc :: [Name] -> SymEx ()
havoc vars = do
    st <- get
    let newHeap = "heap" <> show (hashUnique $ unsafePerformIO newUnique)
    put $ st {heap = S.ArrVar newHeap, heaps = Set.insert newHeap st.heaps}
    traverse_
        ( \n ->
            let freshName = n <> show (hashUnique $ unsafePerformIO newUnique)
            in do
                st' <- get
                put $ (st' :: ExecState){freshVars = Set.insert freshName st'.freshVars}
                setEnv n (S.Var freshName)
        )
        vars

setPathCond :: S.LogExpr -> SymEx ()
setPathCond newCond = do
    st <- get
    put $ st{pathCond = newCond}

addToPathCond :: S.LogExpr -> SymEx ()
addToPathCond cond = do
    pc <- getPathCond
    setPathCond $ S.And pc cond

getPathCond :: SymEx S.LogExpr
getPathCond = do
    st <- get
    return st.pathCond

addToAsserts :: S.LogExpr -> SymEx ()
addToAsserts cond = do
    st <- get
    put $ (st :: ExecState){assertions = cond : st.assertions}

branch :: SymEx a -> SymEx a -> SymEx a
branch = mplus

expr :: C.Expr -> SymEx S.Expr
expr (C.Lit v) = return $ S.Lit v
expr (C.Var n) = getEnv n
expr (C.QuantVar n) = return $ S.QuantVar n
expr (C.Select index) = expr index >>= selectHeap
expr (C.Add lhs rhs) = do
    l <- expr lhs
    r <- expr rhs
    return $ S.Add l r
expr (C.Sub lhs rhs) = do
    l <- expr lhs
    r <- expr rhs
    return $ S.Sub l r

logExpr :: C.LogExpr -> SymEx S.LogExpr
logExpr (C.Eq lhs rhs) = do
    l <- expr lhs
    r <- expr rhs
    return $ S.Eq l r
logExpr (C.Lt lhs rhs) = do
    l <- expr lhs
    r <- expr rhs
    return $ S.Lt l r
logExpr (C.Gt lhs rhs) = do
    l <- expr lhs
    r <- expr rhs
    return $ S.Gt l r
logExpr (C.Not le) = do
    sle <- logExpr le
    return $ S.Not sle
logExpr (C.Or lhs rhs) = do
    l <- logExpr lhs
    r <- logExpr rhs
    return $ S.Or l r
logExpr (C.And lhs rhs) = do
    l <- logExpr lhs
    r <- logExpr rhs
    return $ S.And l r
logExpr (C.Impl lhs rhs) = do
    l <- logExpr lhs
    r <- logExpr rhs
    return $ S.Impl l r
logExpr C.True = return S.True
logExpr C.False = return S.False

forallExpr :: C.Forall -> SymEx S.LogExpr
forallExpr (C.Forall vars body) = do
    bdy <- logExpr body
    return $ S.Forall vars bdy

stmt :: C.Stmt -> SymEx ()
stmt (C.If cond thn els) = do
    cnd <- logExpr cond
    _ <-
        branch
            (do addToPathCond cnd; traverse stmt thn)
            (do addToPathCond (S.Not cnd); traverse stmt els)
    pure ()
stmt (C.Store index e) = do
    idx <- expr index
    sx <- expr e
    storeHeap idx sx
stmt (C.Assign v e) = do
    sx <- expr e
    setEnv v sx
stmt (C.Assert fa) = do
    sfa <- forallExpr fa
    pc <- getPathCond
    addToAsserts $ S.Impl pc sfa
stmt (C.Assume fa) = do
    sfa <- forallExpr fa
    addToPathCond sfa
stmt (C.While cond inv body) = do
    let mutatable = Set.toList (Set.unions $ map C.mutatable body)
    invBefore <- forallExpr inv
    pcBefore <- getPathCond
    -- the path condition must imply the loop invariant before we enter
    addToAsserts $ S.Impl pcBefore invBefore

    -- execute the body
    -- havoc the heap and any variables that may be mutated
    havoc mutatable

    -- we have cond /\ inv as our path condition
    invBeginBody <- forallExpr inv
    condBeginBody <- logExpr cond
    addToPathCond $ S.And invBeginBody condBeginBody

    traverse_ stmt body
    pcEndBody <- getPathCond
    invEndBody <- forallExpr inv

    -- check that cond /\ inv => inv'
    addToAsserts $ S.Impl pcEndBody invEndBody

    -- leave the body
    havoc mutatable
    invAfter <- forallExpr inv
    condAfter <- logExpr cond
    setPathCond $ S.And pcBefore $ S.And invAfter (S.Not condAfter)
