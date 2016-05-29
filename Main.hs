import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    stateLookup env var -- crashes if the variable doesn't exist
    e <- evalExpr env expr
    setVar var e

evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt                = return Nil

evalStmt env (VarDeclStmt [])         = return Nil
evalStmt env (VarDeclStmt (decl:ds))  =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)

evalStmt env (ExprStmt expr)          = evalExpr env expr

--inicio da edicao

--Bloco de comandos
evalStmt env (BlockStmt [])           = return Nil
evalStmt env (BlockStmt [stmt])       = evalStmt env stmt 
evalStmt env (BlockStmt (st:stmts))   =
    evalStmt env st >> evalStmt env (BlockStmt stmts)
--If simples
evalStmt env (IfSingleStmt expr stmt) = do
    x <- evalExpr env expr
    if (evalBoolean x)
        then evalStmt env stmt 
        else return Nil
--If com else
evalStmt env (IfStmt expr stmt stmt2) = do
    x <- evalExpr env expr
    if (evalBoolean x)
        then evalStmt env stmt 
        else evalStmt env stmt2
--While
evalStmt env (WhileStmt expr stmt)    = do
    x <- evalExpr env expr
    if (evalBoolean x)
        then evalStmt env stmt >> evalStmt env (WhileStmt expr stmt)
        else return Nil
--Do-While
evalStmt env (DoWhileStmt stmt expr)  = do
    evalStmt env stmt
    x <- evalExpr env expr
    if (evalBoolean x)
        then evalStmt env (WhileStmt expr stmt)
        else return Nil
--For
evalStmt env (ForStmt init test inc stmt) = do
    case init of
        NoInit -> return Nil
        VarInit vdl -> varDeclList env vdl
        ExprInit expr -> evalExpr env expr
    case test of
        Nothing -> do
            case inc of
                Nothing -> evalStmt env stmt >> evalStmt env (ForStmt NoInit test inc stmt)
                Just incM -> evalStmt env stmt >> evalExpr env incM >> evalStmt env (ForStmt NoInit test inc stmt)
        Just testM -> do
            evaluedTest <- evalExpr env testM
            if (evalBoolean evaluedTest)
                then case inc of
                    Nothing -> evalStmt env stmt >> evalStmt env (ForStmt NoInit test inc stmt)
                    Just incM -> evalStmt env stmt >> evalExpr env incM >> evalStmt env (ForStmt NoInit test inc stmt)
                else return Nil

evalBoolean :: Value -> Bool
evalBoolean (Bool b) = b

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

--
-- Environment and auxiliary functions
--

environment :: Map String Value
environment = Map.empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    case Map.lookup var (union s env) of
        Nothing -> error $ "Variable " ++ show var ++ " not defiend."
        Just val -> (val, s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id val

varDeclList :: StateT -> [VarDecl] -> StateTransformer Value
varDeclList env [] = return Nil
varDeclList env vdl = foldl1 (>>) $ map (varDecl env) vdl

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

--
-- Types and boilerplate
--

type StateT = Map String Value
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, defs) =
    show val ++ "\n" ++ show (toList $ union defs environment) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f Map.empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
