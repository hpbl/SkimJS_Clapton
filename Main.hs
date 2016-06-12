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
evalExpr env NullLit = return Nil
evalExpr env (StringLit str) = return (String str)
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool

evalExpr env (ArrayLit list) = do
    case list of
        [] -> return (List [])
        (x:xs) -> do
            hd <- evalExpr env x
            (List tl) <- evalExpr env (ArrayLit xs)
            return (List ([hd]++tl))
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    stateLookup env var -- crashes if the variable doesn't exist
    e <- evalExpr env expr
    setVar var e
evalExpr env (DotRef exp (Id id)) = do
    l <- evalExpr env (exp)
    case l of
        (List (x:xs)) -> do
            case id of
                "len" -> return (myLength (l) (Int 0))
                "head" -> return x
                "tail" -> return (List xs)
        (List []) -> do
            case id of
                "len" -> return (Int 0)
                "head" -> error $ "Lista vazia"
                "tail" -> error $ "Lista vazia"
        _ -> do 
            error $ "error"
evalExpr env (CallExpr dot lista) = do
    case dot of 
        (DotRef exp (Id id)) -> do
            l <- evalExpr env (exp)
            case l of
                (List l) -> do
                    case id of
                        "concat" -> (myConcat env (l) (lista))
                _ -> do
                    error $ "error"
        _ -> return Nil


myLength :: Value -> Value -> Value
myLength (List []) (Int num) = (Int num)
myLength  (List (l:ls)) (Int num) = myLength  (List ls) (Int (num + 1) )


myConcat :: StateT ->[Value]-> [Expression] -> StateTransformer Value
myConcat env (list) ([]) = return (List list)
myConcat env (list) (x:xs) = do
                vm <- evalExpr env x;
                case vm of
                    (List e) -> myConcat env (list ++ e) (xs)
                    x -> myConcat env (list ++ [x]) xs


--auxEqual :: StateT -> [Value] -> [Expression] -> Value
--auxEqual env [] [] = (Bool True)
--auxEqual env list (x:xs) = do
--            vm <- evalExpr env x
--            myEqual (list) (vm)

--myEqual :: [Value]-> [Value] -> Value
--myEqual  [] [] = (Bool True)
--myEqual  (_) ([]) = (Bool False)
--myEqual  ([]) (_) = (Bool False)
--myEqual  (y:ys) (x:xs) = (x == y) && myEqual (ys) (xs)


evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt                = return Nil
evalStmt env (VarDeclStmt [])         = return Nil
evalStmt env (VarDeclStmt (decl:ds))  =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)

evalStmt env (ExprStmt expr)          = evalExpr env expr
--inicio da edicao

--Bloco de comandos
evalStmt env (BlockStmt [])           = return Nil
--evalStmt env (BlockStmt ((BreakStmt st):stmts)) = return Break
--evalStmt env (BlockStmt [stmt])       = evalStmt env stmt
evalStmt env (BlockStmt (st:stmts))   = do
    evaluedStmt <- evalStmt env st
    case evaluedStmt of
        Break -> return Break
        _ -> evalStmt env (BlockStmt stmts)

evalStmt env (BreakStmt st) = return Break


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
evalStmt env (WhileStmt expr stmt) = do
    x <- evalExpr env expr
    if (evalBoolean x)
        then do
            y <- evalStmt env stmt
            if(isBreak(y))
               then return Nil
               else evalStmt env (WhileStmt expr stmt)
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
                Nothing -> do 
                    evaluedStmt <- evalStmt env stmt
                    if(isBreak evaluedStmt)
                        then return Nil                
                        else evalStmt env (ForStmt NoInit test inc stmt)
                Just incM -> do
                    evaluedStmt <- evalStmt env stmt
                    if(isBreak evaluedStmt)
                        then return Nil
                        else evalExpr env incM >> evalStmt env (ForStmt NoInit test inc stmt)
        Just testM -> do
            evaluedTest <- evalExpr env testM
            if (evalBoolean evaluedTest)
                then case inc of
                    Nothing -> do
                        evaluedStmt <- evalStmt env stmt
                        if(isBreak evaluedStmt)
                            then return Nil
                            else evalStmt env (ForStmt NoInit test inc stmt)
                    Just incM -> do 
                        evaluedStmt <- evalStmt env stmt
                        if(isBreak evaluedStmt)
                            then return Nil
                            else evalExpr env incM >> evalStmt env (ForStmt NoInit test inc stmt)
                else return Nil


evalBoolean :: Value -> Bool
evalBoolean (Bool b) = b

takeId :: Expression -> [Char]
takeId (DotRef a (Id id)) = id

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts



isDotRef :: Expression -> Bool
isDotRef (DotRef (exp) (Id id)) = True
isDotRef _ = False




--compList :: Value -> Value -> Bool
--compList (List []) (List []) = True
--compList (List _) (List []) = False
--compList  (List []) (List _) = False
--compList (List (l:ls)) (List (a:as)) 
--    | (a == l) = compList (ls) (as)
--    | otherwise = False



isBreak :: Value -> Bool
isBreak (Break) = True
isBreak _ = False


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
