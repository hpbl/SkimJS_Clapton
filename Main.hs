import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad hiding (empty)
import Control.Applicative hiding (empty)
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value
import Data.Bits

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
            vm <- evalExpr env x
            (List tl) <- evalExpr env (ArrayLit xs)
            return (List ([vm]++tl))


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
                "len" -> (myLength (x:xs) (Int 0))
                "head" -> return x
                "tail" -> return (List xs)
        (List []) -> do
            case id of
                "len" -> return (Int 0)
                "head" -> error $ "Lista vazia"
                "tail" -> error $ "Lista vazia"
        _ -> do 
            error $ "error"
evalExpr env (CallExpr (DotRef exp (Id id)) lista) = do
    l <- evalExpr env (exp)
    case l of
        (List (x:xs) ) -> do
            case id of
                "concat" -> (myConcat env (x:xs) (lista))
                "equals" -> (auxEqual env (x:xs) lista)
                "len" -> (myLength (x:xs) (Int 0))
                "head" -> return (x)
                "tail" -> return (List xs)
        _ -> do
            error $ "error"


evalExpr env (CallExpr name params) = do
    f <- evalExpr env name
    case f of
        Function _ args stmts -> do
            pushScope env
            argsF env args params
            v <- evalStmt env (BlockStmt stmts)
            popScope env
            case v of
                Return r -> return r
                NilReturn -> return Nil
        _ -> error $ "error in CallExpr"

-- funcao que acessa uma posicao do array
evalExpr env (BracketRef exp1 exp2) = do 
    list <- evalExpr env exp1
    (Int idx) <- evalExpr env exp2
    case list of
        (List l) -> return (l!!idx) -- operador que acessa a posicao n
        _ -> error $ "erro"

-- funcao que troca o valor do array
evalExpr env (AssignExpr op (LBracket lista idx) expr) = do
    l <- evalExpr env lista
    idxNew <- evalExpr env idx
    exprNew <- evalExpr env expr
    case l of
        (List list) -> do
            case op of
                OpAssign -> do
                    pos <- auxList list idxNew exprNew
                    case lista of
                        VarRef (Id var) -> setVar var pos
                        _ -> return Nil
-- funca que auxilia a troca de um valor no array
auxList :: [Value] -> Value -> Value -> StateTransformer Value
auxList (x:xs) (Int idx) val = do
    if(idx == 0) then return (List (val:xs))
        else do
            (List list) <- auxList (xs) (Int (idx -1)) (val)
            return (List (x:list))

argsF :: StateT -> [Id] -> [Expression] ->StateTransformer Value
argsF env [] [] = return Nil
argsF env ((Id x):xs) (y:ys) = do
    v <- evalExpr env y
    localVar x v
    argsF env xs ys
argsF env _ _ = error $ "error"            


myLength :: [Value] -> Value -> StateTransformer Value
myLength ([]) (Int num) = return (Int num)
myLength  ((l:ls)) (Int num) = myLength  (ls) (Int (num + 1))


myConcat :: StateT ->[Value]-> [Expression] -> StateTransformer Value
myConcat env (list) ([]) = return (List list)
myConcat env (list) (x:xs) = do
                vm <- evalExpr env x;
                case vm of
                    (List e) -> myConcat env (list ++ e) (xs)
                    x -> myConcat env (list ++ [x]) xs

auxEqual :: StateT -> [Value] -> [Expression] -> StateTransformer Value
auxEqual env (list) ([]) = return (Bool True)
auxEqual env (list) (x:xs) = do
                vm <- evalExpr env x
                case vm of
                    (List e) -> return (Bool (myEqual (list) (e)))


myEqual :: [Value]-> [Value] -> Bool
myEqual  [] [] = (True)
myEqual  (_) ([]) = (False)
myEqual  ([]) (_) = (False)
myEqual  (y:ys) (x:xs) = (x == y) && myEqual(ys) (xs)


evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt                = return Nil
evalStmt env (VarDeclStmt [])         = return Nil
evalStmt env (VarDeclStmt (decl:ds))  =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)

evalStmt env (ExprStmt expr)          = evalExpr env expr
--inicio da edicao

--Bloco de comandos
evalStmt env (BlockStmt [])           = return Nil
evalStmt env (BlockStmt (st:stmts))   = do
    evaluedStmt <- evalStmt env st
    case evaluedStmt of
        Break -> return Break
        Return r -> return (Return r)
        NilReturn -> return NilReturn
        _ -> evalStmt env (BlockStmt stmts)


evalStmt env (BreakStmt m) = return Break
--If simples
evalStmt env (IfSingleStmt expr stmt) = do
    x <- evalExpr env expr
    if (evalBoolean x)
        then
            evalStmt env stmt
        else return Nil


--If com else
evalStmt env (IfStmt expr stmt stmt2) = do
    x <- evalExpr env expr
    if (evalBoolean x)
        then
            evalStmt env stmt  
        else
            evalStmt env stmt2
        


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
                        then do 
                            --popScope env
                            return Nil                
                        else evalStmt env (ForStmt NoInit test inc stmt)
                Just incM -> do
                    evaluedStmt <- evalStmt env stmt
                    if(isBreak evaluedStmt)
                        then do
                            return Nil
                        else evalExpr env incM >> evalStmt env (ForStmt NoInit test inc stmt)
        Just testM -> do
            evaluedTest <- evalExpr env testM
            if (evalBoolean evaluedTest)
                then case inc of
                    Nothing -> do
                        evaluedStmt <- evalStmt env stmt
                        if(isBreak evaluedStmt)
                            then do
                                return Nil
                            else evalStmt env (ForStmt NoInit test inc stmt)
                    Just incM -> do
                        evaluedStmt <- evalStmt env stmt
                        if(isBreak evaluedStmt)
                            then do
                                return Nil
                            else evalExpr env incM >> evalStmt env (ForStmt NoInit test inc stmt)
                else return Nil






evalStmt env (FunctionStmt (Id name) args stmts) = globalVar name (Function (Id name) args stmts)

evalStmt env (ReturnStmt mexpr) =
    case mexpr of
        Nothing -> return NilReturn
        Just val -> do
            v <- evalExpr env val
            return (Return v)


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

environment :: [Map String Value]
environment = [empty]


localVar :: String -> Value -> StateTransformer Value
localVar var val = ST $ \s -> (val, (insert var val (head s)):(tail s))

globalVar :: String -> Value -> StateTransformer Value
globalVar var val = ST $ \s -> (val, globalVar2 var val s)

globalVar2 :: String -> Value -> StateT -> StateT
globalVar2 var val state = 
    if null (tail state) 
        then (insert var val (head state)):[]
        else (head state): (globalVar2 var val (tail state))





stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    case scopeLookup s var of
        Nothing -> (Global, s)
        Just val -> (val, s)


scopeLookup :: [Map String Value]->String -> Maybe Value
scopeLookup [] _ = Nothing
scopeLookup (s:scopes) var = 
    case Map.lookup var s of
        Nothing -> scopeLookup scopes var
        Just val -> Just val


varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> localVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            localVar id val


varDeclList :: StateT -> [VarDecl] -> StateTransformer Value
varDeclList env [] = return Nil
varDeclList env vdl = foldl1 (>>) $ map (varDecl env) vdl

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, updateVar var val s)

updateVar :: String -> Value -> StateT -> StateT 
updateVar _ _ [] = error $ "uu"
updateVar var val (st:stmt) = case (Map.lookup var st) of
        Nothing -> st:(updateVar var val stmt)
        Just v -> (insert var val st):stmt


pushScope :: StateT -> StateTransformer Value
pushScope env = ST $ \s -> (Nil, empty:s)

popScope::StateT->StateTransformer Value
popScope env = ST $ \s -> (Nil, (tail s))
--
-- Types and boilerplate
--

type StateT = [Map String Value]
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
    show val ++ "\n" ++ show (toList $ union (head defs) (head environment)) ++ "\n" ++ showResult (val, tail(defs))

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f [empty]

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
