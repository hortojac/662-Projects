{-# LANGUAGE GADTs,FlexibleContexts #-}

-- AST and Type Definitions

data KUTypeLang where
    TNum :: KUTypeLang
    TBool :: KUTypeLang
    deriving (Show,Eq)

data KULang where
    Num :: Int -> KULang
    Plus :: KULang -> KULang -> KULang
    Minus :: KULang -> KULang -> KULang
    Mult :: KULang -> KULang -> KULang
    Div :: KULang -> KULang -> KULang
    Exp :: KULang -> KULang -> KULang
    Boolean :: Bool -> KULang
    And :: KULang -> KULang -> KULang
    Or :: KULang -> KULang -> KULang
    Leq :: KULang -> KULang -> KULang
    IsZero :: KULang -> KULang
    If :: KULang -> KULang -> KULang -> KULang
    Between :: KULang -> KULang -> KULang -> KULang
    Bind :: String -> KULang -> KULang -> KULang
    Id :: String -> KULang
    deriving (Show,Eq)


type Env = [(String,KULang)]

type Cont = [(String,KUTypeLang)]


-------------------------------
------ Project Exercises ------
-------------------------------
-- Part 1: Adding Booleans

-- Exercise 1
evalDirect :: KULang -> Maybe KULang
evalDirect (Num x) = if x<0 then Nothing else Just (Num x)
evalDirect (Plus x y) = do
    (Num x') <- evalDirect x
    (Num y') <- evalDirect y
    return (Num (x'+y'))
evalDirect (Minus x y) = do
    (Num x') <- evalDirect x
    (Num y') <- evalDirect y
    if x'-y'<0 then Nothing else return (Num (x'-y'))
evalDirect (Mult x y) = do
    (Num x') <- evalDirect x
    (Num y') <- evalDirect y
    return (Num (x'*y'))
evalDirect (Div x y) = do
    (Num x') <- evalDirect x
    (Num y') <- evalDirect y
    if y'==0 then Nothing else return (Num (x'`div`y'))
evalDirect (Exp x y) = do
    (Num x') <- evalDirect x
    (Num y') <- evalDirect y
    if y'<0 then Nothing else return (Num (x'^y'))
evalDirect (Boolean x) = Just (Boolean x)
evalDirect (And x y) = do
    (Boolean x') <- evalDirect x
    (Boolean y') <- evalDirect y
    return (Boolean (x'&&y'))
evalDirect (Or x y) = do
    (Boolean x') <- evalDirect x
    (Boolean y') <- evalDirect y
    return (Boolean (x'||y'))
evalDirect (Leq x y) = do
    (Num x') <- evalDirect x
    (Num y') <- evalDirect y
    return (Boolean (x'<=y'))
evalDirect (IsZero x) = do
    (Num x') <- evalDirect x
    if x'==0 then return (Boolean True) else return (Boolean False)
evalDirect (If x y z) = do
    (Boolean x') <- evalDirect x
    if x' then return y else return z
evalDirect (Between x y z) = do
    (Num x') <- evalDirect x
    (Num y') <- evalDirect y
    (Num z') <- evalDirect z
    if x'<=y' && y'<=z' then return (Boolean True) else return (Boolean False)
evalDirect (Bind x y z) = do
    y' <- evalDirect y
    return (Bind x y' z)
evalDirect (Id x) = Nothing

-- Exercise 2
evalDeferred :: Env -> KULang -> Maybe KULang
evalDeferred _ (Num x) = if x<0 then Nothing else Just (Num x)
evalDeferred env (Plus x y) = do
    (Num x') <- evalDeferred env x
    (Num y') <- evalDeferred env y
    return (Num (x'+y'))
evalDeferred env (Minus x y) = do
    (Num x') <- evalDeferred env x
    (Num y') <- evalDeferred env y
    if x'-y'<0 then Nothing else return (Num (x'-y'))
evalDeferred env (Mult x y) = do
    (Num x') <- evalDeferred env x
    (Num y') <- evalDeferred env y
    return (Num (x'*y'))
evalDeferred env (Div x y) = do
    (Num x') <- evalDeferred env x
    (Num y') <- evalDeferred env y
    if y'==0 then Nothing else return (Num (x'`div`y'))
evalDeferred env (Exp x y) = do
    (Num x') <- evalDeferred env x
    (Num y') <- evalDeferred env y
    if y'<0 then Nothing else return (Num (x'^y'))
evalDeferred _ (Boolean x) = Just (Boolean x)
evalDeferred env (And x y) = do
    (Boolean x') <- evalDeferred env x
    (Boolean y') <- evalDeferred env y
    return (Boolean (x'&&y'))
evalDeferred env (Or x y) = do
    (Boolean x') <- evalDeferred env x
    (Boolean y') <- evalDeferred env y
    return (Boolean (x'||y'))
evalDeferred env (Leq x y) = do
    (Num x') <- evalDeferred env x
    (Num y') <- evalDeferred env y
    return (Boolean (x'<=y'))
evalDeferred env (IsZero x) = do
    (Num x') <- evalDeferred env x
    if x'==0 then return (Boolean True) else return (Boolean False)
evalDeferred env (If x y z) = do
    (Boolean x') <- evalDeferred env x
    if x' then return y else return z
evalDeferred env (Between x y z) = do
    (Num x') <- evalDeferred env x
    (Num y') <- evalDeferred env y
    (Num z') <- evalDeferred env z
    if x'<=y' && y'<=z' then return (Boolean True) else return (Boolean False)
evalDeferred env (Bind x y z) = do
    y' <- evalDeferred env y
    return (Bind x y' z)
evalDeferred env (Id x) = lookup x env

-- Exercise 3
testEvals :: KULang -> Bool
testEvals x = evalDirect x == evalDeferred [] x

-- Part 2: Type Checking

--Exercise 1
typeofMonad :: Cont -> KULang -> Maybe KUTypeLang
typeofMonad _ (Num x) = if x<0 then Nothing else return TNum
typeofMonad cont (Plus x y) = do
    TNum <- typeofMonad cont x
    TNum <- typeofMonad cont y
    return TNum
typeofMonad cont (Minus x y) = do
    TNum <- typeofMonad cont x
    TNum <- typeofMonad cont y
    return TNum
typeofMonad cont (Mult x y) = do
    TNum <- typeofMonad cont x
    TNum <- typeofMonad cont y
    return TNum
typeofMonad cont (Div x y) = do
    TNum <- typeofMonad cont x
    TNum <- typeofMonad cont y
    return TNum
typeofMonad cont (Exp x y) = do
    TNum <- typeofMonad cont x
    TNum <- typeofMonad cont y
    return TNum
typeofMonad _ (Boolean x) = return TBool
typeofMonad cont (And x y) = do
    TBool <- typeofMonad cont x
    TBool <- typeofMonad cont y
    return TBool
typeofMonad cont (Or x y) = do
    TBool <- typeofMonad cont x
    TBool <- typeofMonad cont y
    return TBool
typeofMonad cont (Leq x y) = do
    TNum <- typeofMonad cont x
    TNum <- typeofMonad cont y
    return TBool
typeofMonad cont (IsZero x) = do
    TNum <- typeofMonad cont x
    return TBool
typeofMonad cont (If x y z) = do
    TBool <- typeofMonad cont x
    y' <- typeofMonad cont y
    z' <- typeofMonad cont z
    if y'==z' then return y' else Nothing
typeofMonad cont (Between x y z) = do
    TNum <- typeofMonad cont x
    TNum <- typeofMonad cont y
    TNum <- typeofMonad cont z
    return TBool
typeofMonad cont (Bind x y z) = do
    y' <- typeofMonad cont y
    typeofMonad ((x,y'):cont) z
typeofMonad cont (Id x) = lookup x cont

--Exercise 2
interpret :: KULang -> Maybe KULang
interpret x = do
    _ <- typeofMonad [] x
    evalDeferred [] x


