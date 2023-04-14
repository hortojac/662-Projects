{-# LANGUAGE GADTs,FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- Abstract Syntax Definitions
data KULang where
    Num :: Int -> KULang  
    Plus :: KULang -> KULang -> KULang 
    Minus :: KULang -> KULang -> KULang
    Mult :: KULang -> KULang -> KULang 
    Div :: KULang -> KULang -> KULang  
    Exp :: KULang -> KULang -> KULang
    If0 :: KULang -> KULang -> KULang -> KULang
    Id :: String -> KULang
    Lambda :: String -> KULang -> KULang 
    App :: KULang -> KULang -> KULang 
    deriving (Show,Eq)

data KULangVal where
    NumV :: Int -> KULangVal
    ClosureV :: String -> KULang -> EnvVal -> KULangVal
    deriving (Show,Eq)

data KULangExt where
    NumX :: Int -> KULangExt
    PlusX :: KULangExt -> KULangExt -> KULangExt
    MinusX :: KULangExt -> KULangExt -> KULangExt
    MultX :: KULangExt -> KULangExt -> KULangExt
    DivX :: KULangExt -> KULangExt -> KULangExt
    ExpX :: KULangExt -> KULangExt -> KULangExt
    If0X :: KULangExt -> KULangExt -> KULangExt -> KULangExt
    LambdaX :: String -> KULangExt -> KULangExt
    AppX :: KULangExt -> KULangExt -> KULangExt 
    BindX :: String -> KULangExt -> KULangExt -> KULangExt
    IdX :: String -> KULangExt
    deriving (Show,Eq)

-- Environment Definitions
type Env = [(String,KULang)]
type EnvVal = [(String,KULangVal)]

-- Reader Definition
data Reader e a = Reader (e -> a)

-- Monad Definition
instance Monad (Reader e) where
 return x = Reader $ \e -> x 
 g >>= f = Reader $ \e -> runR (f (runR g e)) e 

 -- Applicative Definition
instance Applicative (Reader e) where
 pure x = Reader $ \e -> x
(Reader f) <*> (Reader g) = Reader $ \e -> (f e) (g e)

-- Functor Definition
instance Functor (Reader e) where
 fmap f (Reader g) = Reader $ \e -> (f . g) e

-- Fail Definition
instance MonadFail (Reader e) where
        fail = error "fail"

-- Helper Methods
runR :: Reader e a -> e -> a
runR (Reader f) e = f e 

ask :: Reader a a 
ask = Reader $ \e -> e

local :: (e->t) -> Reader t a -> Reader e a 
local f r = ask >>= \e -> return (runR r (f e))

useClosure :: String -> KULangVal -> EnvVal -> EnvVal -> EnvVal
useClosure i v e _ = (i,v):e 


-----------------------------
----- Project Exercises -----
-----------------------------

-- Part 1: Scoping

-- Exercise 1:
evalDyn :: Env -> KULang -> Maybe KULang
evalDyn e (Num x) = if x < 0 then Nothing else Just (Num x)
evalDyn e (Plus x y) = do
    Num x' <- evalDyn e x
    Num y' <- evalDyn e y
    return (Num (x' + y'))
evalDyn e (Minus x y) = do
    Num x' <- evalDyn e x
    Num y' <- evalDyn e y
    if x' - y' < 0 then Nothing else return (Num (x' - y'))
evalDyn e (Mult x y) = do
    Num x' <- evalDyn e x
    Num y' <- evalDyn e y
    return (Num (x' * y'))
evalDyn e (Div x y) = do
    Num x' <- evalDyn e x
    Num y' <- evalDyn e y
    if y' == 0 then Nothing else return (Num (x' `div` y'))
evalDyn e (Exp x y) = do
    Num x' <- evalDyn e x
    Num y' <- evalDyn e y
    return (Num (x' ^ y'))
evalDyn e (If0 x y z) = do
    Num x' <- evalDyn e x
    if x' == 0 then evalDyn e y else evalDyn e z
evalDyn e (Lambda x y) = Just (Lambda x y)
evalDyn e (App x y) = do
    Lambda x' y' <- evalDyn e x
    z <- evalDyn e y
    evalDyn ((x',z):e) y'
evalDyn e (Id x) = lookup x e

-- Exercise 2:
evalStat :: EnvVal -> KULang -> Maybe KULangVal
evalStat e (Num x) = if x < 0 then Nothing else Just (NumV x)
evalStat e (Plus x y) = do
    NumV x' <- evalStat e x
    NumV y' <- evalStat e y
    return (NumV (x' + y'))
evalStat e (Minus x y) = do
    NumV x' <- evalStat e x
    NumV y' <- evalStat e y
    if x' - y' < 0 then Nothing else return (NumV (x' - y'))
evalStat e (Mult x y) = do
    NumV x' <- evalStat e x
    NumV y' <- evalStat e y
    return (NumV (x' * y'))
evalStat e (Div x y) = do
    NumV x' <- evalStat e x
    NumV y' <- evalStat e y
    if y' == 0 then Nothing else return (NumV (x' `div` y'))
evalStat e (Exp x y) = do
    NumV x' <- evalStat e x
    NumV y' <- evalStat e y
    return (NumV (x' ^ y'))
evalStat e (If0 x y z) = do
    NumV x' <- evalStat e x
    if x' == 0 then evalStat e y else evalStat e z
evalStat e (Lambda x y) = Just (ClosureV x y e)
evalStat e (App x y) = do
    ClosureV x' y' e' <- evalStat e x
    z <- evalStat e y
    evalStat (useClosure x' z e' e) y'
evalStat e (Id x) = lookup x e

-- Part 2: Elaboration

-- Exercise 3:
elabTerm :: KULangExt -> KULang
elabTerm (NumX x) = Num x
elabTerm (PlusX x y) = Plus (elabTerm x) (elabTerm y)
elabTerm (MinusX x y) = Minus (elabTerm x) (elabTerm y)
elabTerm (MultX x y) = Mult (elabTerm x) (elabTerm y)
elabTerm (DivX x y) = Div (elabTerm x) (elabTerm y)
elabTerm (ExpX x y) = Exp (elabTerm x) (elabTerm y)
elabTerm (If0X x y z) = If0 (elabTerm x) (elabTerm y) (elabTerm z)
elabTerm (LambdaX x y) = Lambda x (elabTerm y)
elabTerm (AppX x y) = App (elabTerm x) (elabTerm y)
elabTerm (BindX x y z) = App (Lambda x (elabTerm z)) (elabTerm y)
elabTerm (IdX x) = Id x

-- Exercise 4:
interpElab :: EnvVal -> KULangExt -> Maybe KULangVal
interpElab e x = evalStat e (elabTerm x)

-- Part 3: Reader Monad

-- Exercise 5:
evalReader :: KULang -> Reader EnvVal KULangVal
evalReader (Num x) = return (NumV x)
evalReader (Plus x y) = do
    NumV x' <- evalReader x
    NumV y' <- evalReader y
    return (NumV (x' + y'))
evalReader (Minus x y) = do
    NumV x' <- evalReader x
    NumV y' <- evalReader y
    return (NumV (x' - y'))
evalReader (Mult x y) = do
    NumV x' <- evalReader x
    NumV y' <- evalReader y
    return (NumV (x' * y'))
evalReader (Div x y) = do
    NumV x' <- evalReader x
    NumV y' <- evalReader y
    return (NumV (x' `div` y'))
evalReader (Exp x y) = do
    NumV x' <- evalReader x
    NumV y' <- evalReader y
    return (NumV (x' ^ y'))
evalReader (If0 x y z) = do
    NumV x' <- evalReader x
    if x' == 0 then evalReader y else evalReader z
evalReader (Lambda x y) = do
    e <- ask
    return (ClosureV x y e)
evalReader (App x y) = do
    ClosureV x' y' e' <- evalReader x
    z <- evalReader y
    local (useClosure x' z e') (evalReader y')
evalReader (Id x) = do
    e <- ask
    case lookup x e of
        Just v -> return v
        Nothing -> error "Unbound variable"

-- Exercise 6:
interpReader :: KULangExt -> KULangVal
interpReader x = runR (evalReader (elabTerm x)) []