{-# LANGUAGE GADTs, FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}


-- AST Definition
data KUTypeLang where
    TNum :: KUTypeLang
    TBool :: KUTypeLang
    deriving (Show,Eq)

data KULang where
    Num :: Int -> KULang
    Boolean :: Bool -> KULang
    Plus :: KULang -> KULang -> KULang
    Minus :: KULang -> KULang -> KULang
    Mult :: KULang -> KULang -> KULang
    Div :: KULang -> KULang -> KULang
    Exp :: KULang -> KULang -> KULang
    And :: KULang -> KULang -> KULang
    Or :: KULang -> KULang -> KULang
    Leq :: KULang -> KULang -> KULang
    IsZero :: KULang -> KULang
    If :: KULang -> KULang -> KULang -> KULang
    Between :: KULang -> KULang -> KULang -> KULang
    deriving (Show,Eq)

-------------------------------
------ Project Exercises ------
-------------------------------
-- Part 1: Type Inference

-- Exercise 1
evalMonad :: KULang -> Maybe KULang
evalMonad (Num n) = Just (Num n)
evalMonad (Boolean b) = Just (Boolean b)
evalMonad (Plus e1 e2) = do
  v1 <- evalMonad e1
  v2 <- evalMonad e2
  case (v1, v2) of
    (Num n1, Num n2) -> Just (Num (n1 + n2))
    _ -> Nothing
evalMonad (Minus e1 e2) = do
  v1 <- evalMonad e1
  v2 <- evalMonad e2
  case (v1, v2) of
    (Num n1, Num n2) ->
      if n1 - n2 < 0
        then Nothing
        else Just (Num (n1 - n2))
    _ -> Nothing
evalMonad (Mult e1 e2) = do
  v1 <- evalMonad e1
  v2 <- evalMonad e2
  case (v1, v2) of
    (Num n1, Num n2) -> Just (Num (n1 * n2))
    _ -> Nothing
evalMonad (Div e1 e2) = do
  v1 <- evalMonad e1
  v2 <- evalMonad e2
  case (v1, v2) of
    (Num n1, Num n2) ->
      if n2 == 0 || (n1 `div` n2 < 0)
        then Nothing
        else Just (Num (n1 `div` n2))
    _ -> Nothing
evalMonad (Exp e1 e2) = do
  v1 <- evalMonad e1
  v2 <- evalMonad e2
  case (v1, v2) of
    (Num n1, Num n2) -> Just (Num (n1 ^ n2))
    _ -> Nothing
evalMonad (And e1 e2) = do
  v1 <- evalMonad e1
  v2 <- evalMonad e2
  case (v1, v2) of
    (Boolean b1, Boolean b2) -> Just (Boolean (b1 && b2))
    _ -> Nothing
evalMonad (Or e1 e2) = do
  v1 <- evalMonad e1
  v2 <- evalMonad e2
  case (v1, v2) of
    (Boolean b1, Boolean b2) -> Just (Boolean (b1 || b2))
    _ -> Nothing
evalMonad (Leq e1 e2) = do
  v1 <- evalMonad e1
  v2 <- evalMonad e2
  case (v1, v2) of
    (Num n1, Num n2) -> Just (Boolean (n1 <= n2))
    _ -> Nothing
evalMonad (IsZero e) = do
  v <- evalMonad e
  case v of
    Num n -> Just (Boolean (n == 0))
    _ -> Nothing
evalMonad (If e1 e2 e3) = do
  v1 <- evalMonad e1
  case v1 of
    Boolean True -> evalMonad e2
    Boolean False -> evalMonad e3
    _ -> Nothing
evalMonad (Between e1 e2 e3) = do
  v1 <- evalMonad e1
  v2 <- evalMonad e2
  v3 <- evalMonad e3
  case (v1, v2, v3) of
    (Num n1, Num n2, Num n3) -> return $ Boolean (n1 < n2 && n2 < n3)
    _ -> Nothing

-- Exercise 2
typeofMonad :: KULang -> Maybe KUTypeLang
typeofMonad (Num _) = Just TNum
typeofMonad (Boolean _) = Just TBool
typeofMonad (Plus e1 e2) = do
  t1 <- typeofMonad e1
  t2 <- typeofMonad e2
  if t1 == TNum && t2 == TNum
    then Just TNum
    else Nothing
typeofMonad (Minus e1 e2) = do
  t1 <- typeofMonad e1
  t2 <- typeofMonad e2
  if t1 == TNum && t2 == TNum
    then Just TNum
    else Nothing
typeofMonad (Mult e1 e2) = do
  t1 <- typeofMonad e1
  t2 <- typeofMonad e2
  if t1 == TNum && t2 == TNum
    then Just TNum
    else Nothing
typeofMonad (Div e1 e2) = do
  t1 <- typeofMonad e1
  t2 <- typeofMonad e2
  if t1 == TNum && t2 == TNum && evalMonad e2 /= Just (Num 0)
    then Just TNum
    else Nothing
typeofMonad (Exp e1 e2) = do
  t1 <- typeofMonad e1
  t2 <- typeofMonad e2
  if t1 == TNum && t2 == TNum
    then Just TNum
    else Nothing
typeofMonad (And e1 e2) = do
  t1 <- typeofMonad e1
  t2 <- typeofMonad e2
  if t1 == TBool && t2 == TBool
    then Just TBool
    else Nothing
typeofMonad (Or e1 e2) = do
  t1 <- typeofMonad e1
  t2 <- typeofMonad e2
  if t1 == TBool && t2 == TBool
    then Just TBool
    else Nothing
typeofMonad (Leq e1 e2) = do
  t1 <- typeofMonad e1
  t2 <- typeofMonad e2
  if t1 == TNum && t2 == TNum
    then Just TBool
    else Nothing
typeofMonad (IsZero e) = do
  t <- typeofMonad e
  if t == TNum
    then Just TBool
    else Nothing
typeofMonad (If e1 e2 e3) = do
  t1 <- typeofMonad e1
  t2 <- typeofMonad e2
  t3 <- typeofMonad e3
  if t1 == TBool && t2 == t3
    then Just t2
    else Nothing
typeofMonad (Between e1 e2 e3) = do
  t1 <- typeofMonad e1
  t2 <- typeofMonad e2
  t3 <- typeofMonad e3
  if t1 == TNum && t2 == TNum && t3 == TNum
    then Just TBool
    else Nothing

-- Exercise 3
interpTypeEval :: KULang -> Maybe KULang
interpTypeEval expr = do
  t <- typeofMonad expr
  case t of
    TNum -> do
      v <- evalMonad expr
      case v of
        Num n -> return (Num n)
        _ -> Nothing
    TBool -> do
      v <- evalMonad expr
      case v of
        Boolean b -> return (Boolean b)
        _ -> Nothing

-- Part 2: Optimizer

-- Exercise 1
optimize :: KULang -> KULang
optimize (Plus e (Num 0)) = optimize e
optimize (Plus (Num 0) e) = optimize e
optimize (If (Boolean True) e _) = optimize e
optimize (If (Boolean False) _ e) = optimize e
optimize (Plus e1 e2) = Plus (optimize e1) (optimize e2)
optimize (Minus e1 e2) = Minus (optimize e1) (optimize e2)
optimize (Mult e1 e2) = Mult (optimize e1) (optimize e2)
optimize (Div e1 e2) = Div (optimize e1) (optimize e2)
optimize (Exp e1 e2) = Exp (optimize e1) (optimize e2)
optimize (And e1 e2) = And (optimize e1) (optimize e2)
optimize (Or e1 e2) = Or (optimize e1) (optimize e2)
optimize (Leq e1 e2) = Leq (optimize e1) (optimize e2)
optimize (IsZero e) = IsZero (optimize e)
optimize (If e1 e2 e3) = If (optimize e1) (optimize e2) (optimize e3)
optimize (Between e1 e2 e3) = Between (optimize e1) (optimize e2) (optimize e3)
optimize e = e

-- Exercise 2
interpOptEval :: KULang -> Maybe KULang
interpOptEval expr =
  let optimizedExpr = optimize expr
   in evalMonad optimizedExpr

