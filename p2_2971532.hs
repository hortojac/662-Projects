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
evalMonad (Num x) = if x<0 then Nothing else Just (Num x)
evalMonad (Boolean x) = Just (Boolean x)
evalMonad (Plus x y) = do
  x' <- evalMonad x
  y' <- evalMonad y
  case (x', y') of
    (Num n1, Num n2) -> Just (Num (n1 + n2))
    _ -> Nothing
evalMonad (Minus x y) = do
  x' <- evalMonad x
  y' <- evalMonad y
  case (x', y') of
    (Num n1, Num n2) ->
      if n1 - n2 < 0
        then Nothing
        else Just (Num (n1 - n2))
    _ -> Nothing
evalMonad (Mult x y) = do
  x' <- evalMonad x
  y' <- evalMonad y
  case (x', y') of
    (Num n1, Num n2) -> Just (Num (n1 * n2))
    _ -> Nothing
evalMonad (Div x y) = do
  x' <- evalMonad x
  y' <- evalMonad y
  case (x', y') of
    (Num n1, Num n2) ->
      if n2 == 0 || n1 `div` n2 < 0
        then Nothing
        else Just (Num (n1 `div` n2))
    _ -> Nothing
evalMonad (Exp x y) = do
  x' <- evalMonad x
  y' <- evalMonad y
  case (x', y') of
    (Num n1, Num n2) -> Just (Num (n1 ^ n2))
    _ -> Nothing
evalMonad (And x y) = do
  x' <- evalMonad x
  y' <- evalMonad y
  case (x', y') of
    (Boolean b1, Boolean b2) -> Just (Boolean (b1 && b2))
    _ -> Nothing
evalMonad (Or x y) = do
  x' <- evalMonad x
  y' <- evalMonad y
  case (x', y') of
    (Boolean b1, Boolean b2) -> Just (Boolean (b1 || b2))
    _ -> Nothing
evalMonad (Leq x y) = do
  x' <- evalMonad x
  y' <- evalMonad y
  case (x', y') of
    (Num n1, Num n2) -> Just (Boolean (n1 <= n2))
    _ -> Nothing
evalMonad (IsZero x) = do
  x' <- evalMonad x
  case x' of
    Num n -> Just (Boolean (n == 0))
    _ -> Nothing
evalMonad (If x y z) = do
  x' <- evalMonad x
  case x' of
    Boolean True -> evalMonad y
    Boolean False -> evalMonad z
    _ -> Nothing
evalMonad (Between x y z) = do
  x' <- evalMonad x
  y' <- evalMonad y
  z' <- evalMonad z
  case (x', y', z') of
    (Num n1, Num n2, Num n3) -> return $ Boolean (n1 < n2 && n2 < n3)
    _ -> Nothing

-- Exercise 2
typeofMonad :: KULang -> Maybe KUTypeLang
typeofMonad (Num x) = if x<0 then Nothing else Just TNum
typeofMonad (Boolean x) = Just TBool
typeofMonad (Plus x y) = do
  x' <- typeofMonad x
  y' <- typeofMonad y
  if x' == TNum && y' == TNum
    then Just TNum
    else Nothing
typeofMonad (Minus x y) = do
  x' <- typeofMonad x
  y' <- typeofMonad y
  if x' == TNum && y' == TNum
    then Just TNum
    else Nothing
typeofMonad (Mult x y) = do
  x' <- typeofMonad x
  y' <- typeofMonad y
  if x' == TNum && y' == TNum
    then Just TNum
    else Nothing
typeofMonad (Div x y) = do
  x' <- typeofMonad x
  y' <- typeofMonad y
  if x' == TNum && y' == TNum
    then Just TNum
    else Nothing
typeofMonad (Exp x y) = do
  x' <- typeofMonad x
  y' <- typeofMonad y
  if x' == TNum && y' == TNum
    then Just TNum
    else Nothing
typeofMonad (And x y) = do
  x' <- typeofMonad x
  y' <- typeofMonad y
  if x' == TBool && y' == TBool
    then Just TBool
    else Nothing
typeofMonad (Or x y) = do
  x' <- typeofMonad x
  y' <- typeofMonad y
  if x' == TBool && y' == TBool
    then Just TBool
    else Nothing
typeofMonad (Leq x y) = do
  x' <- typeofMonad x
  y' <- typeofMonad y
  if x' == TNum && y' == TNum
    then Just TBool
    else Nothing
typeofMonad (IsZero x) = do
  x' <- typeofMonad x
  if x' == TNum
    then Just TBool
    else Nothing
typeofMonad (If x y z) = do
  x' <- typeofMonad x
  y' <- typeofMonad y
  z' <- typeofMonad z
  if x' == TBool && y' == z'
    then Just y'
    else Nothing
typeofMonad (Between x y z) = do
  x' <- typeofMonad x
  y' <- typeofMonad y
  z' <- typeofMonad z
  if x' == TNum && y' == TNum && z' == TNum
    then Just TBool
    else Nothing

-- Exercise 3
interpTypeEval :: KULang -> Maybe KULang
interpTypeEval x = do {x' <- typeofMonad x;
                       if (x'==TNum)||(x'==TBool) then evalMonad x else Nothing}

-- Part 2: Optimizer

-- Exercise 1
optimize :: KULang -> KULang
optimize (Num x) = Num x
optimize (Boolean x) = Boolean x
optimize (If (Boolean True) x y) = optimize x
optimize (If (Boolean False) x y) = optimize y
optimize (Plus x (Num 0)) = optimize x
optimize (Plus (Num 0) y) = optimize y
optimize (Plus x y) = Plus (optimize x) (optimize y)
optimize (Minus x y) = Minus (optimize x) (optimize y)
optimize (Mult x (Num 0)) = Num 0
optimize (Mult x (Num 1)) = optimize x
optimize (Mult x y) = Mult (optimize x) (optimize y)
optimize (Div x (Num 1)) = optimize x
optimize (Div x y) = if optimize x==optimize y then Num 1 else Div (optimize x) (optimize y)
optimize (Exp e1 e2) = Exp (optimize e1) (optimize e2)
optimize (And x (Boolean False)) = Boolean False
optimize (And x y) = And (optimize x) (optimize y)
optimize (Or x (Boolean True)) = Boolean True
optimize (Or x y) = Or (optimize x) (optimize y)
optimize (Leq x y) = Leq (optimize x) (optimize y)
optimize (IsZero x) = IsZero (optimize x)
optimize (If x y z) = If (optimize x) (optimize y) (optimize z)
optimize (Between x y z) = Between (optimize x) (optimize y) (optimize z)

-- Exercise 2
interpOptEval :: KULang -> Maybe KULang
interpOptEval x = evalMonad (optimize x)
