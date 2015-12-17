module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

instance Num Exp where
 (+) = BinApp Add
 (*) = BinApp Mul
 negate = UnApp Neg
 fromInteger x = Val (fromInteger x)

instance Fractional Exp where
 (/) = BinApp Div
 fromRational x = Val (fromRational x)

instance Floating Exp where
 sin = UnApp Sin
 cos = UnApp Cos
 log = UnApp Log

lookUp :: Eq a => a -> [(a, b)] -> b
-- Searches a given element among the first elements in a list of doubles
-- Returns: the second element
lookUp x xs
 = head [b | (a,b) <- xs, a == x]

-- Lists of doubles: definitions and operators
opsbin = [(Add, (+)), (Mul, (*)), (Div, (/))]
opsun  = [(Neg, negate), (Sin, sin), (Cos, cos), (Log, log)]

eval2 :: Exp -> Env -> Double
-- Evaluates an expression, searching operators in a list
eval2 (Val x) xs
 = x
eval2 (Id x) xs
 = lookUp x xs
eval2 (BinApp op x y) xs
 = (lookUp op opsbin) (eval2 x xs) (eval2 y xs)
eval2 (UnApp op x) xs
 = (lookUp op opsun) (eval2 x xs)

eval :: Exp -> Env -> Double
-- Evaluates an expression
eval (Val x) xs
 = x
eval (Id x) xs
 = lookUp x xs

eval (UnApp Neg x) xs
 = (-1) *( eval x xs)
eval (UnApp Sin x) xs
 = sin (eval x xs)
eval (UnApp Cos x) xs
 = cos (eval x xs)
eval (UnApp Log x) xs
 = log (eval x xs)

eval (BinApp Add x y) xs
 = (eval x xs) + (eval y xs)
eval (BinApp Mul x y) xs
 = (eval x xs) * (eval y xs)
eval (BinApp Div x y) xs
 = (eval x xs) / (eval y xs)


diff :: Exp -> String -> Exp
-- Differentiates an expression
diff  (Val x) xs
 = Val 0.0
diff (Id x) xs
 | x == xs = Val 1.0
 | otherwise = Val 0.0

diff (BinApp Add x y) xs
 = BinApp Add (diff x xs) (diff y xs)

diff (BinApp Mul x y) xs
 = BinApp Add (BinApp Mul x (diff y xs)) (BinApp Mul (diff x xs) y)
