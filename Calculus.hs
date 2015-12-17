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

diff (BinApp Div x y) xs
 = BinApp Div (BinApp Add (BinApp Mul dx y) (UnApp Neg (BinApp Mul x dy))) y2
 where
  dx = diff x xs
  dy = diff y xs
  y2 = BinApp Mul y y

diff (UnApp Neg x) xs
 = UnApp Neg (diff x xs)
diff (UnApp Sin x) xs
 = BinApp Mul (UnApp Cos x) (diff x xs)
diff (UnApp Cos x) xs
 = UnApp Neg (BinApp Mul (UnApp Sin x) (diff x xs))
diff (UnApp Log x) xs
 = BinApp Div (diff x xs) x

facts :: Int -> [Int]
-- Computes a list of k!, 1 <= k <= n
facts n
 = scanl (*) 1  [1..n]

maclaurin :: Exp -> Double -> Int -> Double
-- Computes MacLaurin expansion
maclaurin ex x n
 = sum ((zipWith3 maclaurin' t) (facts n) (map (x^) [0..n]))
 where
  maclaurin' :: Exp -> Int -> Double -> Double
  maclaurin' e m p
   = ((eval e [("x",0)]) / (fromIntegral m) ) * p
  t = take n (iterate (`diff` "x") ex)

maclaurin2 :: Exp -> Double -> Int -> Double
-- Computes MacLaurin expansion
maclaurin2 ex x n
 = sum ((zipWith3 maclaurin' t) (facts n) (map (x^) [0..n]) )
 where
  maclaurin' :: Exp -> Int -> Double -> Double
  maclaurin' e m p
   = ((eval e [("x",0)]) / (fromIntegral m) ) * p
  t = take n (iterate (`diff3` "x") ex)

showExp :: Exp -> String
-- Prints an expression in a mathematical form
showExp (Val x)
 = show x

showExp (Id x)
 = x
showExp (BinApp Add x y)
 = concat ["(",showExp x,"+",showExp y,")"]
showExp (BinApp Mul x y)
 = concat ["(",showExp x,"*",showExp y,")"]
showExp (BinApp Div x y)
 = concat ["(",showExp x,"/",showExp y,")"]

showExp (UnApp Neg x)
 = concat ["-(", showExp x,")"]
showExp (UnApp Sin x)
 = concat ["sin(", showExp x,")"]
showExp (UnApp Cos x)
 = concat ["cos(", showExp x,")"]
showExp (UnApp Log x)
 = concat ["log(", showExp x,")"]

diff3 :: Exp -> String -> Exp
-- Converts diff2 to return an expression
diff3 ex xs
 | diff2 ex xs == Nothing = Val 0
 | otherwise = fromJust (diff2 ex xs)

diff2 :: Exp -> String -> Maybe Exp
-- Alternative of diff according to the second expansion
diff2  (Val x) xs
 = Nothing
diff2 (Id x) xs
 | x == xs = Just 1.0
 | otherwise = Nothing

diff2 (BinApp Add x y) xs
 =  (diff2 x xs) + (diff2 y xs)

diff2 (BinApp Mul x y) xs
 = ( (Just x) * (diff2 y xs)) + ((diff2 x xs) * (Just y))

diff2 (BinApp Div x y) xs
 =( ( (diff2 x xs) * (Just y)) + (negate ((Just x) * (diff2 y xs)) ) ) /
                                                           ((Just y) * (Just y))

diff2 (UnApp Neg x) xs
 =  negate (diff2 x xs)
diff2 (UnApp Sin x) xs
 = (cos (Just x)) * (diff2 x xs)
diff2 (UnApp Cos x) xs
 = negate ( sin (Just x) * (diff2 x xs))
diff2 (UnApp Log x) xs
 = (diff2 x xs) / (Just x)

---------------------------------------------------------------------------
-- Test cases from the spec.

e1, e2, e3, e4, e5, e6 :: Exp

-- > 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- > x*x + y - 7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- > x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- > -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- > sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- > log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))

-- > sin x + 5 * log x
e7 = BinApp Add (UnApp Sin (Id "x")) (BinApp Mul (Val 5.0) (UnApp Log (Id "x")))
-- > x*x*x
e8 = BinApp Mul (BinApp Mul (Id "x") (Id "x")) (Id "x")

-- > log (x*x)
e9 = UnApp Log (BinApp Mul (Id "x") (Id "x"))

-- > sin (2*x+1)
e10 = UnApp Sin (BinApp Add (BinApp Mul (Val 2.0) (Id "x")) (Val 1.0))

----------------------------------------------------------------------
instance (Eq a, Num a) => Num (Maybe a) where

 Nothing * x = Nothing
 Just 1 * x = x
 x * Nothing = Nothing
 x * Just 1 = x
 Just x * Just y = Just (x*y)

 Nothing + Just x = Just x
 Just x + Nothing = Just x
 Just x + Just y = Just (x+y)

 negate (Just x) = Just (negate x)
 negate Nothing = Nothing

instance (Eq a, Fractional a) =>  Fractional (Maybe a) where
 Nothing / (Just x) = Nothing
 (Just x)/ 1 = Just x
 (Just x) / (Just y) = Just (x/y)

instance (Eq a, Floating  a) => Floating (Maybe a) where
 sin Nothing = Nothing
 sin (Just x) = Just (sin x)
 cos (Just x) = Just (cos x)
 cos Nothing = Just 1
 log (Just 1) = Nothing
 log (Just x) = Just (log x)

--The following makes it much easier to input expressions, e.g. sin x, log(x*x)

x, y :: Exp
x = Id "x"
y = Id "y"
                                                                           
