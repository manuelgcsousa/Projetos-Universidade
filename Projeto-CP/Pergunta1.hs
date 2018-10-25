{-# OPTIONS_GHC -XNPlusKPatterns #-}

import Cp 
import Nat
import Test.QuickCheck hiding ((><))



arredonda :: Float -> Int -> Float
arredonda x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n

numbersbetween :: Gen Float
numbersbetween = choose (1,2)

testInv :: Gen Bool
testInv = do 
    d1 <- numbersbetween
    return ((arredonda (invFinalissimo d1 10000) 3)  == (arredonda (1/d1) 3))


--Resolução do problema nº1:
inv x 0 =0
inv x (n+1) = (1-x)^(n+1) + inv x (n)
--
--inv'' x = inv' x 10000
--
--inv' x n = let (a,b) = aux x (n) in a   
--    where
--        aux x 0 = (1,(1-x))
--        aux x (n+1) = let (a,b) = aux x (n)
--                           in (a+b,(1-x)*b)
--
mul1 :: (Num a) => (a,a) -> a
mul1 (x,y) = x * y

add1 :: (Num a) => (a, a) -> a
add1 (x,y) = x+y 

final x 0 = 1 
final x (n+1) = auxiliar x n + final x n

auxiliar x 0 = 1 - x
auxiliar x (n+1) = (1-x) * auxiliar x n 

--invFinal :: Fractional a => a  -> Integer -> (a, Integer)
invFinal x = for (split  (mul1.(id >< const (1-x))) add1)  (1-x,1)
invFinalissimo x = p2.(invFinal x)



