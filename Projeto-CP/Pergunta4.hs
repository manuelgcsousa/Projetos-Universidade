import Cp
import List 
import Nat  
import Exp
import BTree
import LTree
import St 
import Probability hiding (cond)
import Data.List
import Test.QuickCheck hiding ((><))
import System.Random  hiding (split)
import GHC.IO.Exception
import System.IO.Unsafe


type Null   = ()
type Prod a b = (a,b)

type Algae = A
data A = NA | A A B deriving Show
data B = NB | B A deriving Show

inA :: Either Null (Prod A B) -> A
inA = either (const NA)(uncurry A)

outA :: A -> Either Null (Prod A B)
outA NA = Left ()
outA (A a b) = Right (a,b)

inB :: Either Null A -> B
inB = either (const NB) B

outB :: B -> Either Null A
outB NB = Left ()
outB (B a) = Right a

cataA :: (Either Null (Prod c d) -> c) -> (Either Null c -> d) -> A -> c
cataA ga gb = ga . (id -|- cataA ga gb >< cataB ga gb) . outA

cataB :: (Either Null (Prod c d) -> c) -> (Either Null c -> d) -> B -> d
cataB ga gb = gb . (id -|- cataA ga gb) . outB

anaA ga gb = inA . (id -|- anaA ga gb >< anaB ga gb) . ga

anaB ga gb = inB . (id -|- anaA ga gb) . gb

showMyB :: B -> [Char]
showMyB (NB) = []
showMyB (B x) = "B " ++ show x

showAlgae :: A -> [Char] 
showAlgae (NA) = []
showAlgae (A x y) = "A " ++ showAlgae x ++ showMyB y 

g (Left ()) = []
g (Right (x,y)) = "A " ++ x ++ y

g2 (Left ()) = []
g2 (Right (x)) = "B " ++ x

showAlgae2 :: Algae -> String
showAlgae2 = cataA g g2

--divisao :: (Num a) => Int -> a
--divisao x = x/2



myfib :: Int -> Int
myfib 0 = 1
myfib 1 = 1
myfib n = myfib (n-1) + myfib (n-2)


g' ::  Int -> Either () (Int,Int) 
g' 0  = i1 () 
g' x =  i2 ( div (x+x) 2,div (x+x) 2)

g'':: Int -> Either () Int
g'' 0 = i1 ()
g'' x = i2 x

generateAlgae :: Int -> Algae
generateAlgae = anaA g' g''

--testInv :: Bool
testInv n = (length ( showAlgae2 (generateAlgae n) ) )== (myfib  (succ n))