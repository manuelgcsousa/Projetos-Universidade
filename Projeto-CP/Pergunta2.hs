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


work = cataList (split (either (const 0) (p1.cons) ) (either (const True) (p2.cons) ) )