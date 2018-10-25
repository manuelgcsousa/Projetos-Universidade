{-# OPTIONS_GHC -XNPlusKPatterns #-}

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


data B_Tree a = Nil | Block {leftmost::B_Tree a, block :: [(a,B_Tree a)]} deriving (Show,Eq)

t= Block { 
 leftmost = Block {
    leftmost = Nil,
    block = [(1,Nil),(2,Nil),(5,Nil),(6,Nil)]},
 block = [
    (7,Block {
        leftmost = Nil,
        block = [(9,Nil),(12,Nil)]}),
    (16,Block{
        leftmost = Nil,
        block = [(18,Nil),(21,Nil)]})
  ]}


-- Alinea 1 ---------------------------------------------------------

inB_Tree = either (const (Nil)) (uncurry Block)


outB_Tree Nil = i1 ()
outB_Tree (Block a b)  = i2 (a,b)

cataB_Tree f = f . (recB_Tree (cataB_Tree f)) . outB_Tree

anaB_Tree f = inB_Tree . (recB_Tree (anaB_Tree f)) . f

hyloB_Tree f h = cataB_Tree f . anaB_Tree h

baseB_Tree f g = id -|- (g >< map (f >< g) ) 

recB_Tree g = baseB_Tree id g 

instance Functor B_Tree 
    where fmap f = cataB_Tree (inB_Tree . baseB_Tree f id) 

-- Alinea 2 ---------------------------------------------------------

--inorderito :: B_Tree a -> [a] 
--inorderito Nil = []
--inorderito (Block a b) =  (map fst b ) ++ inorderito a ++ concat (map (inorderito . snd) b) 


inorderB_Tree = either nil join 
        where join(a,b)   = a ++ map fst b ++  concat (map snd b)


inordB_Tree :: B_Tree t -> [t]
inordB_Tree = cataB_Tree inorderB_Tree




-- Alinea 3 ----------------------------------------------------------

largeB_Tree = either (const 0) join 
        where join(a,b)   = maximum ((length (map fst b)) : [a]  ++  ( (map snd b) ))


largestBlock :: B_Tree a -> Int
largestBlock = cataB_Tree g0


g0 (Left ()) = 1
g0 (Right (x,y)) = if x >= maximum(map p2 y) then x else maximum(map p2 y)

-- Alinea 4 -------------------------------------------------------------


mirrorANa = anaB_Tree ( (id -|-  id ><  reverse ) . outB_Tree )

mirrorAnita = anaB_Tree mymirror

invB_Tree :: B_Tree a -> B_Tree a
invB_Tree = cataB_Tree (inB_Tree . (id -|-  id >< reverse  ))

-- Alinea 5 --------------------------------------------------------------
qsepB_Tree [] = Left ()
qsepB_Tree (h:t) = Right (s, singl (h,l)) where (s,l) = part (<h) t

qSortB_Tree :: Ord a => [a] -> [a]
qSortB_Tree = hyloB_Tree (inorderB_Tree) (qsepB_Tree)


-- Alinea 6 --------------------------------------------------------------
dotBTree :: Show a => BTree a -> IO ExitCode
dotBTree = dotpict . bmap nothing (Just . show) . cBTree2Exp

t1= Node (6,(Node (3,(Node (2,(Empty,Empty)),Empty)), Node (7,(Empty,Node (9,(Empty,Empty))))))



mirror :: B_Tree a -> B_Tree a
mirror Nil = Nil
mirror (Block a b) = Block (last(map (mirror.snd) b))  (map swap (zip  ((take (l-1) (map (mirror.snd) b) )++[a]) (map (fst) b) ) )
    where
        l = length (map (mirror.snd) b )


mirrorAux = either (const Nil) join
        where join(a,b) = Block (last (map snd b)) (map swap (zip ((take ((length (map snd b)) -1) (map (snd) b) )++[a]) (map (fst) b) ) )


geneMirror Nil = i1 ()
geneMirror (Block a b) = i2 (snd (last $ reverse b), reverse (map swap $ (zip ([a]++ (take ((length (map snd $ reverse b)) -1) (map (snd) $  reverse b) )) (map (fst) $  reverse b) )) )



--mymirror :: B_Tree a -> B_Tree a
mymirror Nil = i1 ()
mymirror (Block a b) = i2 ((snd $ head $ reverse b) ,  [(fst $ head $ reverse b  ,a )] ++ ( (take (length b -1) b )) )