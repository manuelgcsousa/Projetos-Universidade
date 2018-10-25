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

type Equipa = String

equipas :: [Equipa]
equipas = [
   "Arouca","Belenenses","Benfica","Braga","Chaves","Feirense",
   "Guimaraes","Maritimo","Moreirense","Nacional","P.Ferreira",
   "Porto","Rio Ave","Setubal","Sporting","Estoril"
   ]

equipasArvore :: [Equipa]
equipasArvore = [
    "Sporting", "Chaves", "P.Ferreira", "Benfica", "Porto", "Braga",
    "Setubal", "Feirense", "Guimaraes", "Belenenses", "Moreirense", "Maritimo",
    "Arouca", "Estoril", "Rio Ave", "Nacional"]


pap :: Eq a => [(a, t)] -> a -> t
pap m k = unJust (lookup k m) where unJust (Just a) = a

jogo :: (Equipa, Equipa) -> Dist Equipa
jogo(e1,e2) = D [ (e1,1-r1/(r1+r2)),(e2,1-r2/(r1+r2)) ] where
              r1 = rank e1
              r2 = rank e2
              rank = pap ranks
              ranks = [
                  ("Arouca",5),
                  ("Belenenses",3),
                  ("Benfica",1),
                  ("Braga",2),
                  ("Chaves",5),
                  ("Feirense",5),
                  ("Guimaraes",2),
                  ("Maritimo",3),
                  ("Moreirense",4),
                  ("Nacional",3),
                  ("P.Ferreira",3),
                  ("Porto",1),
                  ("Rio Ave",4),
                  ("Setubal",4),
                  ("Sporting",1),
                  ("Estoril",5)]

sorteio = anaLTree lsplit . envia . permuta'

x = permuta' equipas 

--y = anaLTree lsplit . (envia x) 


permuta :: [a] -> [a]
permuta [] = []
permuta l@(x:xs) = z : permuta k
 where
    (z,k) = envia (getR l)



-- Primeira Função do Trabalho
permuta' :: [a] -> IO [a]
permuta' [] = return []
permuta' l@(x:xs) = do {(z,k) <- (getR l) ; u <- permuta' k ; return(z:u)} 

envia = unsafePerformIO

getR :: [a] -> IO (a, [a])
getR x = do {
               i <- getStdRandom (randomR (0,length x-1));
               return (x!!i,retira i x)
             } where retira i x = take i x ++ drop (i+1) x


permutaTeste :: [a] -> [a]
permutaTeste x = fst (envia (getR x)) : (permuta (snd (envia (getR x))))


--eliminatoria :: LTree Equipa -> Dist Equipa 
--eliminatoria (Leaf x) = 
--eliminatoria (Fork (x,y)) = 



poeLista :: LTree Equipa -> [Equipa]
poeLista (Leaf x) = [x]
poeLista (Fork (x,y)) = poeLista x ++ poeLista y 

poe8Pares :: [Equipa] -> [(Equipa,Equipa)]
poe8Pares [] = [] 
poe8Pares (x:y:xs) = (x,y) : poe8Pares xs 

calProb1Equipa :: Equipa -> [Equipa] -> [Dist Equipa]
calProb1Equipa _ [] = [] 
calProb1Equipa x (y:ys) = if x /= y then jogo(x,y) : calProb1Equipa x ys else calProb1Equipa x ys

daProb :: Equipa -> Dist Equipa -> Float 
daProb _ (D []) = 0
daProb e (D ((x,y):xs)) = if x == e then y else daProb e (D xs) 

daTotalProb :: Equipa -> [Dist Equipa] -> [Float]
daTotalProb _ [] = []
daTotalProb x (y:ys) = daProb x y : daTotalProb x ys 

daPar :: Equipa -> [(Equipa,Equipa)] -> Int 
daPar _ [] = 0
daPar x ((y,z):ys) = if x == y || x == z then 1 else 1 + daPar x ys 

calFinal :: Equipa -> [(Equipa,Equipa)] -> [Float] -> Float
calFinal x y (e1:e2:e3:e4:e5:e6:e7:ys) | daPar x y == 1 = e1 * (e2 + e3) * (e4 + e5 + e6 + e7) * sum ys
calFinal x y (e1:e2:e3:e4:e5:e6:e7:ys) | daPar x y == 2 = e3 * (e1 + e2) * (e4 + e5 + e6 + e7) * sum ys
calFinal x y (e1:e2:e3:e4:e5:e6:e7:ys) | daPar x y == 3 = e5 * (e6 + e7) * (e1 + e2 + e3 + e4) * sum ys
calFinal x y (e1:e2:e3:e4:e5:e6:e7:ys) | daPar x y == 4 = e7 * (e5 + e6) * (e1 + e2 + e3 + e4) * sum ys
calFinal x y l@(e1:e2:e3:e4:e5:e6:e7:e8:e9:e10:e11:e12:e13:e14:e15:ys) | daPar x y == 5 = e9 * (e10 + e11) * (e12 + e13 + e14 + e15) * sum (take 8 l)
calFinal x y l@(e1:e2:e3:e4:e5:e6:e7:e8:e9:e10:e11:e12:e13:e14:e15:ys) | daPar x y == 6 = e11 * (e9 + e10) * (e12 + e13 + e14 + e15) * sum (take 8 l)
calFinal x y l@(e1:e2:e3:e4:e5:e6:e7:e8:e9:e10:e11:e12:e13:e14:e15:ys) | daPar x y == 7 = e13 * (e14 + e15) * (e9 + e10 + e11 + e12) * sum (take 8 l)
calFinal x y l@(e1:e2:e3:e4:e5:e6:e7:e8:e9:e10:e11:e12:e13:e14:e15:ys) | daPar x y == 8 = e15 * (e13 + e14) * (e9 + e10 + e11 + e12) * sum (take 8 l)

calFinalRec :: [Equipa] -> [Equipa] -> [Float] 
calFinalRec [] _= [] 
calFinalRec _ [] = []
calFinalRec l (x:xs) = calFinal x (poe8Pares l) (daTotalProb x (calProb1Equipa x l)) : calFinalRec l xs 

