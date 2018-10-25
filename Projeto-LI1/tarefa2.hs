module Main where
import Data.Char 
import qualified Data.Text as T
import Data.List
import System.Directory

inStr :: String -> [String]
inStr [] = []
inStr ['\n'] = [[],[]]
inStr (x:xs) = case x of
    '\n' -> []:inStr xs
    otherwise -> case inStr xs of
        y:ys -> (x:y):ys
        [] -> [[x]]

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines (map (T.unpack . T.stripEnd . T.pack) t)

main = do inp <- getContents
          putStr (outStr (tarefa2 (lines inp)))

correTestes :: IO ()
correTestes = do
    --files1 <- getDirectoryContents "../tests/T1/"
    files2 <- getDirectoryContents "../tests/T2/"
    --files3 <- getDirectoryContents "../tests/T3/"
    --let inputsT1 = map ("../tests/T1/" ++) $ filter (isSuffixOf ".in") files1
    let inputsT2 = map ("../tests/T2/" ++) $ filter (isSuffixOf ".in") files2
    --let inputsT3 = map ("../tests/T3/" ++) $ filter (isSuffixOf ".in") files3
    --mapM_ (correTeste tarefa1) inputsT1
    mapM_ (correTeste tarefa2) inputsT2 
    --mapM_ (correTeste tarefa3) inputsT3

-- | Corre um teste para uma tarefa
correTeste :: ([String] -> [String]) -> String -> IO ()
correTeste tarefa input = do
    -- nome do ficheiro
    let nome = reverse $ drop 3 $ reverse input
    -- texto do mapa
    inp <- readFile input
    -- resultado da tarefa
    let o = outStr (tarefa (inStr inp))
    -- resultado esperado
    esp <- readFile (nome ++ ".out")
    putStr ("[" ++ nome ++ "]: ")
    if (o == esp)   -- compara resultados
    then putStrLn "OK"
    else do
        putStrLn "FALHOU!"
        putStr esp
        putStrLn o

{-| a tarefa2 tem como função retirar os carateres redundantes e substituir a posição inicial do boneco por 'o', a posição das caixa por 'H' e caso estas se encontrem num destino final 'I'
txt corresponde ao mapa completo (tabuleiro+coordenada)
mapa corresponde ao tabuleiro
stringCoords corresponde as Coordenadas (lista de strings)
Esta é constituída essencialmente por 4 passos : 

*novoMapa :retira todos os carateres redundantes e torna o mapa menos pesado para a leitura 
*mapaPosicaoBoneco : acrescenta ao novoMapa o carater 'o' na posição inicial do boneco 
*mapaPosicaoCaixa   : acrescenta ao mapaPosicaoBoneco a posição das caixas 
*msg : reverse do mapa posição , uma vez que utilizamos o mapaInverso (reverse do mapa) de modo a ser mais fácil localizar as coordenadas ao carater correspondente no mapa  
-} 

tarefa2 :: [String] -> [String]
tarefa2 txt = msg
    where mapa = takeWhile (all carateresValidos) txt
          mapaInverso = reverse mapa
          stringCoords = dropWhile (all carateresValidos) txt
          coords = converteCoords 1 stringCoords
          paresCoords = primeiros coords
          tiraCoords = comparaCoords 0 0 dimx dimy mapaInverso
          coordsPossiveis = aumentaX 0 0 dimx dimy 
          dimx = (length (head txt)) -1
          dimy = (length mapa) -1
          novoMapa = mae  coordsPossiveis dimx dimy mapaInverso mapaInverso
          mapaPosicaoBoneco = caracterBoneco paresCoords novoMapa novoMapa
          mapaPosicaoCaixa = caracterCaixa (tail paresCoords) mapaPosicaoBoneco mapaPosicaoBoneco
          msg = reverse mapaPosicaoCaixa --msg = mapa alterado (ATENÇÃO, fazer reverse uma vez que se utiliza o mapa invertido)
 

carateresValidos :: Char -> Bool
carateresValidos c = c == '#' || c == ' ' || c == '.'



--------------------------------------------------------------------------------------------
{-| a funçao mae recebe de input uma lista das coordenadas possíveis dentro do mapa (coordsPossiveis) , dimx (valor maximo de x dentro do mapa), dimy (valor maximo de y dentro do mapa) e  
duas listas de strings em que ambos são mapaInverso (sendo que um serve para localização do caracter a que correspode a coordenadas e o outro é efetivamente modificado)
-}

mae :: [(Int,Int)] -> Int -> Int -> [String] -> [String] -> [String]
mae [] dimx dimy m w = w 
mae ((x,y):t) dimx dimy m w = if comparaCoords x y dimx dimy m 
                              then mae t dimx dimy m (selecaoString w x y) 
                              else mae t dimx dimy m w

{-| a selecaoString seleciona a string do mapa (y da coordenada) onde se encontra a coordenada do cardinal redundante para a qual queremos alterar o carater para ' '
-}

selecaoString :: [String]-> Int ->  Int -> [String]
selecaoString (h:t) x 0 = transformaEspaco h x : t   
selecaoString (h:t) x y = h : selecaoString t x (y -1)

{-| a transforamaEspaço seleciona dentro da string dada pela selecaoString o carater correspondente a coordenada (x da coordenada) e é então substituído
-}
transformaEspaco :: String-> Int-> String
transformaEspaco (h:t) 0 = ' ': t   
transformaEspaco (h:t) x = h : transformaEspaco t (x-1)


--------------------------------------------------------------------------------------------
{-|o caracterBoneco recebe de input uma lista da coordenada inicial do boneco e duas vezes o novoMapa , 
uma vez que um ira servir para localizar a coordenada no mapa e outro para ter as o carateres alterado na respetiva posição inicial do boneco  
-}
caracterBoneco :: [(Int,Int)] -> [String] -> [String] -> [String]
caracterBoneco [] m w = w 
caracterBoneco ((x,y):t) m w = selecaoBoneco w x y 
                              
{-|a selecaoBoneco seleciona a string do mapa (y da coordenada) onde se encontra a coordenada da inicial do boneco para a qual queremos alterar o carater para 'o'  
-}
selecaoBoneco :: [String]-> Int ->  Int -> [String]
selecaoBoneco (h:t) x 0 = transformaBoneco h x : t   
selecaoBoneco (h:t) x y = h : selecaoBoneco t x (y -1)

{-| a transforamaBoneco seleciona dentro da string dada pela selecaoCaixa o carater correspondente a coordenada (x da coordenada) e substitui por 'o'
-}
transformaBoneco :: String-> Int-> String
transformaBoneco (h:t) 0 = 'o' : t   
transformaBoneco (h:t) x = h : transformaBoneco t (x-1)




--------------------------------------------------------------------------------------------

{-|o caracterCaixa recebe de input uma lista das coordenadas das caixas e duas vezes o mapaPosicaoBoneco , 
uma vez que um ira servir para localizar as coordenadas no mapa e outro para ter as os carateres alterados nas respetivas posições das caixas 
obs : considera a lista de pares como a Tail dos paresCoords
-}
caracterCaixa :: [(Int,Int)] -> [String] -> [String] -> [String]
caracterCaixa [] m w = w 
caracterCaixa ((x,y):t) m w = caracterCaixa t m (selecaoCaixa w x y) 
                              

{-|a selecaoCaixa seleciona a string do mapa (y da coordenada) onde se encontra a coordenada da caixa para a qual queremos alterar o carater para 'H' (ou 'I' no caso de estar na posição final)  
-}
selecaoCaixa :: [String]-> Int ->  Int -> [String]
selecaoCaixa (h:t) x 0 = transformaCaixa h x : t   
selecaoCaixa (h:t) x y = h : selecaoCaixa t x (y -1)

{-| a transformaCaixa seleciona dentro da string dada pela selecaoCaixa o carater correspondente a coordenada (x da coordenada) e caso seja um espaço vazio 
então é substituído por 'H' se não é substituído por um 'I'
-}
transformaCaixa :: String-> Int-> String
transformaCaixa (h:t) 0 = if h == ' ' then 'H': t else 'I': t     
transformaCaixa (h:t) x = h : transformaCaixa t (x-1)






-------------------------------------------------------------------------------------------
{-| a aumentaX e aumentaY vão constituir a lista de coordenadas possíveis dentro do mapa ou seja de (0,0) ate (dimx,dimy)
(sendo dimx e dimy respetivamente o valor maximo que x pode adquirir e o valor maximo que y pode adquirir)
-}

aumentaX :: Int -> Int -> Int -> Int -> [(Int,Int)]
aumentaX x y dimx dimy = if x >= 0 && x<= dimx  
                         then  aumentaY x y dimy ++ aumentaX (x+1) y dimx dimy
                         else []


aumentaY :: Int -> Int -> Int -> [(Int,Int)]
aumentaY x y dimy = if y >= 0 && y<= dimy  
                    then (x,y) : aumentaY x (y +1) dimy
                    else []
-------------------------------------------------------------------------------------------
{- | a comparaCoords vai receber uma coordenada e vai compara-la as coordenadas a sua volta de forma perceber se esta pode ou não ser removida
obs : tem de input o valor de x e y de uma coordenada o dimx (valor máximo que x pode adquirir), dimy (valor máximo que y pode adquirir) e o mapaInverso-}


comparaCoords :: Int -> Int -> Int -> Int -> [String] -> Bool
comparaCoords x y dimx dimy m |  x == 0 && y == 0 = if ((localizaCoords (x,y) m) == '#') && (localizaCoords (x + 1, y) m) == '#' && (localizaCoords (x, y + 1) m) == '#' && (localizaCoords (x + 1, y + 1) m) == '#' then True else False
                              
                              |  x == 0 && y /= 0 && y /= dimy= if ((localizaCoords (x,y) m) == '#') && (localizaCoords (x + 1, y) m) == '#' && (localizaCoords (x, y + 1) m) == '#' && (localizaCoords (x, y - 1) m) == '#' && (localizaCoords (x + 1, y + 1) m) == '#' && (localizaCoords (x + 1, y - 1) m) == '#'  then True else False
                              
                              |  x == dimx && y == 0 = if ((localizaCoords (x,y) m) == '#') && (localizaCoords (x - 1, y) m) == '#' && (localizaCoords (x, y + 1) m) == '#' && (localizaCoords (x - 1 , y + 1) m) == '#' then True else False

                              |  x == dimx && y /= 0 && y /= dimy = if ((localizaCoords (x,y) m) == '#') && (localizaCoords (x - 1, y) m) == '#' && (localizaCoords (x, y + 1) m) == '#' && (localizaCoords (x, y - 1) m) == '#' && (localizaCoords (x - 1 , y + 1) m) == '#' && (localizaCoords (x - 1, y - 1) m) == '#'  then True else False

                              |  x == dimx && y == dimy = if ((localizaCoords (x,y) m) == '#') && (localizaCoords (x - 1, y) m) == '#' && (localizaCoords (x, y - 1) m) == '#' && (localizaCoords (x -1, y - 1) m) == '#' then True else False
                              
                              |  x /= 0 && y == 0 && x /= dimx = if ((localizaCoords (x,y) m) == '#') && (localizaCoords (x + 1, y) m) == '#' && (localizaCoords (x, y + 1) m) == '#' && (localizaCoords (x - 1, y ) m) == '#' && (localizaCoords (x -1, y + 1) m)  == '#'&& (localizaCoords (x + 1, y + 1) m) == '#' then True else False

                              |  x == 0 && y == dimy = if ((localizaCoords (x,y) m) == '#') && (localizaCoords (x + 1, y) m) == '#' && (localizaCoords (x, y - 1) m) == '#' && (localizaCoords (x + 1, y - 1) m) == '#' then True else False

                              |  x /= 0 && y == dimy && x /= dimx = if ((localizaCoords (x,y) m) == '#') && (localizaCoords (x - 1, y) m) == '#' && (localizaCoords (x, y - 1) m) == '#' && (localizaCoords (x + 1, y) m) == '#' && (localizaCoords (x - 1, y - 1) m) == '#' && (localizaCoords (x + 1 , y - 1) m) == '#' then True else False

                              |  x /= 0 && y /=0 && x /= dimx && y /= dimy = if (localizaCoords (x,y) m) == '#' && (localizaCoords (x - 1, y) m) == '#' && (localizaCoords (x, y - 1) m) == '#' && (localizaCoords (x + 1, y) m) == '#' && (localizaCoords (x, y + 1) m) == '#' && (localizaCoords (x + 1, y + 1) m) == '#' && (localizaCoords (x - 1, y - 1) m) == '#' && (localizaCoords (x + 1, y - 1) m) == '#' && ((localizaCoords (x - 1, y + 1) m) == '#') then True else False

{- | vai dar como output o char a que uma certa coordenada corresponde dentro do mapa 
-}

localizaCoords :: (Int,Int) -> [String] -> Char
localizaCoords (x,y) m = if (y <= (length m)-1 && y >= 0) && (x >= 0 && x <= (length (head m))-1) 
                         then ((m !!! y) !!! x)
                         else ' '

(!!!) :: Eq a => [a] -> Int -> a
(!!!) (h:ts) 0 = h
(!!!) (h:ts) i = (!!!) ts (i-1)


--






{- | a converteCoords, analiseCoords, algarismos e primeiros permitem criar os paresCoords , ou seja, uma lista das coordenadas da posição inicial do boneco e da posição inicial das caixas-}

converteCoords :: Int -> [String] -> [(Int,Int,Int)]
converteCoords n [] = []
converteCoords n (h:t) = if (analiseCoords h) 
                         then (processaPosicoes h) : (converteCoords (n + 1) t)
                         else [(1,1,n+1)]
                  where
          processaPosicoes h = if (algarismos' x && algarismos' y)
                               then (read x, read y, 0)
                               else (-1, -1, n)
          [x, y] = words h 

analiseCoords :: String -> Bool
analiseCoords "" = False
analiseCoords (h:t) = if primeiroDigito /= "" && segundoDigito /= ""
                      then True
                      else False 
  
    where 
         (primeiroDigito, t1) = (takeWhile isDigit (h:t), dropWhile isDigit (h:t))
         t2 = dropWhile (==' ') t1
         segundoDigito = takeWhile isDigit t2



algarismos' :: String -> Bool
algarismos' "" = False
algarismos' [a] = if ((a >= '0') && ( a <= '9')) then True else False
algarismos' (h:t) = if ((h >= '0') && (h <= '9')) then algarismos' t else False

primeiros :: [(Int,Int,Int)] -> [(Int,Int)]
primeiros [] = []
primeiros ((a,b,c):t) = (a,b) : primeiros t 