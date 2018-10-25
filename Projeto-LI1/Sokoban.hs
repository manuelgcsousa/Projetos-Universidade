module Main where

import Data.Char

import qualified Data.Text as T
import Data.List
import System.Directory

-- * Funções do mooshak

-- | Parte uma @String@ numa lista de linhas
inStr :: String -> [String]
inStr [] = []
inStr ['\n'] = [[],[]]
inStr (x:xs) = case x of
    '\n' -> []:inStr xs
    otherwise -> case inStr xs of
        y:ys -> (x:y):ys
        [] -> [[x]]

-- | Junta uma lista de linhas numa @String@
outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines (map (T.unpack . T.stripEnd . T.pack) t)

-- * Funções de teste

-- | Corre múltiplos testes para as três tarefas
correTestes :: IO ()
correTestes = do
    files1 <- getDirectoryContents "../tests/T1/"
    --files2 <- getDirectoryContents "../tests/T2/"
    --files3 <- getDirectoryContents "../tests/T3/"
    let inputsT1 = map ("../tests/T1/" ++) $ filter (isSuffixOf ".in") files1
    --let inputsT2 = map ("../tests/T2/" ++) $ filter (isSuffixOf ".in") files2
    --let inputsT3 = map ("../tests/T3/" ++) $ filter (isSuffixOf ".in") files3
    mapM_ (correTeste tarefa1) inputsT1
    --mapM_ (correTeste tarefa2) inputsT2 
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
        putStrLn "FALHOU"
        putStr esp
        putStrLn o

main = do inp <- getContents
          putStr (outStr (tarefa1 (lines inp)))

{- | tarefa 1: retorna a mensagem da validaçao do codigo  

Esta pode ser "OK" caso o codigo seja valido ou "Número" o menor numero da linha onde ocorre um erro :

* txt corresponde ao mapa completo (tabuleiro+coordenada)
* mapa corresponde ao tabuleiro
* stringCoords corresponde as Coordenadas (lista de strings)
* Validaçoes : 
          
          * erroMapa : constituida por ValidaMapa 
          * erroBordas : constituida por validaBordas 
          * erroPontos : constituida por validaCaixas
          * erroCords : constituida por validaCoords e caso este nao apresente erros entao e executado o verificadas 
          * verificadas : constituido por verificaCoords 
          * dimx : comprimento do mapa 
          * dimy : largura do mapa  
          * linhaRepetida : constituida por tiralinha, zipada, repetidos e numeroDeLinhas  

-}


tarefa1 :: [String] -> [String]
tarefa1 [] = ["1"]
tarefa1 txt = [msg]
    where (tabuleiro,coordenadasNoMapa) = dividirMapa txt
          mapa = tabuleiro
          mapaInverso = reverse tabuleiro
          stringCoords = espaco coordenadasNoMapa
          erroMapa = validaMapa mapa  
          erroBordas = validaBordas mapa 1  
          erroPontos = validaCaixas mapa stringCoords
          erroCoords = if (validaCoords dimx dimy (dimy + 1) stringCoords == 0) then verificadas else (validaCoords dimx dimy (dimy + 1) stringCoords)
          coords = converteCoords 1 stringCoords
          paresCoords = primeiros coords
          verificadas = verificaCoords paresCoords mapaInverso (dimx +1) 
          boolRepetidos = repetidos paresCoords []
          linhas = numeroDeLinhas (dimy + 1)  tamanhoTXT
          boolLinha = zipada boolRepetidos linhas 
          linhaRepetida = tiraLinha boolLinha
          dimx = length (head mapa)
          dimy = length mapa
          tamanhoTXT = length coordenadasNoMapa + dimy + 1
          msg = if ((juntaErros erroMapa (juntaErros erroBordas (juntaErros erroCoords (juntaErros erroPontos  linhaRepetida )))) == 0)
                 then "OK" 
                   else show $ (juntaErros erroMapa (juntaErros erroBordas (juntaErros erroCoords (juntaErros erroPontos  linhaRepetida ))))


{- | divide o Mapa em listas de strings umas correspondem a um tabuleiro e a outra as coordenadas no mapa 
-}

dividirMapa :: [String] -> ([String],[String])
dividirMapa [] = ([],[])
dividirMapa (h:hs) = if carateresValidos1 h then (h:tabuleiro,coordenadasNoMapa) else ([],h:hs)
  where
  (tabuleiro,coordenadasNoMapa) = dividirMapa hs

{- | parametros utilizados para dividir o mapa 
-}
carateresValidos :: Char -> Bool
carateresValidos z = (z == '#' || z == '.' || z == ' ') 

carateresValidos1 :: String -> Bool
carateresValidos1 [] = True
carateresValidos1 (h:hs) = carateresValidos h && carateresValidos1 hs 

carateresValidos2 :: [String] -> Bool 
carateresValidos2 [] = True
carateresValidos2 (h:hs) = carateresValidos1 h && carateresValidos2 hs 



espaco :: [String] -> [String]
espaco [] = []
espaco x = if (last x) == "" || last x == "   " then espaco (init x) else x 








                            
{- | calcula o menor inteiro entre dois 
obs : caso um destes seja 0 automaticamente dará o outro número
-}
juntaErros :: Int -> Int -> Int
juntaErros 0 j = j
juntaErros i 0 = i
juntaErros i j = min i j   

{- | carateres validos dentro da secção do tabuleiro.
obs : vai ser utilizada para dividir o tabuleiro e as coordenadas em duas partes e para validar o mapa
-}






-- | testa se o mapa (tabuleiro) tem todo o mesmo comprimento 
validaMapa :: [String] -> Int
validaMapa [] = 1
validaMapa (h:t) = aux (length h) 1 (h:t)
    where aux tam n [] = 0
          aux tam n (y:ys) = if length y == tam && testaCarateres y
                             then aux tam (n+1) ys
                             else n
          testaCarateres [] = True
          testaCarateres (x:xs) = if carateresValidos x && testaCarateres xs
                                  then True
                                  else False
















{- | vai analisar em primeiro lugar as bordas horizontais 
   caso cumpram as condições vai analisar as bordasVerticais -}

-- erroBordas = validaBordas mapa 1 
validaBordas :: [String] -> Int -> Int
validaBordas [] n = 0
validaBordas (h:t) n = if and (map (== '#') h)
                       then if and (map (== '#') (last (h:t)))
                            then validaVerticais t (n + 1)
                            else (length (h:t))  
                       else n 

     
validaVerticais :: [String] -> Int -> Int
validaVerticais [] k = 0
validaVerticais (h:t) k = if ((head h) == '#') && ((last h) == '#')
                          then validaVerticais t (k + 1)
                          else k 







                          












{- | validaCoords : vai introduzir no testaCoords a lista de triplos (constiuido por uma coordenada e o contador) dada pelo converteCoords.
caso seja igual a zero volta a repetir o processo para o resto da lista de triplos dada pelo converteCoords.

o testacoords confirma se o contador dado pelo coverteCoords é diferente de 0 , ou seja , se existem erros , e se as coordenadas estao dentro do mapa . 
obs : o Int dado por esta função é binário podendo ser interpretado como um booleano . -}

validaCoords :: Int -> Int -> Int -> [String] -> Int
validaCoords  _ _ _ [] = 0
validaCoords dimx dimy n (h:t) = if (testaCoords (head (converteCoords 1 (h:t)))) == 0  
                                 then validaCoords dimx dimy (n + 1) t
                                 else n
    where
      testaCoords :: (Int, Int, Int) -> Int
      testaCoords (x, y, z) = if (z /= 0)
                                  then z
                                  else (if (x < dimx && y < dimy)
                                        then 0
                                        else 1)

{- | converte uma lista de Strings (stringCoords) em triplos (através do processaPosicoes) .
 primeiro e segundo sao as coordenadas e o terceiro é o contador , ou seja , valor dado na contagem dos erros-}

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


algarismos' :: String -> Bool
algarismos' "" = False
algarismos' [a] = if ((a >= '0') && (a <= '9')) then True else False
algarismos' (h:t) = if ((h >= '0') && (h <= '9')) then algarismos' t else False

{- | testa se as coordenadas são constituídas por um par de números 
-}
analiseCoords :: String -> Bool
analiseCoords "" = False
analiseCoords (h:t) = if primeiroDigito /= "" && segundoDigito /= ""
                      then True
                      else False 
  
    where 
         (primeiroDigito, t1) = (takeWhile isDigit (h:t), dropWhile isDigit (h:t))
         t2 = dropWhile (==' ') t1
         segundoDigito = takeWhile isDigit t2
{- | primeiro dígito : vai retirar tirar enquanto é digit   
     segundo dígito :
                     1. vai deixar retirar enquanto é dígito de forma a retirar o primeiro dígito ,
                     2. após isso vai retirar o espaço vazio 
                     3. retirar enquanto é dígito
                     -}













 
-- | a função primeiros tem como input a função converteCoords e retira o contador , deixando apenas pares correspondentes a coordenadas do mapa
primeiros :: [(Int,Int,Int)] -> [(Int,Int)]
primeiros [] = []
primeiros ((a,b,c):t) = (a,b) : primeiros t 

{- | verificaCoords :verifica se as coordenadas são um espaço vazio ou um pontos, uma vez que correspondem a posição inicial das caixas e a posição do boneco

localizaCoords : recebe num par de coordenadas e localiza o caracter respetivo no mapa , confirmando que estas se encontram dentro do mapa (tabuleiro)
 -}

verificaCoords :: [(Int,Int)] -> [String] -> Int -> Int
verificaCoords [] m n = 0
verificaCoords ((x,y):xs) m n = if m /= [] 
                                then (if (localizaCoords (x,y) m) == ' ' || (localizaCoords (x,y) m) == '.'
                                      then verificaCoords xs m (n+1) 
                                      else n)
                                else n 
    where                              
         localizaCoords :: (Int,Int) -> [String] -> Char
         localizaCoords (x,y) m = if (y <= length m -1 && y >= 0) && (x >= 0 && x <= (length (head m))-1) 
                                  then ((m !!! y) !!! x)
                                  else '#'
                                  
(!!!) :: Eq a => [a] -> Int -> a
(!!!) (h:ts) 0 = h
(!!!) (h:ts) i = (!!!) ts (i-1)



{-| determina se o número de pontos no mapa (posição final de caixas ) é igual ao número de coordenadas das caixas 
 -}

validaCaixas :: [String] -> [String] -> Int
validaCaixas x y = if (((length y) - 1) == (contaPontos (concat (x)))) 
                   then 0 
                   else if  (((length y) - 1) > (contaPontos (concat (x)))) 
                        then (length x) + (length y) 
                        else (length x) + (length y) + 1

    where
          contaPontos [] = 0
          contaPontos (h:t) = if (h == '.')
                              then 1 + contaPontos t
                              else contaPontos t 
          
---------------------------------------------------------------------------------------------------------------
{- | Verifica se não existem coordenadas repetidas e no caso de existir irá dar a primeira linha repetida
-}

repetidos :: [(Int, Int)] -> [(Int, Int)] -> [Bool]
repetidos [] _ = []
repetidos (ponto:pontos) repetido
        | notElem ponto repetido = True:(repetidos pontos (ponto:repetido))
        | otherwise = False:(repetidos pontos (ponto:repetido))




--- y = 1 dimy = dimy 
numeroDeLinhas :: Int-> Int -> [Int]
numeroDeLinhas y dimy = if  y<= dimy  
                        then y : numeroDeLinhas (y +1) dimy
                        else []


zipada :: [Bool] -> [Int] -> [(Bool,Int)]
zipada x y = zip x y 


tiraLinha :: [(Bool,Int)] -> Int
tiraLinha [] = 0
tiraLinha [(x,y)] =  if x == True then 0 else y
tiraLinha ((x,y):t) = if x == False then y else tiraLinha t 