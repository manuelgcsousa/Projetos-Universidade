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



correTestes :: IO ()
correTestes = do
    --files1 <- getDirectoryContents "../tests/T1/"
    --files2 <- getDirectoryContents "../tests/T2/"
    files3 <- getDirectoryContents "../tests/T3/"
    --let inputsT1 = map ("../tests/T1/" ++) $ filter (isSuffixOf ".in") files1
    --let inputsT2 = map ("../tests/T2/" ++) $ filter (isSuffixOf ".in") files2
    let inputsT3 = map ("../tests/T3/" ++) $ filter (isSuffixOf ".in") files3
    --mapM_ (correTeste tarefa1) inputsT1
    --mapM_ (correTeste tarefa2) inputsT2 
    mapM_ (correTeste tarefa3) inputsT3

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

main = do inp <- getContents
          putStr (outStr (tarefa3 (lines inp)))






{- | tarefa 3: valida se é possível o boneco se movimentar tendo em conta três tipos de movimentação: Cima, Baixo, Direita ou Esquerda  

O output desta tarefa é simplesmente a nova coordenada do boneco, tendo em conta o seu tipo de movimentação

* txt corresponde ao mapa completo (tabuleiro+coordenada)
* mapa corresponde ao tabuleiro
* stringCoords corresponde as Coordenadas (lista de strings)
* comandoMapa corresponde ao comando dado. Este comando é lido junto com o mapa, sendo que está por baixo de todas as coordenadas.
  É chamada a função stringCoords, que seleciona todas a lista das coordenadas mais o comando, e usamos a função last para obter o comando.
* posInicialBoneco É a coordenada da posição inicial do boneco. Esta é a primeira das coordenadas que estão logo após o mapa. Por essa mesma razão,
  usou-se a função head para obter o primeiro elemento.
* validaCima valida se o boneco pode mover-se para cima
* validaBaixo valida se o boneco pode mover-se para baixo
* validaEsquerda valida se o boneco pode mover-se para a esquerda
* validaDireita valida se o boneco pode mover-se para a direita

-}



tarefa3 :: [String] -> [String]
tarefa3 txt = [msg]
    where mapa = takeWhile (all carateresValidos) txt
          mapaInverso = reverse mapa
          paresCoords = primeiros coords
          mapacomCarateres = caracterCaixa paresCoords mapaInverso mapaInverso
          stringCoords = dropWhile (all carateresValidos) txt
          coords = converteCoords 1 stringCoords
          paresCoordsSemComando = init paresCoords
          dimx = length (head txt)
          dimy = length mapa
          comandoMapa = last (stringCoords)
          posInicialBoneco = head (paresCoords)
          validaCima = validaUp (head paresCoords) mapacomCarateres
          validaBaixo = validaDown (head paresCoords) mapacomCarateres
          validaEsquerda = validaLeft (head paresCoords) mapacomCarateres
          validaDireita = validaRight (head paresCoords) mapacomCarateres
          msg | (comandoMapa == "U") = validaUp posInicialBoneco mapacomCarateres
              | (comandoMapa == "D") = validaDown posInicialBoneco mapacomCarateres
              | (comandoMapa == "L") = validaLeft posInicialBoneco mapacomCarateres
              | (comandoMapa == "R") = validaRight posInicialBoneco mapacomCarateres

---------------------------------------------------------------------------------------

{- O caracterCaixa identifica, consoante as coordenadas das caixas, o respetivo espaço vazio correspondente a uma caixa
   Com isto, é selecionado esse espaço vazio, com a selecaoCaixa, que por sua vez chama a função transformaCaixa que irá 
   transformar o espaço vazio num 'H' que corresponde a uma caixa no mapa do jogo.
-}

caracterCaixa :: [(Int,Int)] -> [String] -> [String] -> [String]
caracterCaixa [] m w = w 
caracterCaixa ((x,y):t) m w = caracterCaixa t m (selecaoCaixa w x y) 
                              

selecaoCaixa :: [String] -> Int ->  Int -> [String]
selecaoCaixa (h:t) x 0 = transformaCaixa h x : t   
selecaoCaixa (h:t) x y = h : selecaoCaixa t x (y -1)

-- paresCoords como input; 
transformaCaixa :: String -> Int -> String
transformaCaixa (h:t) 0 = 'H': t    
transformaCaixa (h:t) x = h : transformaCaixa t (x-1)












{- | A função validaUp vai validar o movimento do boneco, quando este quer mover-se para cima.
   Nesta função, utiliza-se a função localizaCoords, que localiza uma coordenada no mapa.
   Posto isto, verificamos se, somando uma coordenada no y, ou seja, somando 1 valor para que ele possa subir, se é válido ou não.
   O boneco só se pode movimentar se, na posição em frente estiver um espaço ou um ponto (o espaço corresponde simplesmente a um espaço vazio e o ponto a uma posição final de uma caixa).
   Se a posição em frente não for nem um espaço nem um ponto, a única alternativa é ser uma caixa 'H'.
   Se isto acontecer, o boneco consegue arrastar a caixa, o que faz com que a sua posição mude. 
   Mas, se estiverem duas caixas seguidas, o boneco não as consegue arrastar e, por sua vez, não se consegue movimentar.
   Verifica-se então, somando 2 valores ao y, se existem um espaço ou ponto, para onde uma possível caixa se pode mover.
   Se sim, a coordenada do boneco muda, senão mantem-se igual, e a jogada é inválida.
-}

validaUp :: (Int,Int) -> [String] -> String
validaUp (x,y) m = if (localizaCoords (x,y + 1) m) == ' ' || (localizaCoords (x,y + 1) m) == '.'
                   then (show x ++ " " ++ show (y + 1))
                   else (if (localizaCoords (x,y + 1) m) == 'H' 
                         then (if (localizaCoords (x,y + 2) m) == ' ' || (localizaCoords (x,y + 2) m) == '.'
                               then (show x ++ " " ++ show (y + 1)) 
                               else (show x ++ " " ++ show y))
                         else (show x ++ " " ++ show y))








{- | A função validaDown vai validar o movimento do boneco, quando este quer mover-se para baixo.
   Nesta função, utiliza-se a função localizaCoords, que localiza uma coordenada no mapa.
   Posto isto, verificamos se, subtraindo uma coordenada no y, ou seja, subtraindo 1 valor para que ele possa descer, se é válido ou não.
   O boneco só se pode movimentar se a posição atrás for um espaço ou um ponto. Se a posição atrás não for nem um espaço nem um ponto, a única alternativa é ser uma caixa 'H'.
   Se isto acontecer, o boneco consegue arrastar a caixa, o que faz com que a sua posição mude. 
   Mas, se estiverem duas caixas seguidas, o boneco não as consegue arrastar e, por sua vez, não se consegue movimentar.
   Verifica-se então, subtraindo 2 valores ao y, se existem um espaço ou ponto, para onde uma possível caixa se pode mover.
   Se sim, a coordenada do boneco muda, senão mantem-se igual, e a jogada é inválida.
-}



validaDown :: (Int,Int) -> [String] -> String
validaDown (x,y) m = if (localizaCoords (x,y - 1) m) == ' ' || (localizaCoords (x,y - 1) m) == '.'
                     then (show x ++ " " ++ show (y - 1))
                     else (if (localizaCoords (x,y - 1) m) == 'H' 
                           then (if (localizaCoords (x,y - 2) m) == ' ' || (localizaCoords (x,y - 2) m) == '.'
                                 then (show x ++ " " ++ show (y - 1)) 
                                 else (show x ++ " " ++ show y))
                           else (show x ++ " " ++ show y))






{- | A função validaLeft vai validar o movimento do boneco, quando este quer mover-se para a esquerda.
   Nesta função, utiliza-se a função localizaCoords, que localiza uma coordenada no mapa.
   Posto isto, verificamos se, subtraindo uma coordenada no x, ou seja, subtraindo 1 valor para que ele possa andar para a esquerda, se é válido ou não.
   O boneco só se pode movimentar se, na posição à esquerda estiver um espaço ou um ponto. Se a posição à esquerda não for nem um espaço nem um ponto, a única alternativa é ser uma caixa 'H'.
   Se isto acontecer, o boneco consegue arrastar a caixa, o que faz com que a sua posição mude. 
   Mas, se estiverem duas caixas seguidas, o boneco não as consegue arrastar e, por sua vez, não se consegue movimentar.
   Verifica-se então, subtraindo 2 valores ao x, se existem um espaço ou ponto, para onde uma possível caixa se pode mover.
   Se sim, a coordenada do boneco muda, senão mantem-se igual, e a jogada é inválida.
-}



validaLeft :: (Int,Int) -> [String] -> String
validaLeft (x,y) m = if (localizaCoords (x - 1,y) m) == ' ' || (localizaCoords (x - 1,y) m) == '.'
                     then (show (x - 1) ++ " " ++ show y)
                     else (if (localizaCoords (x - 1,y) m) == 'H' 
                           then (if (localizaCoords (x - 2,y) m) == ' ' || (localizaCoords (x - 2,y) m) == '.'
                                 then (show (x - 1) ++ " " ++ show y) 
                                 else (show x ++ " " ++ show y))
                           else (show x ++ " " ++ show y))










{- | A função validaRight vai validar o movimento do boneco, quando este quer mover-se para a direita.
   Nesta função, utiliza-se a função localizaCoords, que localiza uma coordenada no mapa.
   Posto isto, verificamos se, somando uma coordenada no x, ou seja, somando 1 valor para que ele possa andar para a direita, se é válido ou não.
   O boneco só se pode movimentar se, na posição à direita estiver um espaço ou um ponto. Se a posição a direita não for nem um espaço nem um ponto, a única alternativa é ser uma caixa 'H'.
   Se isto acontecer, o boneco consegue arrastar a caixa, o que faz com que a sua posição mude. 
   Mas, se estiverem duas caixas seguidas, o boneco não as consegue arrastar e, por sua vez, não se consegue movimentar.
   Verifica-se então, somando 2 valores ao x, se existem um espaço ou ponto, para onde uma possível caixa se pode mover.
   Se sim, a coordenada do boneco muda, senão mantem-se igual, e a jogada é inválida.
-}

validaRight :: (Int,Int) -> [String] -> String
validaRight (x,y) m = if (localizaCoords (x + 1,y) m) == ' ' || (localizaCoords (x + 1,y) m) == '.'
                      then (show (x + 1) ++ " " ++ show y)
                      else (if (localizaCoords (x + 1,y) m) == 'H' 
                            then (if (localizaCoords (x + 2,y) m) == ' ' || (localizaCoords (x + 2,y) m) == '.'
                                  then (show (x + 1) ++ " " ++ show y) 
                                  else (show x ++ " " ++ show y))
                            else (show x ++ " " ++ show y))









{- | Carateres válidos dentro da secção do tabuleiro.
obs : vai ser utilizada para dividir o tabuleiro e as coordenadas em duas partes e para validar o mapa
-}
carateresValidos :: Char -> Bool
carateresValidos c = c == '#' || c == ' ' || c == '.'







{- | Converte uma lista de Strings (stringCoords) em triplos (através do processaPosicoes) .
 Em primeiro e segundo são as coordenadas e o terceiro é o contador, ou seja, valor dado na contagem dos erros-}
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



{- | Testa se as coordenadas são constituídas por um par de números 
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

{- | Primeiro dígito : vai retirar enquanto é digito   
     Segundo digito :
                     1. Vai deixar retirar enquanto e digito de forma a retirar o primeiro digito ,
                     2. Após isso vai retirar o espaço vazio 
                     3. Retirar enquanto é dígito
                     -}


algarismos' :: String -> Bool
algarismos' "" = False
algarismos' [a] = if ((a >= '0') && ( a <= '9')) then True else False
algarismos' (h:t) = if ((h >= '0') && (h <= '9')) then algarismos' t else False

-- | A função primeiros tem como input a função converteCoords e retira o contador, deixando apenas pares correspondentes a coordenadas do mapa
primeiros :: [(Int,Int,Int)] -> [(Int,Int)]
primeiros [] = []
primeiros ((a,b,c):t) = (a,b) : primeiros t 


-- | localizaCoords : recebe num par de coordenadas e localiza o caracter respetivo no mapa, confirmando que estas se encontram dentro do mapa (tabuleiro)
localizaCoords :: (Int,Int) -> [String] -> Char
localizaCoords (x,y) m = if (y <= length m && y >= 0) && (x >= 0 && x <= (length (head m))) 
                         then ((m !!! y) !!! x)
                         else '#'

(!!!) :: Eq a => [a] -> Int -> a
(!!!) (h:ts) 0 = h
(!!!) (h:ts) i = (!!!) ts (i-1)