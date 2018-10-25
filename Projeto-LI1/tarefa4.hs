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
    --files3 <- getDirectoryContents "../tests/T3/"
    files4 <- getDirectoryContents "../tests/T4/"
    --let inputsT1 = map ("../tests/T1/" ++) $ filter (isSuffixOf ".in") files1
    --let inputsT2 = map ("../tests/T2/" ++) $ filter (isSuffixOf ".in") files2
    --let inputsT3 = map ("../tests/T3/" ++) $ filter (isSuffixOf ".in") files3
    let inputsT4 = map ("../tests/T4/" ++) $ filter (isSuffixOf ".in") files4
    --mapM_ (correTeste tarefa1) inputsT1
    --mapM_ (correTeste tarefa2) inputsT2 
    --mapM_ (correTeste tarefa3) inputsT3
    mapM_ (correTeste tarefa4) inputsT4

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
          putStr (outStr (tarefa4 (lines inp)))


tarefa4 :: [String] -> [String]
tarefa4 txt = [msg]
    where mapa = takeWhile (all carateresValidos) txt
          mapaInverso = reverse mapa
          paresCoords = primeiros coords
          stringCoords = dropWhile (all carateresValidos) txt
          coords = converteCoords 1 stringCoords
          paresCoordsSemComando = init paresCoords
          comandoMapa = last (stringCoords)
          listaComandos = converterComandoMapa comandoMapa -- Lista que contém os comandos individuais
          comandinho = pai mapaInverso paresCoordsSemComando listaComandos
          tick = mae mapaInverso paresCoordsSemComando listaComandos 0
          stringTick = show (tick)         
          msg = comandinho ++ " " ++ stringTick

---------------------------------------------------------------------------------------
{-|

A funçao pai vai realizar as seguintes tarefas :

1. Aplicar a validaPontos que por sua vez vai analisar o tabuleiro de modo a contar o numero de pontos ,
  ou seja, se ainda existem caixas que necessitam de ser movidas para a sua posição final . 
  
2. Caso ja não exitam pontos na lista de strings (tabuleiros) entao é dado a mensagem Fim , 
  se não existem duas opções : uma é caso ainda existam comandos então a lista de strings e processada de novo com as alterações feitas pelas valida".."
  ou seja as coordenadas sao alteradas e é dada uma nova lista de strings a funçao pai . 

A funçao lerComando tem como objetivo receber as coordenadas do mapa , o tabuleiro e o comando , e conforme este comando 
este vai executar uma das validas , o qual vai alterar as coordenadas do boneco , se este se puder deslocar, e uma caixa caso o boneco desloque uma delas
caso seja dado um comando nao aceitável ou reconhecido, este é ignorado 

--}
pai :: [String] -> [(Int,Int)] -> [String] -> String 
pai (h:t) x [] = if validaPontos (caracterCaixa x (h:t) (h:t)) then "FIM" else "INCOMPLETO"      
pai (h:t) x (c:cs) = if validaPontos (caracterCaixa x (h:t) (h:t))  
                     then "FIM" {- ++ TICK -}
                     else pai (h:t) (lerComandoMapa x (caracterCaixa x (h:t) (h:t)) c) cs
                     
    where -- coordenadas mapaComCaracteres comando
          lerComandoMapa :: [(Int,Int)] -> [String] -> String -> [(Int,Int)]
          lerComandoMapa _ _ [] = []
          lerComandoMapa x y c | c == "U" = validaUp (head x) y x
                               | c == "D" = validaDown (head x) y x  
                               | c == "L" = validaLeft (head x) y x
                               | c == "R" = validaRight (head x) y x 
                               | otherwise = x

{-| A função mae tem a mesma estrutura da função pai , no entanto o seu  output é diferente na questão de em vez de devolver uma msg , vai devolver o número de comandos realmente executados pelo boneco ,
por meio de comparação da lista da lista de strings (tabuleiros), em que caso o tabuleiro dado de input e o dado pelo output do lerComando sejam diferentes então é incrementado +1 ao contador dos comandos executados (i).
casos estes sejam iguais então mantem-se atual tick . 
o validaPontos para saber se o jogo está finalizado ou não. O tick (i) é dado em dois casos: caso os comandos acabem ou caso o mapa esteja finalizado
-}

mae :: [String] -> [(Int,Int)] -> [String] -> Int -> Int 
mae (h:t) x [] i = i      
mae (h:t) x (c:cs) i = if validaPontos (caracterCaixa x (h:t) (h:t))  
                       then i {- ++ TICK -}
                       else if toniCabrita x (lerComandoMapa x (caracterCaixa x (h:t) (h:t)) c)
                            then mae (h:t) (lerComandoMapa x (caracterCaixa x (h:t) (h:t)) c) cs i+1 
                            else mae (h:t) (lerComandoMapa x (caracterCaixa x (h:t) (h:t)) c) cs i
                   
    where -- coordenadas mapaComCaracteres comando
          lerComandoMapa :: [(Int,Int)] -> [String] -> String -> [(Int,Int)]
          lerComandoMapa _ _ [] = []
          lerComandoMapa x y c | c == "U" = validaUp (head x) y x
                               | c == "D" = validaDown (head x) y x  
                               | c == "L" = validaLeft (head x) y x
                               | c == "R" = validaRight (head x) y x 
                               | otherwise = x







{-| a converte comandos serve para transformar a ultima string do ficheiro.txt que serve de input à tarefa , ou seja , os comandos que se pretendem serem executados , de uma string para uma lista de strings 
desta forma vão ser interpretados pela função pai e mae  
-}

-- Converte a string dos comandos numa lista de strings de comandos individuais
converterComandoMapa :: String -> [String]
converterComandoMapa [] = []
converterComandoMapa (h:t) = [h] : converterComandoMapa t 


{-| a validaUp vai validar se o boneco dentro do contexto do mapa pode ou não mover-se, e caso este se mova, se vai ou não alterar também a posição de uma caixa .
Caso este seja alterado então vai ser executado a função  modificaCoordsUp

Esta função vai receber de input a coordenada do elemento a ser modificado (boneco ou caixa) e vai neste caso adicionar +1 unidade à dimensão y da coordenada dada no input, ou seja de modo a mimicar o comando Up (U) no jogo

-}



-- Valida movimento para Cima
validaUp :: (Int,Int) -> [String] -> [(Int,Int)] -> [(Int,Int)]
validaUp (x,y) m c | (localizaCoords (x,y + 1) m) == ' ' || (localizaCoords (x,y + 1) m) == '.' = modificaCoordsUp (x,y) c
                   | (localizaCoords (x,y + 1) m) == 'H' || (localizaCoords (x,y + 1) m) == 'I' = if (localizaCoords (x,y + 2) m) == ' ' || (localizaCoords (x,y + 2) m) == '.'
                                                                                                  then modificaCoordsUp (x,y) (modificaCoordsUp  (x,y + 1) c)
                                                                                                  else c
                   | otherwise = c



modificaCoordsUp :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
modificaCoordsUp (z,k) ((x,y):t) = if z == x && k == y then ((x,y + 1):t) else (x,y):modificaCoordsUp (z,k) t 




{-| a validaDown validar se o boneco dentro do contexto do mapa pode ou não mover-se, e caso este se mova se vai ou não alterar tambem a posição de uma caixa .
Caso este seja alterado entao vai ser executado  a funçao  modificaCoordsDown 

Esta função vai receber de input a coordenada do elemento a ser modificado (boneco ou caixa) e vai neste caso adicionar -1 unidade à dimensao y da coordenada dada no input, ou seja de modo a mimicar o comando Down (D) no jogo


-}

-- Valida movimento para Baixo
validaDown :: (Int,Int) -> [String] -> [(Int,Int)] -> [(Int,Int)]
validaDown (x,y) m c | (localizaCoords (x,y - 1) m) == ' ' || (localizaCoords (x,y - 1) m) == '.' = modificaCoordsDown (x,y) c
                     | (localizaCoords (x,y - 1) m) == 'H' || (localizaCoords (x,y - 1) m) == 'I' = if (localizaCoords (x,y - 2) m) == ' ' || (localizaCoords (x,y - 2) m) == '.'
                                                                                                    then modificaCoordsDown (x,y) (modificaCoordsDown  (x,y - 1) c)
                                                                                                    else c
                     | otherwise = c



modificaCoordsDown :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
modificaCoordsDown (z,k) ((x,y):t) = if z == x && k == y then ((x,y - 1):t) else (x,y):modificaCoordsDown (z,k) t 



{-| a validaLeft vai validar se o boneco dentro do contexto do mapa pode ou não se mover , e caso este se mova se vai ou não alterar também a posição de uma caixa .
Caso este seja alterado então vai ser executado a função  modificaCoordsLeft

Esta função vai receber de input a coordenada do elemento a ser modificado (boneco ou caixa) e vai neste caso subtrair -1 unidade à dimensao x da coordenada dada no input, ou seja de modo a mimicar o comando Left (L) no jogo


-}


-- Valida movimento para a Esquerda
validaLeft :: (Int,Int) -> [String] -> [(Int,Int)] -> [(Int,Int)]
validaLeft (x,y) m c | (localizaCoords (x - 1,y) m) == ' ' || (localizaCoords (x - 1,y) m) == '.' = modificaCoordsLeft (x,y) c
                     | (localizaCoords (x - 1,y) m) == 'H' || (localizaCoords (x - 1,y) m) == 'I' = if (localizaCoords (x - 2,y) m) == ' ' || (localizaCoords (x - 2,y) m) == '.'
                                                                                                    then modificaCoordsLeft (x,y) (modificaCoordsLeft  (x - 1,y) c)
                                                                                                    else c
                     | otherwise = c



modificaCoordsLeft :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
modificaCoordsLeft (z,k) ((x,y):t) = if z == x && k == y then ((x - 1,y):t) else (x,y):modificaCoordsLeft (z,k) t 



{-| a validaRight vai validar se o boneco dentro do contexto do mapa pode ou nao se mover , e caso este se mova se vai ou nao alterar tambem a posição de uma caixa.
Caso este seja alterado entao vai ser executado  a funçao  modificaCoordsRight

Esta função vai receber de input a coordenada do elemento a ser modificado (boneco ou caixa) e vai neste caso adicionar +1 unidade a dimensao x da coordenada dada no input, ou seja de modo a mimicar o comando Right (R) no jogo


-}


-- Valida movimento para a Direita
validaRight :: (Int,Int) -> [String] -> [(Int,Int)] -> [(Int,Int)]
validaRight (x,y) m c | (localizaCoords (x + 1,y) m) == ' ' || (localizaCoords (x + 1,y) m) == '.' = modificaCoordsRight (x,y) c
                      | (localizaCoords (x + 1,y) m) == 'H' || (localizaCoords (x + 1,y) m) == 'I' = if (localizaCoords (x + 2,y) m) == ' ' || (localizaCoords (x + 2,y) m) == '.'
                                                                                                     then modificaCoordsRight (x,y) (modificaCoordsRight  (x + 1,y) c)
                                                                                                     else c

                      | otherwise = c



modificaCoordsRight :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
modificaCoordsRight (z,k) ((x,y):t) = if z == x && k == y then ((x + 1,y):t) else (x,y):modificaCoordsRight (z,k) t 




{-| a validaPontos vai concatetar a Lista de string numa unica string, e após isso conta o número de pontos dentro desta lista, ou seja , dentro do tabuleiro e caso estes sejam 0, então todas as caixas estao na posição final 
dando assim um output booleano True, caso o número seja diferente de zero então o mapa ainda tem caixas que têm de estar na sua posição final e da assim um output False 
-}



validaPontos :: [String] -> Bool
validaPontos x  = if 0 == (contaPontos (concat (x))) then True else False 
                  

    where
          contaPontos [] = 0
          contaPontos (h:t) = if (h == '.')
                              then 1 + contaPontos t
                              else contaPontos t 





{-| Esta função compara as coordenadas de input e output de modo a verificar se estas são iguais ou se foram modificadas ou seja se o boneco e algum elemento do tabuleiro foi modificado
-}
toniCabrita :: [(Int,Int)] -> [(Int,Int)] -> Bool
toniCabrita x y = if x /= y then True else False 






{-| O caracterCaixa identifica, consoante as coordenadas das caixas, o respetivo espaço vazio correspondente a uma caixa
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
transformaCaixa (h:t) 0 = if h == ' ' then 'H': t else 'I': t     
transformaCaixa (h:t) x = h : transformaCaixa t (x-1)









{-|o caracterBoneco recebe de input uma lista da coordenada inicial do boneco e duas vezes o novoMapa , 
uma vez que um irá servir para localizar a coordenada no mapa e outro para ter as o carateres alterado na respetiva posição inicial do boneco  
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








{-| Carateres válidos dentro da secção do tabuleiro.
obs : vai ser utilizada para dividir o tabuleiro e as coordenadas em duas partes e para validar o mapa
-}
carateresValidos :: Char -> Bool
carateresValidos c = c == '#' || c == ' ' || c == '.'







{-| Converte uma lista de Strings (stringCoords) em triplos (através do processaPosicoes) .
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



{-| Testa se as coordenadas são constituídas por um par de números 
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

{-| Primeiro dígito : vai retirar enquanto é digito   
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