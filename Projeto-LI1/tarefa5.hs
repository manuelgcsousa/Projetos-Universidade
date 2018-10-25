module Main where
import qualified Data.Text as T
import Graphics.Gloss
import GlossExtras
import Data.Char
import Graphics.Gloss.Data.Color



main = do inp <- getContents
          let (x,y) = tarefa5 (readPicture inp)
          putStrLn (show (round x) ++ " " ++ show (round y))
{-| A tarefa 5 tem como função receber uma Picture de input, e de output a largura e altura da "Bounding Box" do input .
Esta função tem quatro variaveis locais :

*ListaPontos : Vai executar a calculaPictures que nos da a XMax,XMin, YMax, Ymin de todas as Pictures do Input 
*largura  - vai executar a choveBebados que vai selecionar o XMax e Xmin da listaPontos e faz a diferença dos Dois dando assim a a largura da BoundingBox que engloba o Input
*altura  - vai executar a choveCilindros que vai selecionar o YMax e Ymin da listaPontos e faz a diferença dos Dois dando assim a a altura da BoundingBox que engloba o Input
*msg = é o output final da nossa tarefa , juntando assim a largura e altura num par de Floats 

-}       

tarefa5 :: Picture -> (Float, Float)
tarefa5 x = msg where
    listaPontos=calculaPicture x 
    largura = (choveBebados listaPontos) 
    altura =(choveCilindros listaPontos)  
    msg = (largura,altura)

{-| choveBebados vai selecionar o XMax e Xmin da listaPontos e faz a diferença dos Dois dando assim a a largura da BoundingBox que engloba o Input 
O xMax da String de floats é selecionado pela renaMaiorX e o xMin da String de floats é selecionado pela renaMenorX .
-}   


choveBebados :: [(Float,Float)] -> Float -- largura 
choveBebados [] = 0
choveBebados x = (renaMaiorX x) - (renaMenorX x) where 

        renaMaiorX :: [(Float,Float)] -> Float 
        renaMaiorX ((x,y):t) = aux x ((x,y):t) where
            aux x [] = x
            aux k ((x,y):t) = if k < x then aux x t else aux k t 


        renaMenorX :: [(Float,Float)] -> Float
        renaMenorX ((x,y):t) = aux x ((x,y):t) where
            aux x [] = x 
            aux k ((x,y):t) = if k > x then aux x t else aux k t 


{-| choveCilindros vai selcionar o YMax e Ymin da listaPontos e faz a diferença dos Dois dando assim a altura da BoundingBox que engloba o Input
o O xMax da String de floats é selecionado pela renaMaiorX e o xMin da String de floats é selecionado pela renaMenorX .
-}

choveCilindros :: [(Float,Float)] -> Float
choveCilindros [] = 0
choveCilindros x = (renaMaiorY x) - (renaMenorY x) where

renaMaiorY :: [(Float,Float)] -> Float
renaMaiorY ((x,y):t) = aux x ((x,y):t) where
    aux x [] = x
    aux k ((x,y):t) = if k < y then aux y t else aux k t 

renaMenorY :: [(Float,Float)] -> Float
renaMenorY ((x,y):t) = aux y ((x,y):t) where
    aux x [] = x
    aux k ((x,y):t) = if k > y then aux y t else aux k t 

-- | inToFloat vai receber um int de input e transforma-o num Float. Esta função é utilizada no Bitmap para transformar a width e length dada neste Construtor (originalmente um int) em Float para ser mais fácil trabalhar com o mesmo.


intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)


{-| a função CalculaPicture é a função principal desta Tarefa , a qual utiliza pattern Matching e recursividade para aplicar os processos indicados a cada construtor de forma a obter uma lista de XMax,XMin, YMax, Ymin de Todas as Pictures do Input .

-}
calculaPicture :: Picture -> [(Float,Float)]
calculaPicture Blank = []
calculaPicture (Polygon pontos ) = calculaPolygon pontos
calculaPicture (Line pontos) = calculaPolygon pontos
calculaPicture (Circle raio) = calculaCircle raio
calculaPicture (Bitmap x y z w) = calculaBitmap (intToFloat x) (intToFloat y) 
calculaPicture (Color cor imagem) = calculaPicture imagem
calculaPicture (Translate floatX floatY imagem) = 
                map (\(h,t) -> ((h+floatX),(t+floatY))) (calculaPicture imagem)
calculaPicture (Rotate float imagem) = calculaRotate float imagem
calculaPicture (Scale floatX floatY imagem) = 
                map (\(h,t) -> ((h*floatX),(t*floatY))) (calculaPicture imagem)
calculaPicture (Pictures imagens) = concat (map calculaPicture imagens)




{-| Calcula da lista de pontos dada no Construtor Polygon o  XMax,XMin, YMax, Ymin , fazendo depois uma lista com emparelhados com o elemento 0 , 
uma vez que apenas interessa saber o valor Máximo e Mínimo de uma determinada figura para calcular a altura e largura do output final
-}
calculaPolygon :: [(Float,Float)] -> [(Float,Float)]
calculaPolygon [] = []
calculaPolygon pontos = [(paiNatalMenorX pontos) , (paiNatalMaiorX pontos) , (paiNatalMenorY pontos) , (paiNatalMaiorY pontos) ]


{-|  Calcula através do raio o XMax,XMin, YMax, Ymin, fazendo depois uma lista com emparelhados com o elemento 0 
-}
calculaCircle :: Float -> [(Float,Float)]
calculaCircle 0 = []
calculaCircle raio = [(-raio,0), (raio,0), (0,-raio), (0,raio)]


{-|  Calcula atravez do width e length o  XMax,XMin, YMax, Ymin 
-}

calculaBitmap :: Float -> Float -> [(Float,Float)]
calculaBitmap 0 0 = []
calculaBitmap x y = [(-x/2,-y/2),(-x/2, y/2),(x/2,y/2),(x/2,-y/2) ]



{-| a funçao CalculaRotate é a função utilizada pela CalculaPictura para calcular o construtor Rotate, a qual utiliza pattern Matching e recurisvidade para aplicar os processos indicados a cada construtor de forma a obter uma lista de XMax,XMin, YMax, Ymin de Todas as Pictures do Input apos terem sofrido rotações.

-}
    

calculaRotate :: Float -> Picture -> [(Float,Float)]
calculaRotate float Blank = []
calculaRotate float (Rotate float1 imagem) = calculaRotate ((float)+(float1)) imagem 
calculaRotate float (Polygon pontos) = calculaPolygon (roda (float) pontos)
calculaRotate float (Line pontos) =  calculaPolygon (roda (float) pontos)
calculaRotate float (Circle raio) = calculaPicture (Translate (fst(head(roda (float) [(0.0,0.0)]))) (snd(head(roda (float) [(0.0,0.0)]))) (Circle raio))
calculaRotate float (Bitmap x y z w) = roda ((1)*(float)) (calculaBitmap (intToFloat x) (intToFloat y) )
calculaRotate float (Color cor imagem) = calculaRotate (float) imagem 
calculaRotate float (Translate floatX floatY imagem) = map (\(h,t) -> ((h+floatX),(t+floatY))) (calculaRotate (float) imagem)
calculaRotate float (Scale floatX floatY imagem) = map (\(h,t) -> ((h*floatX),(t*floatY))) (calculaRotate (float) imagem)
calculaRotate float (Pictures imagens) = concat (map (calculaRotate (float)) imagens)

{-| esta função recebe um ângulo de rotação e transforma-o em radianos, e após isso vai aplicar uma fórmula a todos os pontos de forma a saber qual é a posição do pontos após esta rotação.
-}  


roda :: Float -> [(Float,Float)] -> [(Float,Float)]
roda float [] = []
roda float imagem  = map (\(x,y) -> (((cos (a) ) * x + (sin(a))*y ), ((cos (a)) *y - (sin(a))*x))) imagem where
                            a = (1) *(pi * (float/180))
 

{-| as funções paiNatal vão ter como função calcular um par de floats. 
Cada uma das funções paiNatal vai receber uma lista de floats do construtor Polygon e depois calcular pares com XMax, XMin, YMax, Ymin, respetivamente. 
-}
paiNatalMaiorX :: [(Float,Float)] -> (Float,Float)
paiNatalMaiorX ((x,y):t) = aux x ((x,y):t) where
    aux x [] = (x,0) 
    aux k ((x,y):t) = if k < x then aux x t else aux k t 


paiNatalMenorX :: [(Float,Float)] -> (Float,Float)
paiNatalMenorX ((x,y):t) = aux x ((x,y):t) where
    aux x [] = (x,0) 
    aux k ((x,y):t) = if k > x then aux x t else aux k t 


paiNatalMaiorY :: [(Float,Float)] -> (Float,Float)
paiNatalMaiorY ((x,y):t) = aux y ((x,y):t) where
    aux x [] = (0,x) 
    aux k ((x,y):t) = if k < y then aux y t else aux k t 

paiNatalMenorY :: [(Float,Float)] -> (Float,Float)
paiNatalMenorY ((x,y):t) = aux y ((x,y):t) where
    aux x [] = (0,x) 
    aux k ((x,y):t) = if k > y then aux y t else aux k t 



