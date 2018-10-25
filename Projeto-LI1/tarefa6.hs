module Main where 
import Graphics.Gloss -- interface gloss
import Graphics.Gloss.Data.Picture -- importar o tipo Picture
import Graphics.Gloss.Interface ◦ Pure.Game -- importar o tipo Event
import Gloss 
main = do inp <- readFile

	joga (mapaInicial inp) desenhaJogo reageEvento





-- Uma representaçao do estado do jogo.
type Estado =((Int,Int),[String],[(Int,Int)])




-- O estado inicial do jogo.
mapaInicial :: String -> Estado
mapaInicial txt = ((head paresCoords),novoMapa, paresCoords) 
  
  where
	    mapa = takeWhile (all carateresValidos) txt
      stringCoords = dropWhile (all carateresValidos) txt
      coords = converteCoords 1 stringCoords
      paresCoords = primeiros coords
      dimx = (length (head txt)) -1
      dimy = (length mapa) -1
      mapaInverso = reverse mapa
      coordsPossiveis = aumentaX 0 0 dimx dimy 
      novoMapa = reverse (mae coordsPossiveis dimx dimy mapaInverso mapaInverso)

          
          
           carateresValidos :: Char -> Bool
           carateresValidos c = c == '#' || c == ' ' || c == '.'
          
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

		    primeiros :: [(Int,Int,Int)] -> [(Int,Int)]
		    primeiros [] = []
		    primeiros ((a,b,c):t) = (a,b) : primeiros t 

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


            aumentaX :: Int -> Int -> Int -> Int -> [(Int,Int)]
            aumentaX x y dimx dimy = if x >= 0 && x<= dimx  
                                     then  aumentaY x y dimy ++ aumentaX (x+1) y dimx dimy
                                     else []


			aumentaY :: Int -> Int -> Int -> [(Int,Int)]
			aumentaY x y dimy = if y >= 0 && y<= dimy  
			                    then (x,y) : aumentaY x (y +1) dimy
			                    else []









-- Funçao que desenha o jogo.
desenhaJogo :: Estado -> Picture
desenhaJogo = 


-- Fun¸c˜ao que altera o estado do jogo.
reageEvento :: Char -> Estado -> Estado
reageEvento "U" mapa = validaUp :: (Int,Int) -> [String] -> [(Int,Int)] -> [(Int,Int)]
validaUp (x,y) m c = if (localizaCoords (x,y + 1) m) == ' ' || (localizaCoords (x,y + 1) m) == '.'
                   then modificaCoordsUp (x,y) c 
                   else (if (localizaCoords (x,y + 1) m) == 'H' || (localizaCoords (x,y + 1) m) == 'I'
                         then (if (localizaCoords (x,y + 2) m) == ' ' || (localizaCoords (x,y + 2) m) == '.'
                               then modificaCoordsUp (x,y + 1) (modificaCoordsUp  (x,y) c)
                               else c)
                         else c)
                   where 
						modificaCoordsUp :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
						modificaCoordsUp (z,k) ((x,y):t) = if z == x && k == y then ((x,y + 1):t) else (x,y):modificaCoordsUp (z,k) t 

reageEvento "D" mapa = validaDown :: (Int,Int) -> [String] -> [(Int,Int)] -> [(Int,Int)]
validaDown (x,y) m c = if (localizaCoords (x,y - 1) m) == ' ' || (localizaCoords (x,y - 1) m) == '.'
                       then modificaCoordsDown (x,y) c 
                       else (if (localizaCoords (x,y - 1) m) == 'H' || (localizaCoords (x,y - 1) m) == 'I'
                             then (if (localizaCoords (x,y - 2) m) == ' ' || (localizaCoords (x,y - 2) m) == '.'
                                   then modificaCoordsDown (x,y - 1) (modificaCoordsDown  (x,y) c)
                                   else c)
                             else c)
                       where
							modificaCoordsDown :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
							modificaCoordsDown (z,k) ((x,y):t) = if z == x && k == y then ((x,y - 1):t) else (x,y):modificaCoordsDown (z,k) t 



reageEvento "R" mapa = validaRight :: (Int,Int) -> [String] -> [(Int,Int)] -> [(Int,Int)]
validaRight (x,y) m c = if (localizaCoords (x + 1,y) m) == ' ' || (localizaCoords (x + 1,y) m) == '.'
                   then modificaCoordsUp (x,y) c 
                   else (if (localizaCoords (x + 1,y) m) == 'H' || (localizaCoords (x + 1,y) m) == 'I'
                         then (if (localizaCoords (x + 2,y) m) == ' ' || (localizaCoords (x + 2,y) m) == '.'
                               then modificaCoordsUp (x + 1,y) (modificaCoordsUp  (x,y) c)
                               else c)
                         else c)
                 where 
					  modificaCoordsRight :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
					  modificaCoordsRight (z,k) ((x,y):t) = if z == x && k == y then ((x + 1,y):t) else (x,y):modificaCoordsUp (z,k) t 

reageEvento "L" mapa = validaLeft :: (Int,Int) -> [String] -> [(Int,Int)] -> [(Int,Int)]
validaLeft (x,y) m c = if (localizaCoords (x - 1,y) m) == ' ' || (localizaCoords (x - 1,y) m) == '.'
                       then modificaCoordsLeft (x,y) c 
                       else (if (localizaCoords (x - 1,y) m) == 'H' || (localizaCoords (x - 1,y) m) == 'I'
                             then (if (localizaCoords (x - 2,y) m) == ' ' || (localizaCoords (x - 2,y) m) == '.'
                                   then modificaCoordsLeft (x - 1,y) (modificaCoordsLeft  (x,y) c)
                                   else c)
                             else c)
                       where 
					        modificaCoordsLeft :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
					        modificaCoordsLeft (z,k) ((x,y):t) = if z == x && k == y then ((x - 1,y):t) else (x,y):modificaCoordsUp (z,k) t 


reageEvento _ mapa = mapa -- ignora qualquer outro evento

reageTempo _ estado = estado

joga :: Estado -> IO ()
joga mapa = play
	(InWindow "Novo Jogo" (800, 600) (0, 0)) -- tamanho da janela do jogo
	(greyN 0.5) -- cor do fundo da janela
	45 -- refresh rate
	mapaInicial -- mapa inicial
	desenhaJogo -- funçao que desenha o mapa
	reageEvento -- reage a um evento
	reageTempo -- n˜ao reage ao passar do temp

















































validaUp :: (Int,Int) -> [String] -> [(Int,Int)] -> [(Int,Int)]
validaUp (x,y) m c = if (localizaCoords (x,y + 1) m) == ' ' || (localizaCoords (x,y + 1) m) == '.'
                   then modificaCoordsUp (x,y) c 
                   else (if (localizaCoords (x,y + 1) m) == 'H' || (localizaCoords (x,y + 1) m) == 'I'
                         then (if (localizaCoords (x,y + 2) m) == ' ' || (localizaCoords (x,y + 2) m) == '.'
                               then modificaCoordsUp (x,y + 1) (modificaCoordsUp  (x,y) c)
                               else c)
                         else c)



modificaCoordsUp :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
modificaCoordsUp (z,k) ((x,y):t) = if z == x && k == y then ((x,y + 1):t) else (x,y):modificaCoordsUp (z,k) t 




-- Valida movimento para Baixo
validaDown :: (Int,Int) -> [String] -> [(Int,Int)] -> [(Int,Int)]
validaDown (x,y) m c = if (localizaCoords (x,y - 1) m) == ' ' || (localizaCoords (x,y - 1) m) == '.'
                       then modificaCoordsDown (x,y) c 
                       else (if (localizaCoords (x,y - 1) m) == 'H' || (localizaCoords (x,y - 1) m) == 'I'
                             then (if (localizaCoords (x,y - 2) m) == ' ' || (localizaCoords (x,y - 2) m) == '.'
                                   then modificaCoordsDown (x,y - 1) (modificaCoordsDown  (x,y) c)
                                   else c)
                             else c)



modificaCoordsDown :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
modificaCoordsDown (z,k) ((x,y):t) = if z == x && k == y then ((x,y - 1):t) else (x,y):modificaCoordsDown (z,k) t 





-- Valida movimento para a Esquerda
validaLeft :: (Int,Int) -> [String] -> [(Int,Int)] -> [(Int,Int)]
validaLeft (x,y) m c = if (localizaCoords (x - 1,y) m) == ' ' || (localizaCoords (x - 1,y) m) == '.'
                       then modificaCoordsLeft (x,y) c 
                       else (if (localizaCoords (x - 1,y) m) == 'H' || (localizaCoords (x - 1,y) m) == 'I'
                             then (if (localizaCoords (x - 2,y) m) == ' ' || (localizaCoords (x - 2,y) m) == '.'
                                   then modificaCoordsLeft (x - 1,y) (modificaCoordsLeft  (x,y) c)
                                   else c)
                             else c)



modificaCoordsLeft :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
modificaCoordsLeft (z,k) ((x,y):t) = if z == x && k == y then ((x - 1,y):t) else (x,y):modificaCoordsUp (z,k) t 





-- Valida movimento para a Direita
validaRight :: (Int,Int) -> [String] -> [(Int,Int)] -> [(Int,Int)]
validaRight (x,y) m c = if (localizaCoords (x + 1,y) m) == ' ' || (localizaCoords (x + 1,y) m) == '.'
                   then modificaCoordsUp (x,y) c 
                   else (if (localizaCoords (x + 1,y) m) == 'H' || (localizaCoords (x + 1,y) m) == 'I'
                         then (if (localizaCoords (x + 2,y) m) == ' ' || (localizaCoords (x + 2,y) m) == '.'
                               then modificaCoordsUp (x + 1,y) (modificaCoordsUp  (x,y) c)
                               else c)
                         else c)



modificaCoordsRight :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
modificaCoordsRight (z,k) ((x,y):t) = if z == x && k == y then ((x + 1,y):t) else (x,y):modificaCoordsUp (z,k) t 
