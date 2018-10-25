module Main where

import Graphics.Gloss -- interface principal gloss
import Graphics.Gloss.Data.Picture -- para desenhar @Picture@s
import Graphics.Gloss.Interface.Pure.Game -- para reagir a @Event@s

-- | função principal que invoca o jogo.
main :: IO ()
main = do
    bola <- loadBMP "caixa.bmp" -- carrega a imagem da bola
    

    joga ((400,400),(200,200),bola) desenhaMapa reageEvento


-- | Estado do jogo:
--
-- * Dimensões do mapa
-- * Coordenadas da bola no mapa
-- * Imagem da bola
type Mapa = ((Float,Float),(Float,Float),Picture)

type Estado =((Int,Int),[String],[(Int,Int)])

-- | Move a bola uma coordenada para o lado
moveBola :: (Float,Float) -> Mapa -> Mapa
moveBola (x,y) ((xMapa,yMapa),(xBola,yBola),bola) = ((xMapa,yMapa),(arredonda xMapa (x + xBola),arredonda yMapa (y + yBola)),bola)
    where
    -- Evita que a  bola saia fora do mapa
    -- assume que a bola é um quadrado com arestas de comprimento 20
    arredonda limite p = max 10 (min p (limite-10))

-- | Desenha o jogo dentro da janela
desenhaMapa :: Mapa -> Picture
desenhaMapa ((xMapa,yMapa),(x,y),bola) = Pictures [borda,tabuleiro,figura]
    where
    -- borda do mapa a preto, centrada na janela
    borda = Translate (-(xMapa+20)/2) (-(yMapa+20)/2) $ Color black (Polygon [(0,0),(0,yMapa + 20),(xMapa + 20,yMapa + 20),(xMapa + 20,0)])
    -- mapa a branco, centrado na janela
    tabuleiro = Translate (-xMapa/2) (-yMapa/2) $ Color white (Polygon [(0,0),(0,yMapa),(xMapa,yMapa),(xMapa,0)])
    -- bola dentro do mapa do jogo
    figura = Translate (-xMapa/2) (-yMapa/2) $ Translate x y bola 

-- | Reage ao pressionar das setas do teclado, movendo a bola 5 pixéis numa direção
reageEvento :: Event -> Mapa -> Mapa
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) mapa = moveBola (0,5)  mapa
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) mapa = moveBola (0,-5) mapa
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) mapa = moveBola (-5,0) mapa
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) mapa = moveBola (5,0)  mapa
reageEvento _ mapa = mapa -- ignora qualquer outro evento

-- | Não reage ao passar do tempo.
reageTempo :: Float -> mundo -> mundo
reageTempo t m = m

-- | Função que cria um jogo.
joga :: mundo -> (mundo -> Picture) -> (Event -> mundo -> mundo) -> IO ()
joga mapaInicial desenha reage = play
    (InWindow "Bola" (800, 600) (0, 0)) -- Tamanho da janela do jogo
    (greyN 0.5) -- Côr do fundo da janela
    45 -- refresh rate
    mapaInicial -- mapa inicial
    desenha -- função que desenha o mapa
    reage -- função que reage a um evento (carregar numa tecla, mover o rato, etc)
    reageTempo -- função que reage ao passar do tempo 
