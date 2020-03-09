-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g161 where

import LI11920
import Tarefa2_2019li1g161
import Tarefa0_2019li1g161

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [
  (2, m, (Jogador 0 0.1 0.3 0 (Chao True))),
  (2, m, (Jogador 0 0.1 0.2 0 (Chao True))),
  (2, m, (Jogador 0 1.2 1.5 0 (Chao False))),
  (2, m, (Jogador 0 2.1 0 0 (Morto 0.5))),
  (2, m, (Jogador 0 1.1 0 0 (Ar 2 (-90) 1))),
  (2, m, (Jogador 0 1.1 0.1 0 (Ar 2 45 1))),
  (2, m, (Jogador 0 1.5 1 0 (Ar 7 0 0))),
  (1, m, (Jogador 0 1.1 0.1 0 (Chao True))),
  (1, m, (Jogador 0 1.1 0.5 0 (Ar 5 0 0))),
  (3, m, (Jogador 0 2.1 0.2 0 (Ar 5 0 2))),
  (1, m, (Jogador 0 2.1 0.2 0 (Chao True))),
  (2, m, (Jogador 0 1.2 0.2 0 (Ar 3 30 1))),
  (2, m, (Jogador 0 1.1 0.2 0 (Ar 3.1 21 1))),
  (2, m, (Jogador 0 1.1 0.2 0 (Ar 3.1 85 1))),
  (2, m, (Jogador 0 2.1 0.2 0 (Ar 3.1 (-21) 1))),
  (2, m, (Jogador 0 2.2 0.2 0 (Ar 3 (-90) 1))),
  (1, m, (Jogador 0 2.2 0.4 0 (Ar 3 (-30) 2)))]
  where
     m = [[(Recta Terra 0), (Rampa Terra 0 2), (Rampa Terra 2 0)]]

-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.

acelera tempoDecorrido mapa jogadorAnterior@(Jogador{estadoJogador = Chao aceleraJogador}) = jogadorAnterior{velocidadeJogador = velocidadeFinalPositiva}

               where
                    (Jogador pista distanciaAnt velocidadeAnt _ estadoJogadorAnt) = jogadorAnterior
                    velocidadeFinal = velocidadeAnt + (accelMota - atrito * velocidadeAnt) * tempoDecorrido
                    velocidadeFinalPositiva = max velocidadeFinal 0
                    pecaAtual = obterPeca mapa pista distanciaAnt
                    pisoPecaAtual = obtemPiso pecaAtual
                    atrito = devolveAtrito pisoPecaAtual
                    accelMota | velocidadeAnt < 2 && aceleraJogador = 1
                              | otherwise = 0

acelera tempoDecorrido mapa jogador@Jogador{velocidadeJogador = velocidadeAnt, estadoJogador = (ar@Ar {gravidadeJogador = gravidadeJogadorAnt})} = 
     jogador{velocidadeJogador = velocidadeFinalPositiva, estadoJogador = ar{gravidadeJogador = novaGravidade}}

                         
   where
        velocidadeFinal = velocidadeAnt - (0.125 * velocidadeAnt * tempoDecorrido)
        velocidadeFinalPositiva = max velocidadeFinal 0
        novaGravidade = gravidadeJogadorAnt + tempoDecorrido

acelera _ _ jogador = jogador

obtemPiso :: Peca -> Piso
obtemPiso (Recta piso _) = piso
obtemPiso (Rampa piso _ _) = piso

devolveAtrito :: Piso -> Double
devolveAtrito piso | piso == Terra = 0.25
                   | piso == Relva = 0.75
                   | piso == Lama = 1.50
                   | piso == Boost =  -0.50
                   | otherwise = 3


-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t _ jogador@(Jogador {estadoJogador = (Morto timeout)})
  | timeout - t <= 0 = jogador {estadoJogador = (Chao False)}
  | otherwise = jogador {estadoJogador = (Morto (timeout - t))}

move t mapa (Jogador pista dist veloc cola (Ar h i g))
  | intersetam retaPeca retaTrajetoria && difInclinacao > 45 && not (fromIntegral (floor dist) == dist) = Jogador pista novaDistCol 0 cola (Morto 1)
  | intersetam retaPeca retaTrajetoria && not (fromIntegral (floor dist) == dist) = Jogador pista novaDistCol velocParalela cola (Chao False)
  | tempoAtePonta < t = Jogador pista (normalizaDist (dist + trajX) dist) veloc cola (Ar (h + (trajY / t * tempoAtePonta)) i g)
  | otherwise  = Jogador pista (normalizaDist (dist + trajX) dist) veloc cola (Ar (h + trajY) i g)
  where
     pecaAbaixo = (mapa!!pista)!!(floor dist)
     retaPeca = pecaParaReta pecaAbaixo
     distNaPeca = dist - (fromIntegral (floor dist))
     velX = veloc * (cos (i * pi / 180))
     trajX = velX * t
     trajY = - (g - (veloc * (sin (i * pi / 180)))) * t
     tempoAtePonta = (1 - distNaPeca) / velX
     retaTrajetoria = ((Cartesiano distNaPeca h), (Cartesiano (distNaPeca + trajX) (h + trajY)))
     pontoColisao = intersecao retaTrajetoria retaPeca
     difInclinacao = abs (i - (obterInclinacaoPeca pecaAbaixo))
     novaDistCol = (fromIntegral (floor dist)) + (posx pontoColisao)
     velocParalela = veloc * (cos ((pi * (abs (obterInclinacaoPeca pecaAbaixo)) / 180) - (pi * (abs i) / 180)))
     --gravParalela = g * (cos ((abs (obterInclinacaoPeca pecaAbaixo)) - (abs i)))

move t mapa (Jogador pista dist veloc cola est) 
  | novaDistNorm /= novaDist && inclinacaoPecaAbaixo > inclinacaoPecaSeguinte = Jogador pista novaDistNorm veloc cola (Ar (altFinalPeca pecaAbaixo) inclinacaoPecaAbaixo 0)
  | otherwise = Jogador pista novaDistNorm veloc cola est 

  where
     pistaAtual = mapa!!pista
     pecaAbaixo = pistaAtual!!(floor dist)
     inclinacaoPecaAbaixo = obterInclinacaoPeca pecaAbaixo
     inclinacaoPecaSeguinte = obterInclinacaoPeca (pistaAtual!!(floor dist + 1))
     novaDist = dist + (veloc * (cos (inclinacaoPecaAbaixo * pi / 180)) * t)
     novaDistNorm = normalizaDist novaDist dist


altFinalPeca :: Peca -> Double
altFinalPeca (Recta _ h) = fromIntegral h
altFinalPeca (Rampa _ _ h) = fromIntegral h


pecaParaReta :: Peca -> Reta
pecaParaReta (Recta _ h) = ((Cartesiano 0 (fromIntegral h)), (Cartesiano 1 (fromIntegral h)))
pecaParaReta (Rampa _ h0 h1) = ((Cartesiano 0 (fromIntegral h0)), (Cartesiano 1 (fromIntegral h1)))


normalizaDist :: Double -> Double -> Double
normalizaDist novaDist dist
  | novaDist > (fromIntegral (floor dist)) + 1 = fromIntegral (floor dist) + 1
  | otherwise = novaDist