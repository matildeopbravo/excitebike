-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g161 where

import LI11920
import Tarefa0_2019li1g161(atualizaIndiceLista, eIndiceListaValido, normalizaAngulo)
-- * Testes
-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0, Dispara, Estado mapa1 [jogador_a, jogador_c]),
           (0, Dispara, Estado mapa1 [Jogador 1 0.5 0 5 (Chao True)]),
           (0, Dispara, Estado mapa1 [Jogador 1 0 0 5 (Morto 1)]),
           (0, Dispara, Estado mapa1 [Jogador 1 0 0 5 (Ar 3 45 0)]),
           (0, Dispara, Estado mapa1 [Jogador 1 0 0 0 (Chao True)]),
           (0, Dispara, Estado mapa1 [Jogador 1 2.2 0 5 (Chao False)]),
           (0, Dispara, Estado mapa1 [Jogador 1 2.2 0 5 (Chao False)]),
           (0, Acelera, Estado mapa1 [jogador_a, jogador_c]),
           (0, Acelera, Estado mapa1 [Jogador 0 1.3 0 5 (Chao False), jogador_c]),
           (0, Acelera, Estado mapa1 [Jogador 0 1.3 0 5 (Morto 0.5)]),
           (0, Acelera, Estado mapa1 [Jogador 0 1.3 0 5 (Ar 3 45 0)]),
           (0, Desacelera, Estado mapa1 [Jogador 0 1.3 1 5 (Chao True)]),
           (0, Desacelera, Estado mapa1 [Jogador 0 1.3 1 5 (Chao False)]),
           (0, Desacelera, Estado mapa1 [Jogador 0 1.3 0 5 (Morto 0.5)]),
           (0, Desacelera, Estado mapa1 [Jogador 0 1.3 1 5 (Ar 3 45 0)]),
           (1, Movimenta B, Estado mapa1 [jogador_c, jogador_c]),
           (0, Movimenta B, Estado [[Recta Terra 0, Recta Relva 0, Rampa Lama 0 1], pista1] [jogador_d, jogador_c]),
           (0, Movimenta B, Estado mapa2 [Jogador 0 1.5 0 5 (Chao True), jogador_c]),
           (0, Movimenta B, Estado mapadifgrande [Jogador 0 1.9 0 5 (Chao True), jogador_c]),
           (1, Movimenta B, Estado ([[Recta Terra 0, Rampa Relva 0 2, Recta Relva 2],[Recta Terra 0, Recta Boost 0, Recta Boost 0]]) [Jogador 0 1.9 0 5 (Chao True), Jogador 1 2.5 0 5 (Chao False)]),
           (1, Movimenta B, Estado mapadifgrande [Jogador 0 1.9 0 5 (Chao True), Jogador 1 1.9 0 5 (Morto 0.9)]),
           (0, Movimenta B, Estado [[Recta Terra 0, Rampa Relva 0 5, Recta Lama 5 ], [Recta Terra 0, Recta Relva 0, Recta Lama 0 ]] [Jogador 0 2.5 0 5 (Chao True)]),
           (0, Movimenta C, Estado mapa1 [jogador_c, jogador_c]),
           (1, Movimenta C, Estado [[Recta Terra 0, Recta Relva 0, Rampa Lama 0 1],pista1] [jogador_d, jogador_c]),
           (0, Movimenta C, Estado [[Recta Terra 0, Recta Relva 0, Rampa Lama 0 1]] [jogador_d]),
           (1, Movimenta C, Estado mapa2 [Jogador 0 1.5 0 5 (Chao True), Jogador 1 1.5 0 5 (Chao False)]),
           (1, Movimenta C, Estado mapadifgrande [Jogador 0 1.9 0 5 (Chao True), Jogador 1 1.9 0 5 (Chao False)]),
           (1, Movimenta C, Estado mapadifgrande [Jogador 0 1.9 0 5 (Chao True), Jogador 1 1.9 0 5 (Morto 0.9)]),
           (0, Movimenta C, Estado [[Recta Terra 0, Recta Relva 0, Recta Lama 0 ],[Recta Terra 0, Rampa Relva 0 5, Recta Lama 5 ]] [Jogador 1 2.5 0 5 (Chao True)]),
           (1, Movimenta E, Estado mapa1 [jogador_c,jogador_b]),
           (1, Movimenta E, Estado mapa1 [jogador_c, Jogador 1 0 0 5 (Ar 1 80 0)]),
           (1, Movimenta D, Estado mapa1 [jogador_c, Jogador 1 0 0 5 (Ar 1 (-80) 0)]),
           (1, Movimenta D, Estado mapa1 [Jogador 0 1.3 0 5 (Morto 1), jogador_b]),
           (0, Movimenta D, Estado mapa1 [jogador_a,jogador_b])]

   where
       mapa1 = [[Recta Terra 0,Recta Relva 0,Rampa Boost 0 1], [Recta Terra 0,Rampa Relva 0 1, Rampa Lama 1 0]] -- mapa generico
       jogador_a = Jogador 0 1.3 0 5 (Chao True)
       jogador_c = Jogador 1 0 0 5 (Chao False)
       jogador_b = Jogador 1 0 0 5 (Ar 1 20 0) 
       jogador_d = Jogador 0 0 0 5 (Chao True)
       mapa2 = [[Recta Terra 0, Recta Relva 0, Rampa Lama 0 1],[Recta Terra 0, Rampa Relva 0 15, Rampa Lama 15 0]]
       pista1 = [Recta Terra 0, Rampa Relva 0 1, Rampa Lama 1 0]
       mapadifgrande = [[Recta Terra 0, Rampa Relva 0 15, Rampa Lama 15 1],pista1]
              
-- * Funções principais da Tarefa 2.
-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.

jogada jogadorIndex (Movimenta C) estado@(Estado mapaAnterior jogadoresAnt) = estado {jogadoresEstado = jogadoresNovo}
            where
              jogadoresNovo = atualizaIndiceLista jogadorIndex (mudaPista (jogadoresAnt!!jogadorIndex) (Movimenta C) mapaAnterior) jogadoresAnt

jogada jogadorIndex (Movimenta B) estado@(Estado mapaAnterior jogadoresAnt) = estado {jogadoresEstado = jogadoresNovo}
            where
                jogadoresNovo =  atualizaIndiceLista jogadorIndex (mudaPista (jogadoresAnt!!jogadorIndex) (Movimenta B) mapaAnterior) jogadoresAnt

jogada jogadorIndex (Movimenta E) estado@(Estado mapaAnterior jogadoresAnt) =  estado {jogadoresEstado = jogadoresNovo}
            where
              jogadoresNovo = atualizaIndiceLista jogadorIndex (rodaJogador (jogadoresAnt!!jogadorIndex) (Movimenta E)) jogadoresAnt

jogada jogadorIndex (Movimenta D) estado@(Estado mapaAnterior jogadoresAnt) = estado {jogadoresEstado = jogadoresNovo}
            where
              jogadoresNovo = atualizaIndiceLista jogadorIndex (rodaJogador (jogadoresAnt!!jogadorIndex) (Movimenta D)) jogadoresAnt

jogada jogadorIndex (Acelera) estado@(Estado mapaAnterior jogadoresAnt) = estado {jogadoresEstado = jogadoresNovo}
            where
                  jogadoresNovo = atualizaIndiceLista jogadorIndex (mudaAceleracaoJogador (jogadoresAnt !! jogadorIndex) Acelera) jogadoresAnt

jogada jogadorIndex (Desacelera) estado@(Estado mapaAnterior jogadoresAnt) = estado {jogadoresEstado = jogadoresNovo}
            where
                   jogadoresNovo = atualizaIndiceLista jogadorIndex (mudaAceleracaoJogador (jogadoresAnt !! jogadorIndex) Desacelera) jogadoresAnt

jogada jogadorIndex (Dispara) (Estado mapaAnterior jogadoresAnt) = Estado novoMapa jogadoresNovo
            where
                 jogador = jogadoresAnt !! jogadorIndex
                 (novoJogador, novoMapa) = dispara jogador mapaAnterior
                 jogadoresNovo = atualizaIndiceLista jogadorIndex novoJogador jogadoresAnt

-- | Altera a 'pistaJogador' de um dado 'Jogador', quando possível.
mudaPista :: Jogador -- ^ O 'Jogador' que efetuou a 'Jogada'.
          -> Jogada  -- ^ A 'Jogada' que o 'Jogador' efetuou.(Só pode ser Movimenta C ou Movimenta B)
          -> Mapa    -- ^ O 'Mapa' no momento em que o 'Jogador' efetua a 'Jogada'.
          -> Jogador -- ^ O 'Jogador' já com 'pistaJogador' alterada.


mudaPista jogador@Jogador{pistaJogador=npista, distanciaJogador = dist, estadoJogador = (Chao _)} (Movimenta C) mapa
      | npista == 0 = jogador
      | (alturaPecaNova - alturajogador) < (-0.2) = jogador{pistaJogador = (npista -1),estadoJogador = (Ar alturajogador inclinacaojogador 0)}
      | (alturaPecaNova - alturajogador) > 0.2 = jogador{estadoJogador = (Morto 1.0), velocidadeJogador = 0}
      | otherwise = jogador{pistaJogador = npista - 1}

        where
             alturaPecaNova = obterAlturaNaPeca (obterPeca mapa (npista - 1) dist) dist -- devolve a altura exata que o jogador vai passar a ter na peça da pista para o qual mudou. A peça nova é adjacente àquela onde se encontrava anteriormente, só que numa pista também adjacente.Será um valor entre a altura inicial e final, no caso de ser uma rampa.
             alturajogador = obterAlturaNaPeca (obterPeca mapa npista dist) dist -- devolve a altura exata a que o jogador se encontrava antes de mudar de pista
             inclinacaojogador = obterInclinacaoPeca (obterPeca mapa npista dist) -- inclinacao da peça onde o jogador se encontrava antes da mudança de pista.

mudaPista jogador@(Jogador {pistaJogador = npista, distanciaJogador = dist, estadoJogador = (Chao _)}) (Movimenta B) mapa
      | npista == (length mapa - 1) = jogador
      | alturaPecaNova - alturajogador < -0.2 = jogador {pistaJogador = (npista + 1),estadoJogador = (Ar alturajogador inclinacaojogador 0)}
      | alturaPecaNova - alturajogador > 0.2 = jogador {velocidadeJogador = 0, estadoJogador = (Morto 1.0)}
      | otherwise = jogador {pistaJogador = (npista + 1)}

        where
             alturaPecaNova = obterAlturaNaPeca (obterPeca mapa (npista + 1) dist) dist
             alturajogador = obterAlturaNaPeca (obterPeca mapa npista dist) dist
             inclinacaojogador = obterInclinacaoPeca (obterPeca mapa npista dist)

--Se o jogador não estiver no chão ou a jogada não for para cima ou para baixo esta função não terá efeito
mudaPista jogador jogadao mapa = jogador


-- | Altera a inclinação do jogador apenas quando este estiver no ar.
rodaJogador :: Jogador -- ^ O 'Jogador' cuja inclinação vai ser alterada.
            -> Jogada  -- ^ A 'Jogada' que o 'Jogador' efetuou.(Só pode ser Movimenta E ou Movimenta D).
            -> Jogador -- ^ O 'Jogador' com a inclinacao alterada.

rodaJogador jogador@Jogador{estadoJogador = ar@Ar {inclinacaoJogador = inclinacao}} (Movimenta E)
      | incNorm + 15 >= 90 =  jogador{estadoJogador = ar {inclinacaoJogador = 90}}
      | otherwise = jogador{estadoJogador = ar {inclinacaoJogador = incNorm + 15}}
      where
        incNorm | normalizaAngulo inclinacao > 90  = normalizaAngulo inclinacao - 360
                | otherwise = normalizaAngulo inclinacao

rodaJogador jogador@Jogador{estadoJogador = ar@Ar {inclinacaoJogador = inclinacao}} (Movimenta D)
      | incNorm - 15 <= (-90) = jogador{estadoJogador = ar {inclinacaoJogador = (-90)}}
      | otherwise = jogador{estadoJogador = ar {inclinacaoJogador = incNorm - 15}}
      where
        incNorm | normalizaAngulo inclinacao > 90  = normalizaAngulo inclinacao - 360
                | otherwise = normalizaAngulo inclinacao

rodaJogador jogador _ = jogador

-- O angulo varia entre -90 e 90. Cada vez que o jogador quer aumentar ou diminuir a sua inclinação,
-- esta varia  em 15 graus, exceto nos casos em que ao fazer essa variação obter-se-ia um angulo
-- que não estivesse entre -90 e 90.

-- | Devolve a altura exata a que um 'Jogador' se encontra na 'Peca'.
obterAlturaNaPeca :: Peca -- ^ A 'Peca' onde o 'Jogador' se encontra
                  -> Double -- ^ Distância do 'Jogador' ao início da 'Pista'.
                  -> Double -- ^ Altura exata na 'Peca'
obterAlturaNaPeca (Rampa _ altInicial altFinal) distInicio = fromIntegral (altInicial) + (distPeca * alturaRampa)

        where
              distPeca = distInicio - fromIntegral(floor distInicio) -- devolve a distancia horizontal desde o inicio da peça até à posicao em que o jogador se encontra nessa peça.
              alturaRampa = fromIntegral(altFinal - altInicial)

obterAlturaNaPeca (Recta _ alt) _ = fromIntegral alt

-- | Devolve a 'Peca' que se encontra a uma dada distância do início de uma determinada 'Pista'.
obterPeca :: Mapa-> Int -> Double -> Peca              --- Devolve a peca em que o jogador se encontra, dado mapa, o índice da pista em que se encontra e a sua distancia ao inicio.
obterPeca mapa npista distInicio =  (mapa !! npista) !! floor distInicio

-- | Devolve a inclinação de uma 'Peca'.
obterInclinacaoPeca :: Peca -> Double                  -- recebe uma peca e devolve a inclinacao dela
obterInclinacaoPeca (Rampa _ altInicial altFinal) = atan (fromIntegral(altFinal - altInicial))  * 180 / pi -- devolve a inclinacao da peca dada
obterInclinacaoPeca (Recta {}) = 0

-- | Muda o 'estadoJogador' para que o 'Jogador' passe a acelerar ou a não acelerar, conforme a sua 'Jogada'. Não ocorrem alterações se o 'Jogador' estiver 'Morto' ou no 'Ar'.
mudaAceleracaoJogador :: Jogador -> Jogada -> Jogador
mudaAceleracaoJogador jogador@Jogador{estadoJogador = Morto timer} _ = jogador -- se o jogador estiver morto, não podera mudar a sua aceleracao e por isso devolve o proprio Jogador
mudaAceleracaoJogador jogador@Jogador{estadoJogador = Ar {} } _ = jogador -- se o jogadador estive no ar, nao podera mudar a sua aceleracao e por isso devolve o proprio jogador
mudaAceleracaoJogador jogador Acelera = jogador{estadoJogador = (Chao True)} -- se o jogador acelerar, o estadoJogador desse jogador passará  a ser chao True. Se o jogador desacelerar passará a ser chao False.
mudaAceleracaoJogador jogador Desacelera = jogador{estadoJogador = (Chao False)}

-- | Altera o 'Piso' da 'Peca' que se encontra imediatamente antes de um  dado 'Jogador' para 'Cola', tendo em conta a munição e a distancia do 'Jogador'.
dispara :: Jogador -> Mapa-> (Jogador,Mapa)
dispara jogadorInicial@(Jogador{ colaJogador = cola, pistaJogador = npista, estadoJogador=estado, distanciaJogador = dist}) mapa

      | ((estado == Chao True) || (estado == Chao False)) && cola > 0
   && dist >= 1 = (jogadorInicial{colaJogador = cola - 1},novoMapa)
      | otherwise = (jogadorInicial,mapa)

      where
            indicePecaAlterar = (floor dist) - 1  -- indice da peça anterior à  que o jogador se encontra
            pecaaAlterar = obterPeca mapa npista (dist-1)  --devolve a peça anterior à que o jogador se encontra
            pecaAlterada = mudaPisoPeca pecaaAlterar Cola   -- devolve a pecaAalterar alterada, isto é, com o piso alterado para cola.
            novaPista = atualizaIndiceLista indicePecaAlterar pecaAlterada (mapa!!npista) -- devolva a pista onde o jogador se encontra, já com as alteracoes no piso da pecaaAlterar
            novoMapa =  atualizaIndiceLista npista novaPista mapa -- devolve o Mapa todo já com a alteracao do piso na pecaaAlterar

-- | Muda o 'Piso' de uma 'Peca' para outro 'Piso' dado.
mudaPisoPeca :: Peca -> Piso -> Peca
mudaPisoPeca (Recta pisoAnterior altura) novoPiso = (Recta novoPiso altura)
mudaPisoPeca (Rampa pisoAnterior altInicial altFinal) novoPiso = (Rampa novoPiso altInicial altFinal)
