{- |
== Introdução
Nesta tarefa foram implementadas diversas funções que conferem uma componente de inteligência artificial ao bot. Desta forma, o bot tem capacidade de efetuar jogadas que lhe permitem estar em vantagem em relação
aos outros jogadores, de forma e vencer.

== Objetivo
Começou-se por criar funções que permitiam ao bot verificar se a peça em que se encontra ou a seguinte têm cola. Nesse caso, o bot tem funcionalidades para se desviar, efetuando a jogada 'Movimenta' 'Cima' ou 'Movimenta' 'Baixo'. Contudo
é crucial verificar se essa jogada não será uma má alternativa. Por esse motivo, o bot verifica se tal jogada causaria a sua morte e, nesse caso, não a efetuará.Para decidir entre movimenta cima ou baixo, escolhe a peça com menor atrito. Se
o bot estiver localizado ou na pista mais a cima ou na pista mais abaixo é claro que não podera efetuar uma das mudanças de pistas. Assim, tentou-se com que o bot não despediçasse a oportunidade que tem em acelerar, em efetuar
uma jogada sem qualquer efeito no seu estado.

== Conclusao
Esta tarefa foi bastante interessante de se realizar. Contudo, o resultado poderia ter sido significativamente melhor se tivessemos dedicado mais tempo a esta tarefa em detrimento da tarefa 5.
-}
-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g161 where
import LI11920
import Tarefa2_2019li1g161
import Tarefa0_2019li1g161    

-- * Funções principais da Tarefa 6.
data Direction = Cima | Baixo | Todas | Nenhuma deriving (Eq, Show)

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot indice estado@(Estado mapa jogadores) | (not estaNaUltimaPeca) && (desvio /=  Nothing) = jogadaDeDesvio
                                          | otherwise = Just Acelera
        where
             desvio = determinaSeDesvia indice estado
             jog@(Jogador numpista distancia vel cola estadoJogador) = encontraIndiceLista indice jogadores 
             jogadaDeDesvio = Just (Movimenta (desvioParaJogada (fromJust desvio)))  
             estaNaUltimaPeca = ((ceiling distancia) == (length ( head mapa) )) 

             inclinacao :: Maybe Double
             inclinacao = case estadoJogador of (Ar _ i _) -> (Just i)
                                                (_) -> Nothing

-- | Função que converte um valor do tipo Direction para Direcao
desvioParaJogada :: Direction -> Direcao
desvioParaJogada direction | direction == Cima = C
                           | direction == Baixo = B

 -- | Funcao que determina se o Jogador se deve desviar.                          
determinaSeDesvia :: Int -> Estado -> Maybe Direction
determinaSeDesvia indice estado@(Estado mapa jogadores) 
     | ((pisoPecaSeguinte == Cola && ((fromIntegral (ceiling distancia)) - distancia < 0.5)) || (pisoPecaAtual == Cola)) && noChao = (determinaDesvio indice estado)
     | otherwise = Nothing
          where  
                 (Jogador numpista distancia vel cola estadoJogador) = encontraIndiceLista indice jogadores 
                 pista = mapa !! numpista    
                 pisoPecaAtual = obtemPiso (pista !! (floor (distancia)))
                 pisoPecaSeguinte = obtemPiso (pista !! (ceiling distancia))
                 noChao = case estadoJogador of (Chao _) -> True
                                                _ -> False
-- | Funcao que determina o desvio a efetuar pelo Jogador.
determinaDesvio :: Int -> Estado -> Maybe Direction
determinaDesvio indice estado@(Estado mapa jogadores)
      | dirPossivel == Todas = melhorOpcao
      | not (determinaSeMorto indice (jogada' C)) && dirPossivel == Cima = Just Cima -- e se a outra não tiver cola também
      | not (determinaSeMorto indice (jogada' B)) && (dirPossivel == Baixo || dirPossivel == Todas) = Just Baixo
      | otherwise = Nothing
                        where
                              dirPossivel = obtemDirPossivel jog mapa
                              jogada' dir = (jogada indice (Movimenta dir) estado)
                              (maybePecaCima,maybePecaBaixo) = determinaPecasAdjacentes jog mapa
                              jog = encontraIndiceLista indice jogadores 
                              melhorOpcao = determinaOpcao jog mapa

-- | Funcao que determina se o Jogador deve realizar movimenta cima ou movimenta baixo.
determinaOpcao :: Jogador -> Mapa -> Maybe Direction -- supondo que ele pode ir para cima ou para baixo
determinaOpcao  jog@(Jogador numpista distancia _ _ _) mapa | pisoPecaAtual == Boost = Nothing
                                                            | pisoPecaCima == Boost = (Just Cima)
                                                            | pisoPecaBaixo == Boost = (Just Baixo)
                                                            | otherwise = Nothing
          where
                (Just pecaCima,Just pecaBaixo) = determinaPecasAdjacentes jog mapa
                pisoPecaCima = obtemPiso pecaCima
                pisoPecaBaixo = obtemPiso pecaBaixo
                pista = mapa !! numpista   
                pecaAtual = pista !! (floor (fromIntegral numpista)) 
                pisoPecaAtual = obtemPiso pecaAtual

-- | Funcao que determina se o jogador está morto
determinaSeMorto :: Int -> Estado -> Bool
determinaSeMorto indice (Estado _ jogadores) = case estadoJogador of (Morto _) -> True
                                                                     _ -> False
        where
             (Jogador pista distancia vel cola estadoJogador) = encontraIndiceLista indice jogadores

-- | Função que determina as pecas adjacentes à atual, acima e abaixo.
determinaPecasAdjacentes :: Jogador -> Mapa -> (Maybe Peca,Maybe Peca)
determinaPecasAdjacentes jg@(Jogador numpista distancia _ _ _) mapa
    | npistas == 1 || (dirPossivel == Nenhuma)= (Nothing,Nothing)
    | dirPossivel == Baixo = (Nothing, Just pecaBaixo)
    | dirPossivel == Cima = (Just pecaCima,Nothing)
    | otherwise = (Just pecaCima,Just pecaBaixo) 
        where
             npistas = length mapa
             dirPossivel = obtemDirPossivel jg mapa
             pecaCima = obterPeca mapa (numpista-1) distancia
             pecaBaixo = obterPeca mapa (numpista +1) distancia 

-- | Função que devolve a  mudança de direção possível que o jogador pode realizar            
obtemDirPossivel :: Jogador -> Mapa -> Direction
obtemDirPossivel jogador mapa                          | npistas == 1 = Nenhuma
                                                       |npista == 0 = Baixo
                                                       | npista == (npistas - 1) =  Cima
                                                       | otherwise = Todas -- pode ir para cima ou para baixo
        where 
             (Jogador npista distancia vel cola estadoJogador) = jogador
             npistas = length mapa  
-- | Função que devolve o piso de uma 'Peca'
obtemPiso :: Peca -> Piso
obtemPiso (Rampa piso _ _ ) = piso 
obtemPiso (Recta piso _) = piso

-- | Função que converte um Just a para algo do tipo a.
fromJust :: Maybe a -> a
fromJust (Just e) = e

