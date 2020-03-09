-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g161 where
import LI11920
import System.Random

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(x,y,z) | x <- [0..5], y <- [1..15], z <- [0..3]]

-- * Funções pré-definidas da Tarefa 1.

-- | Gera números aleatórios dada uma semente de aleatoriedade e quantidade de números a gerar.
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.

-- | Cria um 'Mapa' dado o número de 'Pista's, comprimento e semente de aleatoriedade.
gera :: Int -- ^ Número de 'Pista's a gerar.
     -> Int -- ^ Número de 'Peca's por 'Pista' / Comprimento de cada 'Pista'.
     -> Int -- ^ Número que a função 'geraAleatorios' usará para criar números aleatórios (semente de aleatoriedade).
     -> Mapa -- ^ O 'Mapa' que a função gera a partir dos parâmetros fornecidos.

gera npistas comprimento semente | npistas <= 0 || comprimento <= 0 = []
                                 | otherwise = (gera (npistas-1) comprimento semente) ++
                                               [geraPista [Recta Terra 0] comprimento segmentoRan]
                                  where
                                        ranporpista = (comprimento - 1) * 2
                                        ranagerar = ranporpista * npistas
                                        listaderan = geraAleatorios ranagerar semente
                                        segmentoRan = segmentoLista (npistas - 1) ranporpista listaderan -- lista que contem os aleatórios necessários para gerar uma npista
-- | Gera uma pista dado um comprimento e uma lista de números aleatórios.
geraPista :: Pista -- ^ Pista inicial que vai ser desenvolvida por recursividade. É sempre iniciada com ['Recta' 'Terra' 0].
          -> Int -- ^ O número de 'Peca's que a pista final deve ter.
          -> [Int] -- ^ A lista de todos os números aleatórios que vão ser usados nesta pista.
          -> Pista -- ^ A pista alterada.
geraPista pistaagerar comprimento listaran
                                      | length pistaagerar == comprimento = pistaagerar
                                      | otherwise = geraPista novaPista comprimento listaran
                                      
                                      where
                                          ranPiso = listaran!!(((length pistaagerar) * 2) - 2)
                                          ranTipo = listaran!!(((length pistaagerar) * 2) - 1)
                                          pecanova = geraPeca (last pistaagerar) (ranPiso, ranTipo)
                                          novaPista =  pistaagerar ++ [pecanova]

-- | Gera uma nova 'Peca' a partir do 'Piso' e altura da 'Peca' anterior e dois números aleatórios.
-- O primeiro número aleatório condiciona o 'Piso' da 'Peca' e o segundo condiciona o tipo de 'Peca' (Rampa, Recta).
-- 
-- prop> geraPeca (Rampa Terra 0 1) (6,1) == Rampa Terra 1 3
-- prop> geraPeca (Recta Boost 0) (2,3) == Recta Relva 0
geraPeca :: Peca -- ^ 'Peca' considerada como anterior à 'Peca' a ser gerada.
         -> (Int,Int) -- ^ Par de números aleatórios que determinam propriedades da 'Peca'.
         -> Peca -- ^ 'Peca' gerada pela função.
         
geraPeca pecaAnterior (ranPiso,ranTipo) 
                          |ranTipo <= 1 = (Rampa pisoGerado altFinalAnt (altFinalAnt + ranTipo + 1)) --rampa a subir
                          |(ranTipo > 1 && ranTipo <= 5 && altFinalAnt == 0)  || ranTipo > 5 = Recta pisoGerado altFinalAnt
                          |otherwise = Rampa pisoGerado altFinalAnt (normalizaAltura (altFinalAnt - (ranTipo - 1))) 
                    
                      where
                            (pisoAnterior,altFinalAnt) = devolvePisoeAltFinal pecaAnterior
                            pisoGerado = geraPiso pisoAnterior ranPiso
-- | Devolve o 'Piso' e a altura final de uma dada 'Peca'.
devolvePisoeAltFinal (Rampa pisoAnt _ altFinalAnt) = (pisoAnt, altFinalAnt)
devolvePisoeAltFinal (Recta pisoAnt altFinalAnt) = (pisoAnt, altFinalAnt)
-- | Devolve um 'Piso' correspondente a um dado inteiro e o 'Piso' da 'Peca' anterior de acordo com a tabela fornecida no enunciado.
--
-- prop> geraPiso Terra 6 == Terra
-- prop> geraPiso Relva 5 == Boost
geraPiso :: Piso -- > 'Piso' considerado como anterior ao 'Piso' a ser gerado
         -> Int -- > Número aleatório que determina  o 'Piso' a ser gerado
         -> Piso -- > 'Piso' gerado
geraPiso pisoAnt ran |ran <= 1 = Terra
                     |ran > 1 && ran <= 3 = Relva
                     |ran == 4 = Lama
                     |ran == 5 = Boost
                     |otherwise = pisoAnt

-- * Pequenas funções auxiliares

-- | Converte qualquer inteiro negativo em 0, de forma a impedir alturas de 'Peca's negativas.
--
-- prop> normalizaAltura 1 == 1
-- prop> normalizaAltura (-1) == 0
-- prop> normalizaAltura 0 == 0
normalizaAltura :: Int -> Int
normalizaAltura x | x < 0 = 0
                  | otherwise = x

-- | Devolve uma lista dividida em segmentos de um dado tamanho
--
-- prop> chunksOf 3 [1,2,3,4,5,6] == [[1,2,3],[4,5,6]]
chunksOf' :: Int -> [a] -> [[a]]
chunksOf' n [] = []
chunksOf' n l = (take n l):(chunksOf' n (drop n l))

-- | Devolve o segmento pretendido de uma lista.
--
-- prop> segmentoLista 1 3 [1,2,3,4,5,6,7,8,9] == [4,5,6]

segmentoLista :: Int -- ^ Índice do segmento pretendido
              -> Int -- ^ Tamanho de cada segmento
              -> [a] -- ^ Lista a dividir
              -> [a] -- ^ Segmento da lista pretendido
segmentoLista segmento tamanhosegmento lista = (chunksOf' tamanhosegmento lista) !! segmento
       
                     