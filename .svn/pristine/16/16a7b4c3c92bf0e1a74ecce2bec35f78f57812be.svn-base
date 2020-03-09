module Tarefa0_2019li1g161 where

-- * Funções não-recursivas.

-- | Um ponto a duas dimensões dado num referencial cartesiado (distâncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
-- , ou num referencial polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
data Ponto = Cartesiano Double Double | Polar Double Angulo deriving Show


-- | Um ângulo em graus.
type Angulo = Double

-- ** Funções sobre vetores

-- | Um 'Vetor' na representação escalar é um 'Ponto' em relação à origem.
type Vetor = Ponto
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>

-- *** Funções gerais sobre 'Vetorwa

-- | Soma dois 'Vetor'es.

{-retiraValores :: Ponto -> (Double,Double)
retiraValores (Cartesiano x y) = (x,y)
retiraValores (Polar d a) = (d,a)


somaVetores :: Vetor -> Vetor -> Vetor
somaVetores p1 p2 = Cartesiano somax somay

            where
                  p1 = case p1 of (Cartesiano x1 y1) ->Cartesiano x1 y1
                                  (Polar d1 a1) -> Cartesiano (d1*cos a1) (d1* sin a1)
                  p2 = case p2 of (Cartesiano x2 y2) -> Cartesiano x2 y2
                                  (Polar d2 a2) -> Cartesiano (d2*cos a2) (d2* sin a2)

                  somax = fst(retiraValores p1) + fst(retiraValores p2)
                  somay = snd(retiraValores p1) + snd(retiraValores p2) -}

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar d a) =  (d * cos arad)
                           where
                                arad = a * pi / 180

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar d a) =  (d * sin arad)
                          where
                               arad = a * pi / 180
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores p1 p2 = Cartesiano (x1+x2)(y1+y2)
      where
            (x1,y1) = (posx p1, posy p1)
            (x2,y2) = (posx p2, posy p2)

-- | Converte Cartesiano em polar
-- Função adicionada para resolver problemas de quadrantes


-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = somaVetores (Cartesiano x1 y1) (Cartesiano (-x2) (-y2))
subtraiVetores (Polar d1 a1) (Polar d2 a2) = somaVetores (Polar d1 a1) (Polar (-d2) a2)
subtraiVetores (Cartesiano x y) (Polar d a) =  somaVetores (Cartesiano x y) (Polar (-d) a)
subtraiVetores (Polar d a) (Cartesiano x y) = somaVetores (Polar d a) (Cartesiano (-x) (-y))


-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor n (Cartesiano x y) = Cartesiano (n*x) (n*y)
multiplicaVetor n (Polar d a) = Polar (d * n) a

cartesiano_polar :: Vetor -> Vetor
cartesiano_polar (Cartesiano x y) = Polar (sqrt(x^2 + y^2)) (atan(y/x))

polar_cartesiano :: Vetor -> Vetor
polar_cartesiano (Polar d angulo) = Cartesiano (d * cos angulo) (d* sin angulo)
-- ** Funções sobre rectas.

-- | Um segmento de reta é definido por dois pontos.
type Reta = (Ponto,Ponto)

-- | Testar se dois segmentos de reta se intersetam.


-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http  ://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.

intersetam :: Reta -> Reta -> Bool
intersetam (p1,p2)(p3,p4) =  (t1 >= 0 && t1 <=1) && (t2 >= 0 && t2 <=1)
            where
                  (x1,y1) = (posx p1, posy p1)
                  (x2,y2) = (posx p2, posy p2)
                  (x3,y3) = (posx p3, posy p3)
                  (x4,y4) = (posx p4, posy p4)
                  t1 = ((y3-y4) * (x1-x3) + (x4-x3)*(y1-y3)) / d
                  t2 = ((y1-y2) * (x1-x3) + (x2-x1)*(y1-y3))/ d
                  d = (x4-x3)*(y1-y2)-(x1-x2)*(y4-y3)


-- | Calcular o ponto de intersecao entre dois segmentos de reta.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersecao :: Reta -> Reta -> Ponto
intersecao (p1,p2)(p3,p4) = somaVetores p1 multiplicacao
      where
            (x1,y1) = (posx p1, posy p1)
            (x2,y2) = (posx p2, posy p2)
            (x3,y3) = (posx p3, posy p3)
            (x4,y4) = (posx p4, posy p4)
            subtracao = subtraiVetores p2 p1 -- (p2-p1)
            multiplicacao = multiplicaVetor t1 subtracao -- t1(p2-p1)
            t1 = ((y3-y4) * (x1-x3) + (x4-x3)*(y1-y3))/d
            d = ((x4-x3)*(y1-y2))-((x1-x2)*(y4-y3))

             -- intersecao = p1+ t1(p2−p1) || intersacao = p3 + t2(p4-p3)

-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
--
-- __Sugestão:__ use a função 'length' que calcula tamanhos de listas
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido i l = i < length l && i >= 0

-- | A dimensão de um mapa dada como um par (/número de linhas/,/número de colunhas/).
type DimensaoMatriz = (Int,Int)

-- | Uma posição numa matriz dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
--
-- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
type PosicaoMatriz = (Int,Int)

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
--
-- __Sugestão:__ relembre a função 'length', referida anteriormente.
matrizValida :: (Eq a) => Matriz a -> Bool
matrizValida (h:t) | h == [] || length(h:t) <= 0 = False
                   | length(h:t) == 1 = True
                   | otherwise = matrizValida t

                   -- quando encontrarS uma vazia

dimensaoMatriz :: (Eq a) => Matriz a -> DimensaoMatriz
dimensaoMatriz [] = (0,0)
dimensaoMatriz  m      | matrizValida m == False = (0,0)
                       | otherwise = (length m, length (head m))




-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool
ePosicaoMatrizValida (l,c) m =  l < length m && c < length (head m)

-- * Funções recursivas.

normalizaAngulo :: Angulo -> Angulo
normalizaAngulo ang | ang < 0 = normalizaAngulo(ang + 360)
                    | ang > 360 = normalizaAngulo(ang - 360)
                    | otherwise = ang

-- | Devolve o elemento num dado índice de uma lista.
--
-- __Sugestão:__ Não use a função (!!) :: [a] -> Int -> a :-)
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista 0 (x:xs) = x
encontraIndiceLista i(x:xs) = encontraIndiceLista (i-1)(xs)

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista 0 valor (x:xs) = (valor:xs)
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista i valor (x:xs) = if eIndiceListaValido i (x:xs) then x:(atualizaIndiceLista (i-1) (valor) (xs))
                                     else []

--[10,20,30,40] atualiza indice 2

--atualizaIndiceLista 2 100 lista = 10: atualizaIndiceLista 1 100 [20,30,40]
                              --  = 10:20: atualizaIndiceLista 0 100 [30,40]
                              -- = 10:20:100:[40]

-- atualizaIndiceLista i elemento lista = if eIndiceListaValido i lista then (lista !! i) = elemento
                                    --   else lista
-- v paraelolos
-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.re
encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz (m,n) matriz = encontraIndiceLista n (encontraIndiceLista m matriz)

-- | Modifica um elemento numa dada 'Posicao'
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.


atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (0,c) valor (x:xs) = a:xs
                  where a = atualizaIndiceLista c valor x

atualizaPosicaoMatriz (l,c) valor (x:xs) = atualizaIndiceLista l b (x:xs)
                  where
                        linha = encontraIndiceLista l (x:xs)
                        b = atualizaIndiceLista c valor linha -- devolve a linha alterada
