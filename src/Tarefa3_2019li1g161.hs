{- |
== Introdução
Nesta tarefa foram implementados diversos algoritmos, de modo a atingir o maior nível de compressão de instruções possível. Esse objetivo foi alcançado sem que nenhum 
tipo de informação fosse perdido e de forma a que as instruções permanecessem válidas.

==Objetivos

De modo a comprimir as instruções dadas a um conjunto de Bulldozer, procurou-se implementar diversos algoritmos de otimização. 

 - Começou-se por desenvolver uma otimização a nível dos padrões horizontais que verificava se existiam pecas repetidas adjacentes na mesma pista. Porém, decidiu-se aprimorar ainda mais esse algoritmo, introduzindo o conceito de 
"padrões horizontais infinitos". Este algoritmo consiste, não apenas na verificação de peças repetidas seguidas, mas também, na descoberta de padrões de conjuntos de pecas
diferentes mas que se repetem a nível da mesma pista.
 - Implementou-se também o algoritmo dos padrões verticais que compara peças adjacentes de pistas diferentes e procura
dar instruções a mais do que uma bulldozer em simultâneo. 
 - Por último, foi desenvolvido o algoritmo dos padrões desfasados que utiliza, nomeadamente, o teleporta.
Após uma série de experiências, chegou-se à conclusão de que o "teleporta" apenas compensa ser utilizado em casos muitos específicos. Para isso foram utilizadas diversas funções que determinam
se o nível de otimização atingido, ao aplicar este algoritmo, é compensativo. 
 
== Conclusão
Consideramos que os resultados obtidos foram bastante satisfatórios e que foi atingido um elevado grau de compressão, como era pedido. Surpreendentemente, verificou-se que o algoritmo que fez aumentar
significativamente a taxa de compressão foi o algoritmo dos padrões horizontais "infinitos". De forma a obter este nível de compressão, a qualidade do código foi um pouco sacrificada, pelo que
temos consciência que esse parâmetro poderia ter estado melhor. Contudo, consideramos que o mais importante objetivo, a compressão, foi alcançado.

-}   
   
module Tarefa3_2019li1g161 where
import LI11920
import Data.List

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [m1,m2,m3,m4,m5,m6]
    where
         m1 = [[Recta Terra 0, Recta Terra 0, Recta Terra 0, Recta Relva 0,Recta Relva 0,Recta Terra 0],[Recta Terra 0, Recta Terra 0, Recta Terra 0, Recta Relva 0 , Recta Relva 0,Recta Terra 0]]
         m2 =  [[Recta Terra 0, Recta Relva 0, Recta Relva 0, Recta Relva 0, Recta Relva 0]]
         m3 =  [[Recta Terra 0, Rampa Boost 0 1, Rampa Boost 1 0, Recta Relva 0, Recta Lama 0],[Recta Terra 0, Rampa Relva 0 1, Rampa Relva 1 0, Recta Lama 0, Recta Lama 0 ],[Recta Terra 0, Rampa Terra 0 2, Rampa Boost 2 0, Recta Boost 0, Recta Lama 0]]
         m4 = [[Recta Terra 0,Recta Terra 0, Recta Terra 0]]
         m5 = [[Recta Terra 0, Recta Terra 0, Recta Relva 0, Recta Relva 0, Recta Relva 0,Recta Terra 0, Recta Terra 0,Recta Terra 0]]
         m6 = [[Recta Terra 0, Recta Relva 0, Rampa Boost 0 1, Rampa Boost 1 0, Recta Relva 0 , Rampa Boost 0 1, Rampa Boost 1 0]]
-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções   '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.

desconstroi :: Mapa -> Instrucoes
desconstroi mapa = menorInstrucao

    where
        mapaEmInstrucaoPorPista = mapaParaInstrucaoPorPista mapa 0
        vertical = otimizaVerticalmente mapaEmInstrucaoPorPista
        composta = otimizaVerticalmente(reaplicaPadroesHorizontaisMatriz mapaEmInstrucaoPorPista)
        horizontal = reaplicaPadroesHorizontaisMatriz mapaEmInstrucaoPorPista
        npistas = length mapa
        desfasadoOtimizado = otimizaMapaDesfasado mapaEmInstrucaoPorPista npistas [0]
        verticalDesfasado= otimizaVerticalmente (desfasadoOtimizado)
        horizontalDesfasado= reaplicaPadroesHorizontaisMatriz(desfasadoOtimizado)
        verticalHorizontalDesfasado = otimizaVerticalmente(reaplicaPadroesHorizontaisMatriz desfasadoOtimizado) 
        menorInstrucao = minimoDeInstrucoes [vertical, concat(horizontal), composta,concat(desfasadoOtimizado), concat(horizontalDesfasado),verticalDesfasado, verticalHorizontalDesfasado]

-- | Dada uma matriz de 'Instrucoes', devolve a lista com o menor número de 'Instrucoes'.       
minimoDeInstrucoes :: [Instrucoes] -> Instrucoes
minimoDeInstrucoes [instrucao] = instrucao 
minimoDeInstrucoes (lista:t) | tamanhoInstrucoes (lista) <= tamanhoInstrucoes(minimoDeInstrucoes t)  = lista   
                             | otherwise = minimoDeInstrucoes t
-- | Verifica se a 'Pista' reune as condições necessárias para uma eficaz aplicação da função 'otimizaPistaDesfasado'.            
verificaSePistaValida :: Pista -> Int -> Bool -- valida para aplicacao do desfazado
verificaSePistaValida pista indiceBulldozer = (numeroPecasDiferentes == 2) && (verificaSeSeguidas pista) && (not(verificaSeExtremo pistaEmInstrucoes (pecaPrincipal,pecaAalterar) indiceBulldozer)) 
            where
                  pistaSemRetaTerra0 = tail pista -- tirando a reta terra 0
                  pecasDiferentes = devolvePecasDiferentes pista
                  (pecaPrincipal,pecaAalterar) = determinaPecasAlterar pista pecasDiferentes
                  numeroPecasDiferentes = length (nub(pistaSemRetaTerra0))
                  pistaEmInstrucoes = pistaParaInstrucao pista indiceBulldozer

-- | Verifica se as 'Pecas' que ocorrem em menor número, para as quais a bulldozer se vai teleportar, são todas adjacentes umas às outras, isto é, se estão seguidas.
--
--prop> verificaSeSeguidas [Anda[0] Terra, Anda [0] Relva, Anda [0] Relva, Anda [0] Terra, Anda [0] Terra] == True                  
verificaSeSeguidas :: Pista -> Bool
verificaSeSeguidas pista = apenasUmVerdade listaBooleanos 0
        where
            listaBooleanos = (map ((elem) pecaAalterar) pistaAgrupada )
            pistaAgrupada = group pista
            pecasDiferentes = devolvePecasDiferentes pista
            (pecaPrincipal,pecaAalterar) = determinaPecasAlterar pista pecasDiferentes
            apenasUmVerdade :: [Bool] -> Int -> Bool
            apenasUmVerdade [] i = True
            apenasUmVerdade (booleano:booleanos) i  | i > 1 = False
                                                    | booleano == True = apenasUmVerdade booleanos (i+1)
                                                    | otherwise =  apenasUmVerdade booleanos i

-- | Aplica um algoritmo de otimização a cada uma das pistas de um mapa(se pretinente), recorrendo à 'Instrucao' 'Teleporta'.
otimizaMapaDesfasado :: [Instrucoes] -> Int -> [Int] -> [Instrucoes]
otimizaMapaDesfasado [] _ _ = []
otimizaMapaDesfasado instrucoeS@(instrucaopista0:instrucoes) npista [i] | i == npista = []
                                                                        | otherwise =(otimizaPistaDesfasado instrucaopista0 i):(otimizaMapaDesfasado instrucoes npista [i+1]) 
            
                 
-- | Aplica um algoritmo de otimização a uma pista, recorrendo à 'Instrucao' 'Teleporta'.
otimizaPistaDesfasado :: Instrucoes -> Int -> Instrucoes
otimizaPistaDesfasado [] _ = []
otimizaPistaDesfasado [instrucao1] _ = [instrucao1]
otimizaPistaDesfasado (listaInstrucoes@(instrucao1:instrucao2:instrucoes)) indiceBulldozer 
     | not(verificaSePistaValida pista indiceBulldozer) = listaInstrucoes
     | ocorrenciaspecaAalterar > 2 = (Repete comprimentoPistaSemTerra0 [instrucaoPrincipal]):(Teleporta [indiceBulldozer] unidadesTeleporta ):[Repete ocorrenciaspecaAalterar [instrucaoOtimizar]]
     | otherwise = (Repete comprimentoPistaSemTerra0 [instrucaoPrincipal]):(Teleporta [indiceBulldozer] unidadesTeleporta):(replicate ocorrenciaspecaAalterar instrucaoOtimizar)

            where
                    pista = last(instrucoesParaMapa listaInstrucoes)
                    pecasDiferentes = devolvePecasDiferentes pista
                    comprimentoPistaSemTerra0 = (length pista) - 1
                    (pecaPrincipal,pecaAalterar) = determinaPecasAlterar pista pecasDiferentes
                    instrucaoPrincipal = pecaParaInstrucao pecaPrincipal indiceBulldozer           
                    instrucaoOtimizar = pecaParaInstrucao pecaAalterar indiceBulldozer                                                  
                    ocorrenciaspecaAalterar = contaOcorrencias instrucaoOtimizar listaInstrucoes
                    unidadesTeleporta = unidadesATeleportar listaInstrucoes instrucaoOtimizar 0
                  
                    -- recebe uma pista apenas com dois tipos de peças diferente           
                    -- aplica horizontal nj
-- | Determina a quantidade de unidades que a Bulldozer se deve teleportar, de modo a deslocar-se para o começo do segmento de 'Peca' a alterar.
unidadesATeleportar:: Instrucoes -> Instrucao -> Int -> Int
unidadesATeleportar [] _ _ = 0
unidadesATeleportar instrucoes instrucaoOtimizar unidades 
                                              | (instrucaoOtimizar `elem` ultimasInstrucoes) =  (unidades - comprimento1 ) 
                                              | otherwise = unidadesATeleportar (instrucoesSemSegmento) instrucaoOtimizar (-comprimento1)
        where
             instrucoesAgrupadas = group instrucoes
             ultimasInstrucoes = last instrucoesAgrupadas
             comprimento1 = length ultimasInstrucoes
             instrucoesSemSegmento = concat(init(instrucoesAgrupadas))

-- | Dada uma 'Pista' que apenas contém dois tipos de 'Pecas' diferentes, devolve essas duas 'Pecas'.    
--
--prop> devolvePecasDiferentes[Recta Terra 0, Recta Terra 0, Recta Relva 0] == (Recta Terra 0, Recta Relva 0)               
devolvePecasDiferentes :: Pista -> (Peca,Peca) 
devolvePecasDiferentes pista@(pecaA:pecaB:pecas) =  (pecaA,peca2)
       where
            indicepeca2 = posPrimeiraOcorrencia pista 0
            peca2 = pista !! indicepeca2
-- | Determina qual a 'Peca' que ocorre mais e a que occorre menos numa pista.
determinaPecasAlterar :: Pista -- ^ Pista que reune as condições necessárias à aplicação do 'Teleporta'.
                       -> (Peca,Peca) -- ^ As únicas duas 'Peca's que ocorrem na 'Pista'.
                       -> (Peca,Peca) -- ^ São respetivamente a 'Peca' que ocorre mais e a 'Peca' que ocorre menos. --> (pecaPrincipal, pecaAalterar)
determinaPecasAlterar pista (peca1,peca2) | ocorrenciasPeca1 > ocorrenciasPeca2 = (peca1,peca2)
                                           | otherwise = (peca2,peca1)
        where
             ocorrenciasPeca1 = contaOcorrencias peca1 pista
             ocorrenciasPeca2 = contaOcorrencias peca2 pista 
-- | Verifica se alguma das 'Peca' que ocorre menos na 'Pista' se localiza nos extremos da pista
--
--prop> verificaSeExtremo  [Anda[0] Relva, Anda[0] Terra, Anda [0] Terra] == True
verificaSeExtremo :: Instrucoes -> (Peca,Peca) -> Int -> Bool --- recebe instrucoes relativas a uma pista
verificaSeExtremo instrucoes (pecaPrincipal,pecaAalterar) indiceBulldozer = head instrucoes == instrucaoOtimizar || last instrucoes == instrucaoOtimizar 
        where
            instrucaoOtimizar = pecaParaInstrucao pecaAalterar indiceBulldozer
  

             -- a partir de que compimrneo
-- | Devolve a posição da primeira ocorrência de uma dada 'Peca' numa 'Pista'.
posPrimeiraOcorrencia :: Pista -> Int -> Int
posPrimeiraOcorrencia (pecaA:pecaB:pecas) i | pecaA == pecaB = posPrimeiraOcorrencia (pecaB:pecas) (i+1)
                                            | otherwise = i + 1
-- | Conta o número de ocorrências de um elemento numa lista
contaOcorrencias :: Eq a => a -> [a] -> Int
contaOcorrencias x =  length . filter (==x) 
-- | Aplica o algoritmo de otimização horizontal a 'Instrucoes' de uma só pista.
--
--- | Aplica o algoritmo de otimização horizontal a 'Instrucoes' de uma só pista.
--
-- __Nota__ Não será feita nenhuma compressão horizontal no caso de serem apenas duas instruções iguais seguidas, uma vez que ficaria exatamente com o mesmo número de instruções aquando da utilização do 'Repete'.
padraoHorizontal :: Instrucoes -- ^ 'Instrucoes' para a construção de uma pista
                  -> Int  -- ^ Contador que contará o numero de repetições das 'Instrucoes'. É sempre iniciado com o valor 1.
                  -> Int -- ^ Tamanho de padrão de instruções a encontrar
                  -> Instrucoes -- ^ 'Instrucoes' otimizadas horizontalmente para a construção de uma pista.

padraoHorizontal [] _ _ = []
padraoHorizontal instrucoes i 1   | (length instrucoes == 1) && i > 2 = (Repete i padrao1):(tail instrucoes)
                                  | (length instrucoes == 1) && i == 2 = concat(padrao1:[instrucoes])
                                  | length instrucoes == 1 = instrucoes
                                  | padrao1 == padrao2 = (padraoHorizontal (tail instrucoes) (i+1) 1)
                                  | i > 2 = (Repete i (padrao1)):(padraoHorizontal (drop 1 instrucoes) 1 1)
                                  | i == 2 = padrao1 ++ padrao1 ++ (padraoHorizontal (tail instrucoes) 1 1)
                                  | otherwise = padrao1++(padraoHorizontal (tail instrucoes) 1 1)                                         
  where
    padrao1 = take 1 instrucoes
    padrao2 = take 1 (drop 1 instrucoes)
                       
padraoHorizontal instrucoes i n   | (length instrucoes < (n * 2)) && ((n == 1 && i > 2) || (n > 1 && i > 1)) = (Repete i padrao1):(drop n instrucoes)
                                  | length instrucoes < (n * 2) = instrucoes
                                  | padrao1 == padrao2 = (padraoHorizontal (drop n instrucoes) (i+1) n)
                                  | (n == 1 && i > 2) || (n > 1 && i > 1) = (Repete i (padrao1)):(padraoHorizontal (drop n instrucoes) 1 n)
                                  | otherwise = (head instrucoes):(padraoHorizontal (tail instrucoes) 1 n)
               
  where
        padrao1 = take n instrucoes
        padrao2 = take n (drop n instrucoes)
--Constroi uma matriz com a otimização das instruções com padrões de todos os tamanho possiveis mas menores que metade do tamanho das instruções não compressas, de forma a poder-se encontrar o tamanho de padrão ótimo
compilaPadroesHorizontais :: Instrucoes -> Int -> [Instrucoes]
compilaPadroesHorizontais instrucoes n | n * 2 <= length instrucoes = (padraoHorizontal instrucoes 1 n):(compilaPadroesHorizontais instrucoes (n+1))
                                       | otherwise  = [padraoHorizontal instrucoes 1 n]

-- | Aplica a funçao ´'compilaPadroesHorizontais' às instruções de todas as pistas do mapa.
padroesHorizontaisMatriz :: [Instrucoes] -- ^ Matriz de 'Instrucoes' em que cada lista contém as instruções relativas a uma pista.
                        -> [Instrucoes] -- ^ Matriz cujas listas são as 'Instrucoes' otimizadas segundo os padrões horizontais de cada pista.
padroesHorizontaisMatriz [] = []
padroesHorizontaisMatriz (x:xs) = instrucoesDePista:(padroesHorizontaisMatriz xs)
      where
           instrucoesDePista = minimoDeInstrucoes (compilaPadroesHorizontais x 1)
-- Reaplica os o algoritmo dos padrões horizontais enquanto o tamanhp das instruções otimizadas for inferior ao das intrucções não otimizadas.
reaplicaPadroesHorizontaisMatriz :: [Instrucoes]-> [Instrucoes]
reaplicaPadroesHorizontaisMatriz [] = []
reaplicaPadroesHorizontaisMatriz matrizInstrucoes 
    | comparaMatrizInstrucoes tamanhoInstNaoOtim tamanhoInstOtim = reaplicaPadroesHorizontaisMatriz matrizOtimizada
    | otherwise = matrizInstrucoes

    where
          matrizOtimizada = padroesHorizontaisMatriz matrizInstrucoes
          tamanhoInstNaoOtim = map tamanhoInstrucoes matrizInstrucoes
          tamanhoInstOtim = map tamanhoInstrucoes matrizOtimizada

-- Verifica se o tamanho da lista otimizada é menor que o da lista não otimizada.
comparaMatrizInstrucoes :: [Int] -> [Int] -> Bool
comparaMatrizInstrucoes (x:xs) (y:ys)
  | y < x = True
  | otherwise = comparaMatrizInstrucoes xs ys
comparaMatrizInstrucoes _ _ = False

-- | Converte uma 'Pista' para 'Instrucoes'.
pistaParaInstrucao :: Pista -> Int -> Instrucoes
pistaParaInstrucao [] _ = []
pistaParaInstrucao (p:ps) indiceBulldozer = (pecaParaInstrucao p indiceBulldozer) : (pistaParaInstrucao ps indiceBulldozer)

-- | Converte uma 'Peca' para 'Instrucoes'.
pecaParaInstrucao :: Peca -> Int -> Instrucao
pecaParaInstrucao (Recta piso altura) indiceBulldozer = (Anda [indiceBulldozer] piso)
pecaParaInstrucao (Rampa piso alturainicial alturafinal) indiceBulldozer | difaltura > 0 =  Sobe [indiceBulldozer] piso difaltura
                                                                         | otherwise =  Desce [indiceBulldozer] piso (abs difaltura)

                                      where difaltura =  alturafinal - alturainicial

-- | Aplica o algoritmo de otimização vertical.
otimizaVerticalmente :: [Instrucoes] -- ^ Matriz de 'Instrucoes' em que cada lista contém as instruções relativas a uma pista.
                     -> Instrucoes -- ^ 'Instrucoes' otimizadas verticalmente e concatenadas de forma a juntar 'Instrucoes' de todas as 'Pista's.
otimizaVerticalmente [instrucoes] = instrucoes -- se tiver apenas uma pista
otimizaVerticalmente instrucoesPorPista@(instrucao1:instrucoes)  = concat (map juntaInstrucoesIguais transpostaInstrucoes)
    where
          transpostaInstrucoes = transpose instrucoesPorPista 

-- | Converte um mapa numa Matriz de 'Instrucoes' em que cada lista corresponde às 'Instrucoes' de uma pista.
mapaParaInstrucaoPorPista :: Mapa -> Int -> [Instrucoes]
mapaParaInstrucaoPorPista [] _ = []
mapaParaInstrucaoPorPista (pista:pistas) i = (pistaParaInstrucao (tail(pista)) i):(mapaParaInstrucaoPorPista pistas (i+1))

-- | Dada uma 'Instrucao', devolve a lista de índices de Bulldozer correspondente à mesma.                                      
devolveIndicesBulldozer :: Instrucao -> [Int]
devolveIndicesBulldozer (Anda listaIndices _) = listaIndices
devolveIndicesBulldozer (Sobe listaIndices _ _) = listaIndices
devolveIndicesBulldozer (Desce listaIndices _ _) = listaIndices
devolveIndicesBulldozer (Teleporta listaIndices _) = listaIndices



-- | Junta as 'Instrucoes' relativas a peças iguais mas em pistas diferentes.
--
-- A função principal deste algoritmo, que é a 'otimizaVerticalmente', recebe uma Matriz de 'Instrucoes' organizdas por pista. De seguida, esta matriz é transposta e a função 'JuntaInstrucoesIguais' é chamada com cada uma das linhas da transposta.
--
--prop> juntaInstrucoesIguais [Anda [0] Terra, Anda[1] Terra] == [Anda [0,1] Terra] 
juntaInstrucoesIguais :: Instrucoes -> Instrucoes
juntaInstrucoesIguais [] = []
juntaInstrucoesIguais [x] = [x]

juntaInstrucoesIguais (x@(Repete n1 instrucoes1):(y@(Repete n2 instrucoes2):instrucoes)) | (n1 == n2) = juntaInstrucoesIguais (repetesOtimizadas:instrucoes)
                                                                                         | otherwise = x:juntaInstrucoesIguais(y:instrucoes)
        where
                            
            matrizInstrucoes = instrucoes1: [instrucoes2]
            instrucoesOtimizadas = otimizaVerticalmente matrizInstrucoes
            repetesOtimizadas = Repete n2 instrucoesOtimizadas
          

juntaInstrucoesIguais instrucoes@(instrucao1:t) | elemInstrucao instrucao1 t = ((juntaIndiceBulldozer listaComIguais)):(juntaInstrucoesIguais listaSemIguais)
                                                | otherwise = instrucao1:(juntaInstrucoesIguais t)
                                        
                        where
                             listaComIguais = (mantemInstrucoesIguais instrucao1 instrucoes)
                             listaSemIguais = removeInstrucoesIguais instrucao1 instrucoes

                            
-- | Dadas apenas 'Instrucoes' que correspondem à mesma 'Peca' a função junta as listas de índices de cada uma delas.
juntaIndiceBulldozer ::  Instrucoes -> Instrucao 
juntaIndiceBulldozer [x] = x
juntaIndiceBulldozer (instrucao1:instrucao2:instrucoes) = (case instrucao1 of Anda listaIndices piso -> juntaIndiceBulldozer((Anda listafinal piso):instrucoes)
                                                                              Sobe listaIndices piso unidade -> juntaIndiceBulldozer((Sobe listafinal piso unidade):instrucoes)
                                                                              Desce listaIndices piso unidade -> juntaIndiceBulldozer((Desce listafinal piso unidade):instrucoes)
                                                                              Teleporta listaIndices unidade -> juntaIndiceBulldozer((Teleporta listafinal unidade):instrucoes))
    where 
         (indiceInstrucao1,indicesInstrucao2) = (devolveIndicesBulldozer instrucao1,  devolveIndicesBulldozer instrucao2)
         listafinal = indiceInstrucao1 ++ indicesInstrucao2
-- | Verifica se a 'Instrucao' dada, possui alguma 'Instrucao' equivalente (de acordo com a função 'verificaSeInstrucaoIgual') na lista de 'Instrucoes' fornecida.
--
--prop> elemInstrucao (Anda[0] Terra) [Anda[1] Relva, Anda[2] Terra] == True
elemInstrucao :: Instrucao -> Instrucoes -> Bool 
elemInstrucao x [] = False
elemInstrucao instrucao1 (instrucao2:instrucoes) = (verificaSeInstrucaoIgual instrucao1 instrucao2) || (elemInstrucao instrucao1 instrucoes)

-- | Verifica se as 'Instrucoes' dadas são equivalentes a nível da ação da Bulldozer e do piso a colocar. Desta forma, não tem em conta a lista de índices de Bulldozer de cada 'Instrucao'.
--
--prop> verificaSeInstrucaoIgual (Anda [0] Terra) (Anda [1] Terra ) == True
--prop>  verificaSeInstrucaoIgual (Sobe [0] Relva 1) (Sobe [1] Relva 2) == False
verificaSeInstrucaoIgual :: Instrucao -> Instrucao -> Bool 
verificaSeInstrucaoIgual (Anda l1 unidade1 ) (Anda l2 unidade2) = (unidade1 == unidade2)
verificaSeInstrucaoIgual (Sobe l1 piso1 unidade1 ) (Sobe l2 piso2 unidade2) = (piso1 == piso2 && unidade1 == unidade2)   
verificaSeInstrucaoIgual (Desce l1 piso1 unidade1 ) (Desce l2 piso2 unidade2) = (piso1 == piso2 && unidade1 == unidade2)  
verificaSeInstrucaoIgual (Teleporta l1 unidade1) (Teleporta l2 unidade2) = unidade1 == unidade2
verificaSeInstrucaoIgual _ _ = False

-- | Dada uma lista de 'Instrucoes' e uma 'Instrucao', são removidas dessa lista todas as 'Instrucoes' que forem consideradas equivalentes à dada 'Instrucao'.
--
-- __Nota:__ Essa verificação da equivalência é feita através da função 'verificaSeInstrucaoIgual'.
--
--prop> removeInstrucoesIguais (Anda [0] Terra) [Anda[1] Terra, Anda[2] Relva] == [Anda [2] Relva]
removeInstrucoesIguais :: Instrucao -> Instrucoes -> Instrucoes 
removeInstrucoesIguais x [] = []
removeInstrucoesIguais x (h:t) | verificaSeInstrucaoIgual x h = removeInstrucoesIguais x t
                               | otherwise = h : removeInstrucoesIguais x t 
-- | Dada uma lista de instruções e uma instrução, são mantidas nessas lista todas as instruções que forem consideradas equivalentes à dada instrução e removidos os restantes elementos.
--
-- prop> mantemInstrucoesIguais (Anda [0] Terra) [Anda[1] Terra, Anda[2] Relva] == [Anda [1] Terra]
mantemInstrucoesIguais :: Instrucao -> Instrucoes -> Instrucoes 
mantemInstrucoesIguais x [] = []
mantemInstrucoesIguais x (h:t) | verificaSeInstrucaoIgual x h = h : mantemInstrucoesIguais x t
                               | otherwise = mantemInstrucoesIguais x t  


-- * Funções utilizadas na conversão de 'Instucoes' para Mapa

-- | Converte 'Instrucoes' num 'Mapa' através de várias funcões auxiliares
--
--prop> instrucoesParaMapa [(Anda [1,2] Boost), (Anda [0] Relva)] == [[(Recta Terra 0), (Recta Relva 0)],[(Recta Terra 0), (Recta Boost 0)], [(Recta Terra 0), (Recta Boost 0)]]
instrucoesParaMapa :: Instrucoes -> Mapa
instrucoesParaMapa [] = []
instrucoesParaMapa inst = 
  organizaInstrucoes inst (replicate npistas 1) (replicate npistas [(Recta Terra 0)])
  where
    npistas = (maximum (indicesBulldozers inst)) + 1


-- | Converte uma 'Instrucao' na 'Peca' correspondente
--
--prop> instrucaoParapeca (Anda [23] Terra) 1 == Recta Terra 1
instrucaoParaPeca :: Instrucao -> Int -> Peca
instrucaoParaPeca (Anda _ piso) altAnterior = Recta piso altAnterior
instrucaoParaPeca (Sobe _ piso h) altAnterior = Rampa piso altAnterior (altAnterior + h)
instrucaoParaPeca (Desce _ piso h) altAnterior = Rampa piso altAnterior (altAnterior - h)

-- | Adiciona 'Peca's a um 'Mapa' acumulador percorrendo 'Instrucoes' e mantendo conta das posicões dos bulldozers que constroem o 'Mapa'
organizaInstrucoes :: Instrucoes -> [Int] -> Mapa -> Mapa
organizaInstrucoes inst@((i@(Teleporta ind n)):is) posBulldozers mapaAcumulador =
  organizaInstrucoes is (moveBulldozers posBulldozers ind n) mapaAcumulador
organizaInstrucoes inst@((i@(Repete n instARepetir)):is) posBulldozers mapaAcumulador
  | n <= 0 = organizaInstrucoes is posBulldozers mapaAcumulador
  | otherwise = organizaInstrucoes (instARepetir ++ [(Repete (n - 1) instARepetir)] ++ is) posBulldozers mapaAcumulador
organizaInstrucoes inst@(i:is) posBulldozers mapaAcumulador
  | length ind > 0 = organizaInstrucoes (instSemIndice:is) (moveBulldozers posBulldozers [(head ind)] 1) novoMapa
  | otherwise = organizaInstrucoes is posBulldozers mapaAcumulador
  where
    instSemIndice = removePrimeiroIndice i
    ind = indicesBulldozers [i]
    altAnterior = alturaPeca ((mapaAcumulador!!(head ind))!!(posBulldozers!!(head ind) - 1))
    novaPeca = instrucaoParaPeca i altAnterior
    novaPista = substituirEmLista novaPeca (mapaAcumulador!!(head ind)) (posBulldozers!!(head ind))
    novoMapa = substituirEmLista novaPista mapaAcumulador (head ind)
organizaInstrucoes [] _ mapaAcumulador = mapaAcumulador

-- | Verifica a altura de uma Peca
--
--prop> alturaPeca (Rampa Terra 0 5) == 5
alturaPeca :: Peca -> Int
alturaPeca (Recta _ h) = h
alturaPeca (Rampa _ _ h) = h

-- | Remove o primeiro elemento da lista de indices de uma Instrucao
--
--prop> removePrimeiroIndice (Sobe [1,2] Relva 2) == (Sobe [2] Relva 2)
removePrimeiroIndice :: Instrucao -> Instrucao
removePrimeiroIndice (Anda ind piso) = Anda (tail ind) piso
removePrimeiroIndice (Sobe ind piso h) = Sobe (tail ind) piso h
removePrimeiroIndice (Desce ind piso h) = Desce (tail ind) piso h

-- | Altera as posicões de uma lista de bulldozers numa lista com as posicões de todos os bulldozers
--
--prop> moveBulldozers [1,2] [0] 2 == [3,2]
moveBulldozers :: [Int] -> [Int] -> Int -> [Int]
moveBulldozers bulldozers (i:is) dist = 
  moveBulldozers (substituirEmLista novoIndice bulldozers i) is dist
  where
    novoIndice = (bulldozers!!i) + dist
moveBulldozers bulldozers _ _ = bulldozers

-- | Devolve os índices presentes nas listas de indices em 'Instrucoes'.
--
--prop> indicesBulldozers [(Anda [0] Terra), (Sobe [2] Relva)] == [0,2]
indicesBulldozers :: Instrucoes -> [Int]
indicesBulldozers ((Anda ind _):is) = ind ++ (indicesBulldozers is)
indicesBulldozers ((Sobe ind _ _):is) = ind ++ (indicesBulldozers is)
indicesBulldozers ((Desce ind _ _):is) = ind ++ (indicesBulldozers is)
indicesBulldozers ((Teleporta ind _):is) = ind ++ (indicesBulldozers is)
indicesBulldozers ((Repete _ inst):is) = (indicesBulldozers inst) ++ (indicesBulldozers is)
indicesBulldozers _ = []

-- | Substitui um elemento de uma lista por um novo num dado indice
--
--prop> substituirEmLista 2 [9,4,5] 1 == [9,2,5]
substituirEmLista :: a -> [a] -> Int -> [a]
substituirEmLista e l n = (take n l) ++ [e] ++ (drop (1 + n) l)
