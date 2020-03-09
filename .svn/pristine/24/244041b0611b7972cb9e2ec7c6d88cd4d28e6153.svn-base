module Menus where


-- | Elemento constituinte de um menu.
data Elemento = Elemento { idEspecial :: String, fullscreen :: Bool, pos :: (Float, Float), menu :: TipoMenu} deriving (Show)

data TipoMenu = Lista {colunas :: Int, padding :: (Float, Float), spriteNames :: [(String,String)], indiceCursor :: Int} |
                Botao {sprite :: String} |
                MenuQP {jogadores :: [EstadoJogadorQP], opcoesqp :: OpcoesQP, indiceCursor :: Int} |
                ScoreBoard {score :: [(String, Int)]}
                deriving (Show)
-- | Opcões do menu 'Quickplay' que podem ser configuradas pelo utilizador. Contém dois boleanos, o primeiro indica se a parte selecionada do menu são as configurações do Jogo ou a seleção da cor dos jogadores. O segundo indica se as opções 
-- estºao em modo de alteração de valores(incrementação).
data OpcoesQP =  OpcoesQP {nBots :: Int,  comprimento :: Int, nPistas :: Int, seed :: Int, isOpen :: Bool, opcaoSelecionada :: Bool} deriving Show

-- | Estado de Jogador no menu 'Quickplay' que é relevante para saber que conjunto de teclas é responsável por alterar a cor de cada jogador.
data EstadoJogadorQP = EstadoJogadorQP {indiceJogador :: Int, spriteMota :: String, isFocused :: Bool} deriving Show


-- | Função principal dos menus que atualiza o ultimo menu da lista de todos os menus
-- ativos no momento e devolve uma lista de menus conforme a opção escolhida pelo jogador

-- | Função que dá aos cursores dos menus uma proriedade de scrolling. Quando o índice do cursor é incrementado para um valor superior ao índice máximo válido, este volta ao ínicio (ou fim).
normalizaCursor :: Int -> [a] -> Int
normalizaCursor i es
  | i < 0           = length es - 1
  | i >= (length es) = 0
  | otherwise       = i


menusRelevantes :: [Elemento] -> [Elemento]
menusRelevantes (e@Elemento{fullscreen = True}:es) = [e]
menusRelevantes (e:es) = (menusRelevantes es) ++ [e]
menusRelevantes [] = []

-- | Função que indica quais os jogadores que foram adicionados ao futuro jogo no menu 'Quickplay'.
jogadoresAtivos :: [EstadoJogadorQP] -> Int
jogadoresAtivos ((EstadoJogadorQP _ _ True):js) = jogadoresAtivos js + 1
jogadoresAtivos (j:js) = jogadoresAtivos js
jogadoresAtivos [] = 0


verificaAdicionado :: EstadoJogadorQP -> Bool
verificaAdicionado (EstadoJogadorQP _ _ add) = add

-- | Função que adiciona um jogador ao futuro jogo no menu quickplay . O utilizador correspondente poderá passar a configurar a cor do seu Jogador.
adicionaJogador :: [EstadoJogadorQP] -> Int -> [EstadoJogadorQP]
adicionaJogador (j:js) 0 = j{isFocused = True}:js
adicionaJogador (j:js) i = j:(adicionaJogador js (i-1))
adicionaJogador [] _ = []

-- | Função que remove um Jogador do futuro jogo no menu quickplay. O jogador passa a estar "inativo" e o utilizador não poderá alterar as suas cores.
removeJogador :: [EstadoJogadorQP] -> Int -> [EstadoJogadorQP]
removeJogador (j:js) 0 = j{isFocused = False}:js
removeJogador (j:js) i = j:(removeJogador js (i-1))
removeJogador [] _ = []


indicesJogadoresAtivos :: [EstadoJogadorQP] -> [Int]
indicesJogadoresAtivos (j@(EstadoJogadorQP i _ True):js) = i:(indicesJogadoresAtivos js)
indicesJogadoresAtivos (j:js) = indicesJogadoresAtivos js
indicesJogadoresAtivos [] = []


criaListaVencedores :: [String] -> [Int] -> [(String, Int)]
criaListaVencedores motas (v:vs) = (motas!!v, (v+1)):(criaListaVencedores motas vs)
criaListaVencedores motas [] = []


-- | Os vários menus predefinidos do jogo

menuPrincipal :: Elemento
menuPrincipal = Elemento
                "mainmenu"                                  --ID Especial
                True                                        --Fullscreen
                (-600, 0)                                   --Posição
                (Lista                                      --Construtor de Tipo de Menu
                3                                           --Colunas
                (500, 0)                                    --Padding (x,y) entre elementos
                [("campanha0","campanha1"),
                 ("quickplay0","quickplay1"),
                 ("opcoes0","opcoes1")]                     --Sprites
                0)                                          --Indice de Cursor

botaoPressStart :: Elemento
botaoPressStart = Elemento
                "pstart"                                    --ID Especial
                True                                        --Fullscreen
                (0,0)                                       --Posição
                (Botao                                      --Construtor de Tipo de Menu
                "pressq")                                   --Sprite


menuQuickPlay :: Elemento                
menuQuickPlay = Elemento
                "quickplay"                            --  ID Especial
                True                                    -- Fullscreen 
                (-683, 384)                              -- Posição
                (MenuQP                                    -- Construtor de Tipo de Menu
                [(EstadoJogadorQP 0 "redbike" True),
                (EstadoJogadorQP 1 "bluebike" False),       --  Estados quickplay dos jogadores
                (EstadoJogadorQP 2 "cpubike" False),
                (EstadoJogadorQP 3 "whitebike" False)]
                (OpcoesQP 3 15 4 1 False False)
                0)


menuCampanha :: Elemento
menuCampanha = Elemento
               "campanha"
               True
               ((-750),0)
               (Lista
               4
               (500, 0)
               [("nivel1_0", "nivel1_1"),
                ("nivel2_0", "nivel2_1"),
                ("nivel3_0", "nivel3_1"),
                ("nivel4_0", "nivel4_1")]
                0)

botaoControlos :: Elemento
botaoControlos = Elemento
                 "controls"
                 True
                 (0,0)
                 (Botao
                 "controls")