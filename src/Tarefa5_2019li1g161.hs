  {- | 

= Introdução 
Nesta tarefa foi desenvolvida uma interface gráfica para o jogo, recorrendo ao Gloss, biblioteca que facilita a criação de jogos em 2D. Esta interface atualiza ao longo de um determinado periodo 
de tempo e conforme o input do utilizador. 
Desta forma, consegue-se obter um jogo totalmente completo e pronto a ser jogado. 

= Objetivos 
Em primeiro lugar, procurou-se ter um jogo completamente funcional, apelativo e intuitivo de jogar.
Além de cumprir os requisitos mínimos, implementaram-se funcionalidades extra no jogo que serão agora enumeradas:

  - Opção de jogar até 4 jogadores 'humanos' e/ou contra 'bots'.
  - Modo de jogo 'Quickplay' com configurações do número de Jogadores, das figuras do jogador e detalhes do 'mapa'.
  - Modo de jogo 'Campanha' que tem níveis com dificuldade cada vez maior.
  - Implementação de menus : 'menu' 'Principal', 'menu' 'QuickPlay', 'menu' 'Controlos' (indica as teclas a ser utilizadas).
  - Implementação de um 'mapa' 'dinamico' que se desloca juntamente com o jogador (possibilidade de jogar em mapas extremamente extensos).

= Conclusão
  Estamos bastante satisfeitos com o resultado do nosso jogo e podemos dizer que foi, sem dúvida, um projeto agradável de se realizar. Determinadas aspetos poderiam ter sido aprimorados
  com o devido tempo, mas tendo em consideração a falta deste, cremos que o resultado foi satisfatório. Implementamos diversas funcionalidades que não eram exigidas e que tornaram o jogo
  significativamente mais apelativo e intuitivo de jogar.

-}


module Main where

import LI11920
import Tarefa1_2019li1g161
import Tarefa2_2019li1g161
import Tarefa4_2019li1g161
import Tarefa6_2019li1g161
import Menus
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy


-- * Função Principal
-- | Contem a função 'Play' que, por sua vez, inclui as funções : 'desenhaEstadoMestre', 'receberInput', 'reageTempo'
main :: IO ()
main = do

  -- Motas
  Just cpubike <- loadJuicyPNG "img/cpubike.png"
  Just bluebike <- loadJuicyPNG "img/bluebike.png"
  Just whitebike  <- loadJuicyPNG"img/whitebike.png"
  Just redbike  <- loadJuicyPNG "img/redbike.png"
  Just hills <- loadJuicyPNG "img/hills.png"
  Just greenbike <- loadJuicyPNG "img/greenbike.png"

-- menu Principal
  Just qpSelecionada <- loadJuicyPNG "img/qpSelecionada.png"
  Just campanhaSelecionada <- loadJuicyPNG "img/campanhaSelecionada.png"
  Just controlosSelecionados <- loadJuicyPNG "img/controlosSelecionados.png"
-- menu QuickPLay
  Just tituloNumeroPistas <- loadJuicyPNG "img/tituloNumeroPistas.png"
  Just tituloComprimento <- loadJuicyPNG "img/tituloComprimento.png"
  Just tituloNBots <- loadJuicyPNG "img/tituloNBots.png"
  Just retroceder <- loadJuicyPNG "img/retroceder.png"
  Just ajustaComprimento <- loadJuicyPNG "img/ajustaComprimento.png"
  Just ajustaPistas <- loadJuicyPNG "img/ajustaPistas.png"
  Just ajustaBots <- loadJuicyPNG "img/ajustaBots.png"
-- menu Campanha
  Just nivel10 <- loadJuicyPNG "img/nivel1_0.png"
  Just nivel11 <- loadJuicyPNG "img/nivel1_1.png"
  Just nivel20 <- loadJuicyPNG "img/nivel2_0.png"
  Just nivel21 <- loadJuicyPNG "img/nivel2_1.png"
  Just nivel30 <- loadJuicyPNG "img/nivel3_0.png"
  Just nivel31 <- loadJuicyPNG "img/nivel3_1.png"
  Just nivel40 <- loadJuicyPNG "img/nivel4_0.png"
  Just nivel41 <- loadJuicyPNG "img/nivel4_1.png"
  Just pressq <- loadJuicyJPG "img/pressq.jpeg"

  -- controlos
  Just controlos <- loadJuicyPNG "img/controlos.png"


  let menuSprites = [("nivel1_0", nivel10), ("nivel1_1", nivel11), ("nivel2_0", nivel20), ("nivel2_1", nivel21), ("nivel3_0", nivel30), ("nivel3_1", nivel31), ("nivel4_0", nivel40), ("nivel4_1", nivel41), ("cpubike", cpubike), ("bluebike", bluebike), ("redbike", redbike), ("greenbike",greenbike), ("whitebike", whitebike),("pressq", pressq),("qpSelecionada", qpSelecionada),("campanhaSelecionada", campanhaSelecionada), ("controlosSelecionados",controlosSelecionados),("tituloNumeroPistas", tituloNumeroPistas ),("tituloComprimento", tituloComprimento), ("tituloNBots", tituloNBots), ("retroceder",retroceder), ("ajustaComprimento", ajustaComprimento ), ("ajustaPistas", ajustaPistas), ("ajustaBots",ajustaBots),("controlos",controlos) ]
  let jogoSprites = [("hills", hills), ("cpubike", cpubike), ("bluebike", bluebike), ("redbike", redbike), ("greenbike",greenbike),("whitebike",whitebike)]
  let listaSprites = [menuSprites, jogoSprites]
 
  play (InWindow "Excite Bike" (1366,768) (0,0))    -- janela onde irá correr o jogo
    (greyN 0.5)                                     -- côr do fundo da janela
    60                                              -- frame rate
    (estadoMestreInicial listaSprites)              -- estado inicial
    desenhaEstadoMestre                             -- desenha o estado do jogo
    receberInput                                    -- regista os inputs relevantes
    reageTempo                                      -- reage ao passar do tempo

-- * Estado Mestre e outros tipos de dados

data EstadoMestre = EstadoMestre 
    { menus         :: [Elemento]
    , estadoJogo    :: EstadoJogo -- ^ Estado que descreve o estado de uma dada "corrida"
    , teclasHold    :: [Key] -- ^ Conjunto de teclas que estão carregadas nesta frame
    , teclasTap     :: [Key] -- ^ Conjunto de teclas que foram premidas nesta frame
    , teclasRelease :: [Key] -- ^ Conjunto de teclas que foram soltas neste frame
    , opcoes        :: Opcoes -- ^ Conjunto de teclas que podem ser utilizadas para interagir com o Jogo.
    , sprites       :: [[Sprite]] -- ^ Lista que contém pares cuja primeira componente é o 'id' de uma 'Picture' e a segunda componente é a própria 'Picture'.
    }

data EstadoJogo = EstadoJogo 
    {emJogo :: Bool -- ^ Bool que descreve se está a decorrer uma corrida
    , estado :: Estado -- ^ Estado da corrida atual
    , cameras :: [Camera] -- ^ Lista de cameras que acompanham os jogadores humanos de forma a poder renderizar o mapa de forma dinâmica
    , motas :: [String] -- ^ Lista de nomes das imagens das motas de cada jogador, bot ou não, desta corrida
    , iHumanos :: [Int] -- ^ Lista de índices dos jogadores correspondentes a jogadores humanos
    , vencedores :: [Int]} -- ^ Lista de jogadores que já chegaram ao fim do mapa da corrida atual

data Opcoes = Opcoes 
    {teclas :: [[Key]]}

data Camera = Camera {posCamera :: (Float, Float)} deriving Show
-- | Interação que pode ser realizada por diferentes utilizadores, motivo pelo qual é utilizado um inteiro para o índice do Jogador.
data Interacao 
  = ICima Int
  | Esquerda Int
  | IBaixo Int
  | Direita Int
  | Confirma Int
  | Cancela Int

-- | Tipo que representa um par com o nome de uma imagem e a respetiva imagem
type Sprite = (String, Picture)

-- | Função que dada uma lista com todas as imagens que o jogo usará, devolve um EstadoMestre no seu estado inicial predefinido
estadoMestreInicial :: [[Sprite]] -- ^ Conjunto de pares de nomes de imagens e imagens que vão ser utilizados neste jogo
                    -> EstadoMestre
estadoMestreInicial sprites = EstadoMestre 
  [botaoPressStart] 
  (EstadoJogo False (Estado [[]] []) [] [] [] [])
  [] [] [] 
  opcoes 
  sprites
  where
    opcoes = Opcoes [[(Char 'w'), (Char 'a'), (Char 's'), (Char 'd'), (Char 'q'), (Char 'e')],
              [(SpecialKey KeyUp), (SpecialKey KeyLeft), (SpecialKey KeyDown), (SpecialKey KeyRight), (SpecialKey KeyShiftR), (SpecialKey KeyCtrlR)],
             [(Char 'i'), (Char 'j'), (Char 'k'), (Char 'l'), (Char 'u'), (Char 'o')],
              [(Char 't'), (Char 'f'), (Char 'g'), (Char 'h'), (Char 'r'), (Char 'y')]]


-- | Função que dado um EstadoMestre devolve a imagem que resulta da conjunção da renderização da corrida atual, se estiver a decorrer um, e dos menus abertos atualmente
desenhaEstadoMestre :: EstadoMestre -> Picture
desenhaEstadoMestre (EstadoMestre menus estadoJogo@EstadoJogo{emJogo = True} _ _ _ opcoes sprites) =
  Pictures ((desenhaJogo estadoJogo (sprites!!1)) ++ (desenhaInterface (menusRelevantes menus) opcoes (head sprites)))
desenhaEstadoMestre (EstadoMestre menus@(e:es) _ _ ttap _ opcoes sprites) =  Pictures (desenhaInterface (menusRelevantes menus)opcoes (head sprites))




-- * Receber Inputs

-- | Função que recebe um evento de input e regista de acordo com os conjuntos de teclas do EstadoMestre
receberInput :: Event -> EstadoMestre -> EstadoMestre
receberInput (EventKey k Down _ _) estado@EstadoMestre{teclasHold = th, teclasTap = tt} = estado{teclasHold = k:th, teclasTap = k:tt}
receberInput (EventKey k Up _ _) estado@EstadoMestre{teclasHold = th, teclasRelease = tr} = estado{teclasHold = (filter (/=k) th), teclasRelease = k:tr}
receberInput _ estado = estado


-- * Desenhar Jogo

-- | Função que dado um EstadoJogo e um conjunto de sprites devolve as imagens correspondentes à renderização da mapa e dos
-- jogadores da corrida atual do ponto de vista da primeira camera
desenhaJogo :: EstadoJogo -> [Sprite] -> [Picture]
desenhaJogo estJ@EstadoJogo{cameras = (cam:cs), estado = est, motas = mts} sprites = 
  (fundo:(desenhaCamera cam est mts sprites))
  where
    fundo = carregaImagem "hills" sprites (1,1)


-- | Função que dado uma Camera, um Estado, uma lista de nomes de images, e um conjunto de sprites
-- devolve as imagens correspondentes à renderização da mapa e dos jogadores da corrida atual
-- do ponto de vista da primeira camera
desenhaCamera :: Camera -- ^ Camera usada para definir o ponto de vista da renderização
              -> Estado -- ^ Estado a renderizar
              -> [String] -- ^ Lista de nomes das motas de cada jogador da corrida atual
              -> [Sprite] -- ^ Lista de sprites a usar na renderização
              -> [Picture]
desenhaCamera (Camera (x,y)) (Estado mapa jogs@((Jogador pista dist _ _ _):js)) motas sprites =
  desenhaJogadores jogs (x, y) (desenhaMapa mapa (mapaDeTransparencia 0 mapa ((floor dist), pista)) ((-(x*100) - 150), (-(y*50) - 50))) mapa motas sprites


-- | Função que dado um Mapa, um mapa de transparência e coordenadas devolve a renderização desse mapa
desenhaMapa :: Mapa -- ^ Mapa a renderizar
            -> [[Int]] -- ^ Mapa de transparência em que cada inteiro corresponde à transparência de uma peça no mapa
            -> (Float, Float) -- ^ Coordenadas do ponto de vista
            -> [Picture]
desenhaMapa [] _ _ = []
desenhaMapa (p:ps) (t:ts) (x, y) = (Pictures (desenhaPista p t (x, y))):(desenhaMapa ps ts (x, (y-50)))


-- | Função que dada uma pista e o seu mapa de transparência correspondente bem como as coordenadas do ponto
-- de vista renderiza essa pista
desenhaPista :: Pista -- ^ Pista a renderizar
             -> [Int] -- ^ Mapa de transpârencia, a cada inteiro corresponde a transparência da peça no mesmo índice da pista
             ->  (Float, Float) -- ^ Coordenadas do ponto de vista
             -> [Picture]
desenhaPista (p:ps) (transp:ts) (x, y) = (desenhaPeca p (x, y) transp):(desenhaPista ps ts ((x+100), y))
desenhaPista [] _ _  = []


-- | Função que dada uma peça , as coordenadas do ponto de vista e o seu valor de transparência
-- renderiza essa peça
desenhaPeca :: Peca  -- ^ Peça a renderizar
            -> (Float, Float) -- ^ Coordenadas do ponto de vista
            ->  Int -- ^ Transparência da peça
            -> Picture
desenhaPeca (Recta piso h) (x, y) transp = Translate 0 (fromIntegral h * 50)
  (Pictures 
  [(Color (dim ( palete)) (Polygon [(x - 2, y - 2), (x + 102, y - 2), (x + 102, y + 52), (x - 2, y + 52)])),
  (Color palete (Polygon [(x, y), (x + 100, y), (x + 100, y + 50), (x, y + 50)])),
  (Color (dark palete) (Polygon [(x, y), (x + 100, y), (x + 100, y - wallH), (x, y - wallH)]))])
  where
    palete = pisoParaCor piso transp
    wallH = fromIntegral (h * 50)
desenhaPeca (Rampa piso hi hf) (x, y) clear = Translate 0 (fromIntegral hi * 50)
  (Pictures
  [(Color (dim ( palete)) (Polygon [(x - 2, y - 2), (x + 102, y + difh - 2), (x + 102, y + 52 + difh), (x - 2, y + 52)])),
  (Color palete (Polygon [(x, y), (x + 100, y + difh), (x + 100, y + 50 + difh), (x, y + 50)])),
  (Color (dark palete) (Polygon [(x, y), (x + 100, y + difh), (x + 100, y - wallH), (x, y - wallH)]))])
  where
    palete = pisoParaCor piso clear
    difh = fromIntegral ((hf - hi) * 50)
    wallH = fromIntegral (hi * 50)


-- | Função que dado uma piso de uma peça e a sua transparência devolve a cor a renderizar
pisoParaCor :: Piso -- ^ Piso a renderizar
            -> Int -- ^ Transparência
            -> Color
pisoParaCor p transp
    | p == Terra = makeColorI 160 60 25 transp
    | p == Relva = makeColorI 5 120 45 transp
    | p == Lama = makeColorI 105 50 40 transp
    | p == Boost = makeColorI 10 250 185 transp
    | otherwise = makeColorI 255 255 255 transp


-- | Função que dado um acumulador, um mapa, e as coordenadas de um jogador devolve um mapa de transparência de
-- forma a que o jogador não fique tapado por peças 
mapaDeTransparencia :: Int -- ^ Acumulador
                    -> Mapa -- ^ Mapa para o qual calcular transparência
                    -> (Int, Int) -- ^ Coordenadas de um jogador (distância, pista)
                    -> [[Int]]
mapaDeTransparencia n mapa@(p:ps) (x, nPista)
  | n <= nPista = (replicate (length p) 255):(mapaDeTransparencia (n + 1) (p:ps) (x, nPista))
  | otherwise = (pistaParaTransparencia (mapa!!n) (mapa!!nPista) x (n - nPista)):(mapaDeTransparencia (n + 1) (p:ps) (x, nPista))
mapaDeTransparencia _ [] _ = []


-- | Função que dada uma pista, a pista de um jogador, a distância desse jogador e
-- as coordenadas de um jogador devolve um mapa de transparência de forma a que o jogador não fique tapado por peças 
pistaParaTransparencia :: Pista  -- ^ Pista para a qual calcular transparência
                       -> Pista -- ^ Pista do jogador
                       -> Int -- ^ Floor da distância do jogador
                       -> Int -- ^ Diferença entre índice de pista a calcular e pista do jogador
                       -> [Int]
pistaParaTransparencia (p:ps) (pJog:pJogs) x difPistas
  | x > 0 || (x > -5 && not (pecaSobrepoe p pJog difPistas)) = (255):(pistaParaTransparencia ps pJogs (x-1) difPistas)
  | x < -4 && not (pecaSobrepoe p pJog difPistas) = replicate (length ps + 1) 255
  | otherwise = (100):(pistaParaTransparencia ps pJogs (x-1) difPistas)


-- | Função que dadas 2 peças e a difereça entre o índice da pista de cada uma devolve
-- um booleano que indica se a primeira peça se sobrepõe à segunda
pecaSobrepoe :: Peca -- ^ Peça a verificar se é sobreposta
             -> Peca -- ^ Peça a verificar se sobrepõe
             -> Int -- ^ Diferença entre índice das pistas de cada peça
             -> Bool
pecaSobrepoe p1 p2 difPistas
  | (maximum (obterAlturasPeca p1) - difPistas) > minimum (obterAlturasPeca p2) = True
  | otherwise = False



-- | Função que dada 1 peça devolve um conjunto de inteiros que contem a altura inicial e final da peça
obterAlturasPeca :: Peca -- ^ Peça para a qual verificar alturas
                 -> [Int]
obterAlturasPeca (Recta _ h) = [h]
obterAlturasPeca (Rampa _ h1 h2) = [h1,h2]


-- | Função que dada uma lista de jogadores, as coordenadas de um ponto de vista, uma renderização de um mapa, o mesmo mapa, uma lista de nomes de motas
-- e o conjunto de sprites do jogo devolve a renderização do mapa com os jogadores inseridos
desenhaJogadores :: [Jogador] -- ^ Jogadores a renderizar
                 -> (Float, Float) -- ^ Coordenadas do ponto de vista
                 -> [Picture] -- ^ Mapa renderizado, em que cada imagem do conjunto corresponde a uma pista
                 -> Mapa -- ^ Mapa a renderizar
                 -> [String] -- ^ Lista de nomes das motas dos jogadores
                 -> [Sprite] -- ^ Lista de sprites do jogo
                 -> [Picture]
desenhaJogadores (j:js) coordsCam mapaPictures mapa (mota:motas) sprites =
  desenhaJogadores js coordsCam (desenhaJogador j coordsCam mapaPictures mapa mota sprites) mapa motas sprites
desenhaJogadores [] _ mapaPictures _ _ _ = mapaPictures


-- | Função que dado um jogador, as coordenadas de um ponto de vista, uma renderização de um mapa, o mesmo mapa, um nome de uma mota
-- e o conjunto de sprites do jogo devolve a renderização do mapa com esse jogador inserido
desenhaJogador :: Jogador -- ^ Jogador a renderizar
               -> (Float, Float) -- ^ Coordenadas do ponto de vista
               -> [Picture] -- ^ Renderização de um mapa, em que cada imagem do conjunto corresponde a uma pista
               -> Mapa -- ^ Mapa a renderizar
               -> String -- ^ Nome da mota correspondente ao jogador a renderizar
               -> [Sprite] -- ^ Lista de sprites do jogo
               -> [Picture]
desenhaJogador (Jogador nPista dist _ _ (Ar h i _)) (x, y) mapaPictures mapa mota sprites = 
  take (nPista) mapaPictures ++ [Pictures (mapaPictures!!nPista:[Translate xPic yPic motaPic])] ++ (drop (nPista+1) mapaPictures)
  where
    motaPic = Rotate (realToFrac (-i)) (carregaImagem mota sprites (1,1))
    xPic = realToFrac (-150 + dist*100) - x*100
    yPic = realToFrac (-10 -50*(fromIntegral nPista) + h*50) -y*50
desenhaJogador (Jogador nPista dist _ _ _) (x, y) mapaPictures mapa mota sprites =
  take (nPista) mapaPictures ++ [Pictures (mapaPictures!!nPista:[Translate xPic yPic motaPic])] ++ (drop (nPista+1) mapaPictures)
  where
    peca = mapa!!nPista!!(floor dist)
    motaPic = Rotate (realToFrac (-(obterInclinacaoPeca peca))) (carregaImagem mota sprites (1,1))
    h = obterAlturaNaPeca peca dist
    xPic = realToFrac (-150 + dist*100) - x*100
    yPic = realToFrac (-10 -50*(fromIntegral nPista) + h*50) - y*50



-- * Desenhar Interface
-- | Função que desenha os diversos elementos(menus) relevantes da interface gráfica.
desenhaInterface :: [Elemento] -> Opcoes -> [Sprite] -> [Picture]
desenhaInterface (e:es) opcoes sprites = (desenhaElemento e opcoes sprites):(desenhaInterface es opcoes sprites)
desenhaInterface [] _  _ = []

-- | Função que desnha um elemento (menu) da interface gráfica.
desenhaElemento :: Elemento -> Opcoes -> [Sprite] -> Picture
desenhaElemento(e@Elemento{idEspecial = "pstart"}) _ sprites = carregaImagem "pressq" sprites (2,2)
desenhaElemento (e@Elemento{idEspecial = "mainmenu"}) _ sprites = desenhaMenuPrincipal e sprites
desenhaElemento (e@Elemento{idEspecial = "campanha"}) _ sprites = Scale 0.5 0.5 (Pictures (desenhaLista e sprites 0))
desenhaElemento e@Elemento{menu = Lista{}} _ sprites = Pictures (desenhaLista e sprites 0)
desenhaElemento e@Elemento{menu = Botao{}} _ sprites = desenhaBotao e sprites
desenhaElemento e@Elemento{menu = MenuQP{}} opcoes sprites = Pictures (desenhaMenuQP e opcoes sprites)
desenhaElemento e@Elemento{menu = ScoreBoard{}} _ sprites = Pictures (desenhaScoreboard e sprites)

-- | Função que desenha o menu principal com as opções 'Quickplay', 'Campanha' e 'Controlos'.
desenhaMenuPrincipal :: Elemento -> [Sprite] -> Picture
desenhaMenuPrincipal e@(Elemento {menu = Lista {indiceCursor = i}}) sprites | i == 0 = (carregaImagem "qpSelecionada" sprites (0.73,0.73))
                                                                            | (i == 1) = (carregaImagem "campanhaSelecionada" sprites (0.73,0.73)) 
                                                                            | otherwise =(carregaImagem "controlosSelecionados" sprites (0.73,0.73))
-- | Função que desenha um menu do tipo 'Lista'.
desenhaLista :: Elemento -> [Sprite] -> Int -> [Picture]
desenhaLista e@(Elemento _ _ (posx, posy) m@(Lista col (padx, pady) (nomeSpr:sprts) cursor)) spriteList i =
  (Translate x y item):(desenhaLista e{menu = m{spriteNames = sprts}} spriteList (i + 1))
  where
       x = posx + padx * (fromIntegral (mod i col))
       y = posy - pady * (fromIntegral (div i col))
       item
        | cursor == i = carregaImagem (snd nomeSpr) spriteList (1,1)
        | otherwise =  carregaImagem (fst nomeSpr) spriteList (1,1)
desenhaLista (Elemento _ _ _ (Lista _ _ [] _)) _ _ = []

-- | Função que desenha um menu do tipo 'Botao'.
desenhaBotao :: Elemento -> [Sprite] -> Picture
desenhaBotao (Elemento "controlos" _ (x,y) (Botao nomeSpr)) sprites =   Scale 0.78 0.78 (Translate x y (carregaImagem nomeSpr sprites (1,1)))
desenhaBotao (Elemento _ _ (x,y) (Botao nomeSpr)) sprites =
  Translate x y (carregaImagem nomeSpr sprites (1,1))

-- |Função que desenha uma 'Picture' criada no photoshop como fundo do menu.
desenhaImagemBackground :: Elemento -> [Sprite] -> Picture
desenhaImagemBackground  e@Elemento{menu = m@MenuQP{indiceCursor = i,opcoesqp = op@OpcoesQP {isOpen =  False}}} sprites= (carregaImagem "tituloNumeroPistas" sprites (0.73,0.73))  
desenhaImagemBackground  e@Elemento{menu = m@MenuQP{indiceCursor = i, opcoesqp = op}} sprites                              
  | (i ==  0 && opcaoSelecionada op) = (carregaImagem "ajustaPistas" sprites (0.73,0.73)) 
  | (i == 0) = (carregaImagem "tituloNumeroPistas" sprites (0.73,0.73)) 
  | (i ==1 && opcaoSelecionada op) = (carregaImagem "ajustaComprimento" sprites (0.73,0.73))
  |(i == 1 ) = (carregaImagem "tituloComprimento" sprites (0.73,0.73)) 
  | (i == 2 && opcaoSelecionada op) = (carregaImagem "ajustaBots" sprites (0.73,0.73)) 
  | (i == 2) = (carregaImagem "tituloNBots" sprites (0.73,0.73)) 
  | otherwise = (carregaImagem "retroceder" sprites (0.73,0.73)) 


-- | Função que desenha o menu 'Quickplay'. Recorre a polígonos escurecidos de modo a tornar-se mais evidente aquilo que está selecionado.                                                                                   
desenhaMenuQP :: Elemento -> Opcoes -> [Sprite] -> [Picture]
desenhaMenuQP e@(Elemento _ _ (x,y) (MenuQP jogadores opcoesQP@OpcoesQP{isOpen = toggledOpc} cursor)) opcoes sprites =
  [(desenhaImagemBackground e sprites)]++ [pressQ] ++(desenhaJogadoresQP jogadores (x, y) opcoes sprites) ++ desenhaOpcoes opcoesQP cursor ++ [taparJanela]
  where
    pressQ = Translate (-620) (340) (Scale 0.2 0.2 (Text ("Pressiona Q para Jogar")))
    taparJanela
      | toggledOpc = Color (makeColorI 30 30 30 150) (Translate 0 190 (Polygon (rectanglePath 1366 400)))
      | otherwise = Color (makeColorI 30 30 30 150) (Translate 0 (-270) (Polygon (rectanglePath 1366 530)))


-- | Converte uma tecla para um texto do tipo "'Pressiona 'tecla'"
teclaParaTexto :: Key -> Picture
teclaParaTexto (Char x) = Text ("Pressiona " ++ show x)
teclaParaTexto (SpecialKey x) = Text ("Pressiona " ++ show x)


-- | Função que desenha no menu QuickPlay os quatro jogadores da cor selecionada pelo utilizador e o seu texto correspondente.
desenhaJogadoresQP :: [EstadoJogadorQP] -> (Float, Float) -> Opcoes -> [Sprite] -> [Picture]
desenhaJogadoresQP [] _ _ _ = []
desenhaJogadoresQP (j:js) (x,y) opcoes sprites =
 (Translate x y (desenhaJogadorQP j opcoes sprites)):(desenhaJogadoresQP js (x+290,y) opcoes sprites)
                  

-- | Função que desenha no menu QuickPlay o jogador da cor selecionada pelo utilizador e o seu texto correspondente.
desenhaJogadorQP :: EstadoJogadorQP -> Opcoes -> [Sprite] -> Picture
desenhaJogadorQP (EstadoJogadorQP i mota added) (Opcoes tOpc) sprites =
  Pictures [scaledPlayer, Translate 180 (-500) (carregaImagem "keystemplate" sprites (1,1)), feedbackAdded]
  where
    scaledPlayer =  Scale 0.9 0.9 (Translate 260 (-180) (carregaImagem mota sprites (4,4)))
    tecla = teclaParaTexto ((tOpc!!i)!!4)
    feedbackAdded
      | added = Blank
      | otherwise = Pictures [Translate 230 (-195) (Color (makeColorI 30 30 30  150) (Polygon (rectanglePath 275 390))),
                              Translate 110 (-50) (Scale 0.2 0.2 tecla)]     

-- | Função que desenha no ecrã os valores das opções do menu Quickplay (comprimento, número de pistas e bots).
desenhaOpcoes :: OpcoesQP -> Int -> [Picture]
desenhaOpcoes (OpcoesQP nCPUs comprimento nPistas seed aberto selecionado) _= [opNPistas, opCPUs, opComprimento] 
  where                               
    opNPistas = Translate (-475) (-270) (Scale 0.2 0.2 (Text ((show nPistas))))
    opComprimento = Translate (-35) (-270) (Scale 0.2 0.2 (Text ( (show comprimento))))
    opCPUs = Translate (375) (-260) (Scale 0.2 0.2  (Text  (show nCPUs)))

-- | Função que dado um elemento scoreboard e as sprites do jogo desenha essa scoreboard
desenhaScoreboard :: Elemento -> [Sprite] -> [Picture]
desenhaScoreboard e@Elemento{pos = (x,y), menu = m@ScoreBoard{score = ((mota, i):s)}} sprites =
  Scale 0.3 0.3 (Pictures [Translate x (y+30) (carregaImagem mota sprites (3, 3)), Translate (x+100) y (Text ("Jogador " ++ (show i)))]):(desenhaScoreboard e{pos= (x, y - 175), menu = m{score = s}} sprites )
desenhaScoreboard e _ = []
-- * Atualizar EstadoMestre

-- | Função que dado um valor de tempo e um estado mestre devolve o resultado da passagem
-- de tempo em relação a esse estado mestre
reageTempo :: Float -> EstadoMestre -> EstadoMestre
reageTempo t estadoMestre@(EstadoMestre [] estJogo@EstadoJogo{motas = ms, vencedores = vs} teclasHold teclasTap teclasRelease (Opcoes teclasOpc) _)  
  | determinarJogoTerminado estJogo = esvaziaTeclas estadoMestre{menus = [Elemento "" False ((-300),200) (ScoreBoard (criaListaVencedores ms vs))]}
  | otherwise = esvaziaTeclas (atualizaJogo  t estadoMestre (interacoesParaJogadas interacoes))
    where
      interacoes = ((teclasParaInteracoes teclasOpc teclasHold), (teclasParaInteracoes teclasOpc teclasTap),(teclasParaInteracoes teclasOpc teclasRelease))
reageTempo _ estadoMestre@(EstadoMestre _ _ _ teclasTap _ (Opcoes teclasOpc) _) =
  esvaziaTeclas (atualizaMenus estadoMestre (teclasParaInteracoes teclasOpc teclasTap))


-- | Função que dadas 3 listas de interações devolve uma lista de pares de jogadas e o índice do jogador correspondente a essa jogada
interacoesParaJogadas :: ([Interacao], [Interacao], [Interacao]) -- ^ Interações correspondentes às teclas carregadas, premidas nesta frame, e soltas nesta frame, respetivamente
                      -> [(Jogada, Int)]
interacoesParaJogadas (((Esquerda i):iHs), iTap, iRelease) = (Movimenta E, i):(interacoesParaJogadas (iHs, iTap, iRelease))
interacoesParaJogadas (((Direita i):iHs), iTap, iRelease) = (Movimenta D, i):(interacoesParaJogadas (iHs, iTap, iRelease))
interacoesParaJogadas (((Confirma i)):iHs, iTap, iRelease) = (Acelera, i):(interacoesParaJogadas (iHs, iTap, iRelease))
interacoesParaJogadas ((iInvalida:iHs), iTap, iRelease) = interacoesParaJogadas (iHs, iTap, iRelease) 

interacoesParaJogadas (iHold, ((ICima i):iTs), iRelease) = (Movimenta C, i):(interacoesParaJogadas (iHold, iTs, iRelease))
interacoesParaJogadas (iHold, ((IBaixo i):iTs), iRelease)  = (Movimenta B, i):(interacoesParaJogadas (iHold, iTs, iRelease))
interacoesParaJogadas (iHold, ((Cancela i):iTs), iRelease) = (Dispara, i):(interacoesParaJogadas (iHold, iTs, iRelease))
interacoesParaJogadas (iHold, (iInvalida:iTs), iRelease) = interacoesParaJogadas (iHold, iTs, iRelease)

interacoesParaJogadas (iHold, iTap, ((Confirma i):iRs))  = (Desacelera, i):(interacoesParaJogadas (iHold, iTap, iRs))
interacoesParaJogadas (iHold, iTap, (iInvalida:iRs)) = interacoesParaJogadas (iHold, iTap, iRs)
interacoesParaJogadas _ = []


-- | Função que dado um valor de tempo, um estado mestre e uma lista de pares de jogadas e o índice do jogador correspondente a essa jogada,
-- devolve um estado mestre correspondente à passagem de tempo em relação ao estado mestre inicial, tendo em conta as jogadas a efetuar
-- sobre cada jogador da corrida desse estado mestre
atualizaJogo :: Float -- ^ Tempo decorrido
             -> EstadoMestre -- ^ Estado Mestre para o qual calcular passagem de tempo
             -> [(Jogada, Int)] -- ^ Lista de pares de jogadas e o índice do jogador correspondente a essa jogada
             -> EstadoMestre
atualizaJogo t estadoMestre@(EstadoMestre _ estJogo@EstadoJogo{estado = estadoAnt, cameras = cam, iHumanos = humanos, vencedores = venc} _ _ _ _ _) jogadasHumanas =
  estadoMestre{estadoJogo = estJogo{estado = estadoFinal, cameras = (moveCameras cam estadoFinal), vencedores = jogadoresNoFinal estadoFinal venc 0}}
  where
    estadoAposJogadas = executaJogadasBots (executaJogadasHumanas jogadasHumanas estadoAnt humanos) humanos 0
    estadoFinal = estadoAposJogadas {jogadoresEstado = executaPasso t estadoAposJogadas (jogadoresNoFinal estadoAposJogadas venc 0)}


-- | Função que dada uma lista de pares de jogadas e o índice do jogador correspondente a essa jogada,
-- um estado de uma corrida e um conjunto de índices dos jogadores humanos dessa corrida, devolve o resultado
-- de efetuar essas jogadas nesse estado, tendo em conta quais podem ser efetuadas com os jogadores humanos presentes
executaJogadasHumanas :: [(Jogada, Int)] -- ^ Lista de pares de jogadas e o índice do jogador correspondente a essa jogada
                      -> Estado  -- ^ Estado da corrida 
                      -> [Int] -- ^ Conjunto dos índices dos jogadores humanos desta corrida
                      -> Estado
executaJogadasHumanas ((jgd, jogador):js) estado humanos
  | elem jogador humanos = executaJogadasHumanas js (jogada jogador jgd estado) humanos
  | otherwise = executaJogadasHumanas js estado humanos 
executaJogadasHumanas [] estado humanos = estado

-- | Função que dado um estado de uma corrida, um conjunto de índices dos jogadores humanos dessa corrida
-- e um acumulador, devolve o resultado de efetuar jogadas escolhidas por um bot nesse estado, tendo em conta
-- quais jogadores são bots e quais são humanos
executaJogadasBots :: Estado -- ^ Estado da corrida
                   -> [Int] -- ^ Conjunto dos índices dos jogadores humanos desta corrida
                   -> Int -- ^ Acumulador
                   -> Estado
executaJogadasBots estado@Estado{jogadoresEstado = jogs} humanos i  
  | i == length jogs = estado
  | elem i humanos = executaJogadasBots estado humanos (i+1)
  | jogBot == Nothing = executaJogadasBots estado humanos (i+1)
  | otherwise = executaJogadasBots (jogada i (fromJust jogBot) estado) humanos (i+1)
  where
    jogBot = bot i estado


-- | Função que dado um valor de tempo, um estado de uma corrida e o conjunto dos índices dos jogadores no final desta corrida
-- devolve o resultado da passagem de tempo nesse estado segundo a função passo
executaPasso :: Float -- ^ Tempo decorrido
             -> Estado -- ^ Estado da corrida
             -> [Int] -- ^ Conjunto dos índices dos jogadores no final desta corrida
             -> [Jogador]
executaPasso t (Estado mapa (j:js)) vencedores
  | elem 0 vencedores = j:(executaPasso t (Estado mapa js) [x-1 | x <- vencedores])
  | otherwise = (passo (realToFrac t) mapa j):(executaPasso t (Estado mapa js) [x-1 | x <- vencedores])
executaPasso _ (Estado mapa []) _ = []


-- | Função que dado um EstadoJogo verifica se a corrida atual terminou, vendo se todos os jogadores
-- humanos já chegaram ao fim do mapa
determinarJogoTerminado :: EstadoJogo -> Bool
determinarJogoTerminado est@(EstadoJogo _ (Estado mapa jogadores) _ _ [h] _) = jogadorNoFinal (jogadores!!h) mapa
determinarJogoTerminado est@(EstadoJogo _ (Estado mapa jogadores) _ _ (h:hs) _) = 
  (jogadorNoFinal (jogadores!!h) mapa) && (determinarJogoTerminado est{iHumanos = hs})
determinarJogoTerminado est@(EstadoJogo _ _ _ _ [] _) = False


-- | Função que dado um estado de uma corrida devolve os índices dos jogadores no final do mapa
jogadoresNoFinal :: Estado -> [Int] -> Int -> [Int]
jogadoresNoFinal (Estado mapa (j:js)) venc n
  | jogadorNoFinal j mapa && (not (elem n venc)) = jogadoresNoFinal (Estado mapa js) (venc ++ [n]) (n+1)
  | otherwise = jogadoresNoFinal (Estado mapa js) venc (n+1)
jogadoresNoFinal (Estado _ []) venc _ = venc
 

-- | Função que dado um jogador e um mapa verifica se esse jogador está no final do mapa
jogadorNoFinal :: Jogador -> Mapa -> Bool
jogadorNoFinal (Jogador _ dist _ _ _) mapa = ((ceiling dist) == length (mapa!!0))


-- | Função que dada uma lista de cameras e um estado altera as suas coordenadas de forma a que elas acompanhem o jogador 
-- correspondente nesse estado, executando o movimento de uma forma não linear
moveCameras :: [Camera] -> Estado -> [Camera]
moveCameras ((Camera (x,y)):cs) estado@(Estado mapa (j:js)) = (Camera (xFinal, yFinal)):(moveCameras cs estado)
  where
    (xAlvo, yAlvo) = obterAlvoCamera mapa j
    xFinal = x + (xAlvo - x) / 5
    yFinal = y + (yAlvo - y) / 5
moveCameras [] _ = []


-- | Função que dado um mapae o um jogador devolve as coordenadas de uma camera que está estacionária a acompanhar o jogador
obterAlvoCamera :: Mapa -> Jogador -> (Float, Float)
obterAlvoCamera mapa jog@Jogador{pistaJogador = nPista, distanciaJogador = dist} =
  (realToFrac dist, (realToFrac (obterAlturaNaPeca (obterPeca mapa nPista dist) dist - (fromIntegral nPista))))


-- | Função que atualiza os diversos tipos de menus conforme as interações do utilizador.
atualizaMenus :: EstadoMestre -> [Interacao] -> EstadoMestre
atualizaMenus estadoMestre (i:is) = atualizaMenus (atualizaMenu estadoMestre i) is
atualizaMenus estadoMestre [] = estadoMestre

atualizaMenu :: EstadoMestre -> Interacao -> EstadoMestre
atualizaMenu estM@EstadoMestre{menus = (m@Elemento{menu = ls@Lista{colunas = col, spriteNames = sprts, indiceCursor = i }}):ms} (ICima 0) =
  estM{menus = m{ menu = ls{ indiceCursor = (normalizaCursor (i - col) sprts) }}:ms}
atualizaMenu estM@EstadoMestre{menus = (m@Elemento{menu = ls@Lista{colunas = col, spriteNames = sprts, indiceCursor = i }}):ms} (IBaixo 0) =
  estM{menus = m{ menu = ls{ indiceCursor = (normalizaCursor (i + col) sprts) }}:ms}
atualizaMenu estM@EstadoMestre{menus = (m@Elemento{menu = ls@Lista{colunas = col, spriteNames = sprts, indiceCursor = i }}):ms} (Esquerda 0) =
  estM{menus = m{ menu = ls{ indiceCursor = (normalizaCursor (i - 1) sprts) }}:ms}
atualizaMenu estM@EstadoMestre{menus = (m@Elemento{menu = ls@Lista{colunas = col, spriteNames = sprts, indiceCursor = i }}):ms} (Direita 0) =
  estM{menus = m{ menu = ls{ indiceCursor = (normalizaCursor (i + 1) sprts) }}:ms}

atualizaMenu estM@EstadoMestre{menus = m@Elemento{menu = (ScoreBoard _)}:ms, estadoJogo = estJ} (Cancela _) =
  estM{menus = [menuPrincipal, botaoPressStart], estadoJogo = estJ{emJogo = False}}

atualizaMenu estM@EstadoMestre{menus = m@Elemento{idEspecial = "mainmenu", menu = ls@Lista{indiceCursor = i}}:ms} (Confirma 0)
  | i == 0 = estM{menus = menuQuickPlay:(m:ms)}
  | i == 1 = estM{menus = menuCampanha:(m:ms)}
  | otherwise = estM{menus = menuControlos:(m:ms)}

atualizaMenu estM@EstadoMestre{menus = m@Elemento{idEspecial = "campanha", menu = ls@Lista{indiceCursor = i}}:ms} (Confirma 0) =
  estM{menus = [], estadoJogo = (EstadoJogo True (nivel i) [Camera (0,0)] (criaMotasCampanha i) [0] [])}

atualizaMenu estM@EstadoMestre{menus = m@Elemento{ idEspecial = "pstart"}:ms} (Cancela _) =
  estM
atualizaMenu estM@EstadoMestre{menus = m@Elemento{ idEspecial = "pstart"}:ms} (Confirma 0) =
  estM{menus = [menuPrincipal, m]}

atualizaMenu estM@EstadoMestre{menus = m@Elemento{menu = mqp@(MenuQP jogs opc@(OpcoesQP cpus comprimento pistas seed isOpen opcaoSelecionada) indice)}:ms} (Confirma 0)
  | isOpen && indice /= 3 = estM{menus = m{menu = mqp{opcoesqp = opc{opcaoSelecionada = (not opcaoSelecionada)}}}:ms}
  | isOpen = estM{menus =ms}
  | otherwise = estM{estadoJogo = (EstadoJogo True (Estado (gera pistas comprimento pistas) (criaJogadores jogs cpus 0)) (criaCameras jogs) (criaMotas jogs cpus) (indicesJogadoresAtivos jogs) []), menus = []}
atualizaMenu estM@EstadoMestre{menus = m@Elemento{menu = mqp@(MenuQP jogs opc@(OpcoesQP _ _ _ _ isOpen _) _)}:ms} (Confirma player)
  | isOpen = estM
  | otherwise = estM{menus = m{menu = mqp{jogadores = jogsFinal, opcoesqp = normalizaOpcoes opc (jogadoresAtivos jogsFinal)}}:ms}
  where
    jogsFinal = adicionaJogador jogs player
atualizaMenu estM@EstadoMestre{menus = m@Elemento{menu = mqp@(MenuQP jogs opc@(OpcoesQP cpus comprimento pistas seed isOpen opcaoSelecionada) _)}:ms} (Cancela player)
  | player == 0 && isOpen && opcaoSelecionada = estM{menus = m{menu = mqp{opcoesqp = opc{opcaoSelecionada = False}}}:ms}
  | player == 0 && isOpen = estM{menus = m{menu = mqp{opcoesqp = opc{isOpen = False}}}:ms}
  | player == 0 = estM{menus = ms}
  | not (isOpen || (player == 0)) = estM{menus = m{menu = mqp{jogadores = removeJogador jogs player}}:ms}
  | otherwise = estM
atualizaMenu estM@EstadoMestre{menus = m@Elemento{menu = mqp@(MenuQP jogs opc@(OpcoesQP cpus comprimento pistas seed isOpen opcaoSelecionada) cursor)}:ms} int@(Esquerda player)
  | player == 0 && isOpen && opcaoSelecionada = estM{menus = m{menu = mqp{opcoesqp = (ajustaOpcoesQP opc int cursor (jogadoresAtivos jogs))}}:ms}
  | player == 0 && isOpen = estM{menus = m{menu = mqp{indiceCursor = normalizaInt (cursor-1) (0, 2)}}:ms}
  | verificaAdicionado (jogs!!player) && (not isOpen) = estM{menus = m{menu = mqp{ jogadores = (take player jogs) ++ ((mudarMotaJogador (jogs!!player) int):(drop (player+1) jogs))}}:ms}
  | otherwise = estM
atualizaMenu estM@EstadoMestre{menus = m@Elemento{menu = mqp@(MenuQP jogs opc@(OpcoesQP cpus comprimento pistas seed isOpen opcaoSelecionada) cursor)}:ms} int@(Direita player)
  | player == 0 && isOpen && opcaoSelecionada = estM{menus = m{menu = mqp{opcoesqp = (ajustaOpcoesQP opc int cursor (jogadoresAtivos jogs))}}:ms}
  | player == 0 && isOpen = estM{menus = m{menu = mqp{indiceCursor = normalizaInt (cursor+1) (0, 3)}}:ms}
  | verificaAdicionado (jogs!!player) && (not isOpen) = estM{menus = m{menu = mqp{ jogadores = (take player jogs) ++ ((mudarMotaJogador (jogs!!player) int):(drop (player+1) jogs))}}:ms}
  | otherwise = estM
atualizaMenu estM@EstadoMestre{menus = m@Elemento{menu = mqp@MenuQP{opcoesqp = ops@OpcoesQP{opcaoSelecionada = isSelected, isOpen = open}, indiceCursor = indice}}:ms} int@(ICima 0)
  | open && (indice == 3) = estM{menus = m{menu = mqp{indiceCursor = 0}}:ms}
  | isSelected = estM
  | otherwise = estM{menus = m{menu = mqp{opcoesqp = ops{isOpen = False}}}:ms}
atualizaMenu estM@EstadoMestre{menus = m@Elemento{menu = mqp@(MenuQP  {opcoesqp = opc, indiceCursor = indice})}:ms} int@(IBaixo 0) 
  | isOpen opc = estM{menus = m{menu = mqp{indiceCursor = 3}}:ms}
  | otherwise =   estM{menus = m{menu = mqp{opcoesqp = opc{isOpen = True}}}:ms}


atualizaMenu estM@EstadoMestre{menus = m:ms} (Cancela _) = estM{menus = ms}

atualizaMenu estM i = estM


-- | Função que permite incrementar e reduzir os valores do número de pistas, comprimento e número de bots, conforme a interação do utilizador.
ajustaOpcoesQP :: OpcoesQP -> Interacao -> Int -> Int -> OpcoesQP
ajustaOpcoesQP opc@(OpcoesQP nCPUs comp pistas seed _ _) (Direita _) indice nJogs
  | indice == 0 = normalizaOpcoes (opc{nPistas = pistas + 1}) nJogs
  | indice == 2 = normalizaOpcoes (opc{nBots = nCPUs + 1}) nJogs
  | otherwise = normalizaOpcoes (opc{comprimento = comp + 1}) nJogs
ajustaOpcoesQP opc@(OpcoesQP nCPUs comp pistas seed _ _) (Esquerda _) indice nJogs
  | indice == 0 = normalizaOpcoes (opc{nPistas = pistas - 1}) nJogs
  | indice == 2 = normalizaOpcoes (opc{nBots = nCPUs - 1}) nJogs
  | otherwise = normalizaOpcoes (opc{comprimento = comp - 1}) nJogs


-- | Função que dadas as opções de um menu quickplay e o número de jogadores humanos que vão participar numa dada corrida
-- altera os valoresdas opções de forma a criar um Estado válido
normalizaOpcoes :: OpcoesQP -> Int -> OpcoesQP
normalizaOpcoes opc@(OpcoesQP nCPUs comp npistas _ _ _) nJogs =
  opc{nBots = normalizaInt nCPUs (0, npistas-nJogs), comprimento = normalizaInt comp (5, 500), nPistas = normalizaInt npistas (1,50)}


-- | Função que dado o estado de um jogador num menu quickplay e uma interação devolve o nome
-- da mota correspondente a esse jogador se este a tentar mudar 
mudarMotaJogador :: EstadoJogadorQP -> Interacao -> EstadoJogadorQP
mudarMotaJogador j@EstadoJogadorQP{spriteMota = sprite} int = j{spriteMota = percorrerMotas sprite int}


-- | Função que dado o nome de uma mota e uma interação devolve o nome da mota seguinte ou anterior
-- numa lista predefinida de motas, de acordo com a intereção executada
percorrerMotas :: String -> Interacao -> String
percorrerMotas motaAtual (Esquerda _) = listaMotas!!(normalizaInt indiceAtual (0, (length listaMotas - 1)))
  where
    listaMotas = ["redbike", "bluebike", "whitebike", "greenbike"]
    indiceAtual = indexOf motaAtual listaMotas - 1
percorrerMotas motaAtual (Direita _) = listaMotas!!(normalizaInt indiceAtual (0, (length listaMotas - 1)))
  where
    listaMotas = ["redbike", "bluebike", "whitebike", "greenbike"]
    indiceAtual = indexOf motaAtual listaMotas + 1


-- | Função que dada uma lista de estados de jogadores de um menu quickplay, o número de cpus que vão participar numa corrida
-- a um acumulador, devolve uma lista de Jogadores que vão participar nesta corrida
criaJogadores :: [EstadoJogadorQP] -> Int -> Int -> [Jogador]
criaJogadores (j@EstadoJogadorQP{isFocused = True}:js) cpus indice = (criaJogador indice):(criaJogadores js cpus (indice+1))
criaJogadores (j:js) 0 indice = criaJogadores js 0 (indice+1)
criaJogadores (j:js) cpus indice = (criaJogador indice):(criaJogadores js (cpus-1) (indice+1))
criaJogadores _ 0 _ = []
criaJogadores _ cpus indice = (criaJogador indice):(criaJogadores [] (cpus-1) (indice+1))


-- | Função que dado um inteiro devolve um Jogador cujo índice da pista é esse inteiro
criaJogador :: Int -> Jogador
criaJogador indice = Jogador indice 0 0 5 (Chao False)


-- | Função que dada uma lista de estados de jogadores de um menu quickplay devolve a lista de camera
-- de cada jogador humano que vai participar numa corrida correspondente a esse menu
criaCameras :: [EstadoJogadorQP] -> [Camera]
criaCameras (j@EstadoJogadorQP{indiceJogador = i, isFocused = True}:js) =
  Camera (0, fromIntegral i):(criaCameras js)
criaCameras (j:js) = criaCameras js
criaCameras [] = []


-- | Função que dada uma lista de estados de jogadores de um menu quickplay e o número de cpus
-- de uma corrida devolve a lista de motas de todos os jogadores dessa corrida
criaMotas :: [EstadoJogadorQP] -> Int -> [String]
criaMotas ((EstadoJogadorQP _ mota True):js) nCPUs = mota:(criaMotas js nCPUs)
criaMotas (j:js) 0 = criaMotas js 0
criaMotas (j:js) nCPUs = "cpubike":(criaMotas js (nCPUs - 1))
criaMotas [] 0 = []
criaMotas [] nCPUs = replicate nCPUs "cpubike"


-- | Função que dadas as teclas nas opções do jogo e uma lista de teclas e verificar devolve
-- as interações correspondentes às teclas a verificar
teclasParaInteracoes :: [[Key]] -> [Key] -> [Interacao]
teclasParaInteracoes teclasOpc (t:ts)
  | elem t (teclasOpc!!0) = determinarTipoInteracao (teclasOpc!!0) t 0:(teclasParaInteracoes teclasOpc ts)
  | elem t (teclasOpc!!1) = determinarTipoInteracao (teclasOpc!!1) t 1:(teclasParaInteracoes teclasOpc ts)
  | elem t (teclasOpc!!2) = determinarTipoInteracao (teclasOpc!!2) t 2:(teclasParaInteracoes teclasOpc ts)
  | elem t (teclasOpc!!3) = determinarTipoInteracao (teclasOpc!!3) t 3:(teclasParaInteracoes teclasOpc ts)
  | otherwise = teclasParaInteracoes teclasOpc ts
teclasParaInteracoes teclasOpc [] = []
  

-- |  Função que dada uma lista de teclas das opções, uma tecla e um indice de um jogador
-- devolve a interação correspondente a essa tecla
determinarTipoInteracao :: [Key] -> Key -> Int -> Interacao
determinarTipoInteracao teclasOpc t indiceJogador
  | t == teclasOpc!!0 = ICima indiceJogador
  | t == teclasOpc!!1 = Esquerda indiceJogador
  | t == teclasOpc!!2 = IBaixo indiceJogador
  | t == teclasOpc!!3 = Direita indiceJogador
  | t == teclasOpc!!4 = Confirma indiceJogador
  | otherwise = Cancela indiceJogador

-- Funções Auxiliares

-- | Função que dado um estado mestre, esvazia as teclas tap e release de um estado mestre
esvaziaTeclas :: EstadoMestre -> EstadoMestre
esvaziaTeclas e = e {teclasTap = [], teclasRelease = []}


-- | Função que dado o nome de uma imagem e um conjunto de sprites devolve a imagem com esse nome
-- dessde conjunto de imagens
carregaImagem :: String -> [Sprite] -> (Float,Float) -> Picture
carregaImagem nomeSprite ((nome, sprite):sprts) (x,y)
  | nomeSprite == nome = (Scale x y sprite)
  | otherwise = carregaImagem nomeSprite sprts (x,y)
carregaImagem _ [] _ = Blank


-- | Função que dado um elemento e uma lista verifica o índice desse elemento nessa lista, ou -1 se não existir
indexOf :: Eq a => a -> [a] -> Int
indexOf e (l:ls)
  | e == l = 0
  | otherwise = indexOf e ls + 1
indexOf e [] = -1


-- | Função que dado um inteiro e um par que corresponde a um valor máx e min,
-- devolve esse valor ou ou min se for menor que min ou max se for maior que o max
normalizaInt :: Int -> (Int, Int) -> Int
normalizaInt i (min, max)
  | i < min = min
  | i > max = max
  | otherwise = i

-- | Função que dada uma interação devolve o índice do jogador correspondente
indiceInteracao :: Interacao -> Int
indiceInteracao (ICima i) = i
indiceInteracao (Esquerda i) = i
indiceInteracao (Direita i) = i
indiceInteracao (IBaixo i) = i
indiceInteracao (Confirma i) = i
indiceInteracao (Cancela i) = i


-- | Função que dado um inteiro devolve um nivel de uma lista de niveis prefeitos
nivel :: Int -> Estado
nivel 0 = Estado [[Recta Terra 0, Recta Terra 0, Recta Terra 0, Rampa Terra 0 1, Recta Lama 1,    Recta Relva 1,     Rampa Boost 1 0, Recta Terra 0,   Rampa Terra 0 1, Rampa Boost 1 2, Recta Lama 2,  Rampa Terra 2 1, Recta Boost 1,   Rampa Terra 1 0, Recta Terra 0, Recta Terra 0,   Recta Terra 0,     Rampa Boost 0 1, Rampa Boost 1 0, Recta Terra 0,   Recta Terra 0,   Rampa Boost 0 1, Rampa Boost 1 0, Recta Terra 0,   Recta Terra 0,   Rampa Boost 0 1, Rampa Boost 1 0, Recta Terra 0, Recta Terra 0],
                 [Recta Terra 0,  Recta Terra 0, Recta Terra 0, Rampa Terra 0 1, Recta Lama 1,    Recta Relva 1,     Rampa Boost 1 0, Recta Terra 0,   Recta Terra 0,   Rampa Relva 0 1, Recta Relva 1, Recta Relva 1,   Recta Lama 1,    Rampa Terra 1 0, Recta Terra 0, Recta Terra 0,   Rampa Boost 0 1,   Rampa Boost 1 0, Recta Terra 0,   Recta Terra 0,   Rampa Boost 0 1, Rampa Boost 1 0, Recta Terra 0,   Recta Terra 0,   Rampa Boost 0 1, Rampa Boost 1 0, Recta Terra 0,   Recta Terra 0, Recta Terra 0],
                 [Recta Terra 0,  Recta Terra 0, Recta Terra 0, Rampa Terra 0 1, Recta Lama 1,    Rampa Boost 1 0,   Recta Terra 0,   Rampa Terra 0 1, Recta Terra 1,   Rampa Boost 1 2, Recta Lama 2,  Rampa Terra 2 1, Recta Boost 1,   Rampa Terra 1 0, Recta Terra 0, Rampa Boost 0 1, Rampa Boost 1 0,   Recta Terra 0,   Recta Terra 0,   Rampa Boost 0 1, Rampa Boost 1 0, Recta Terra 0,   Recta Terra 0,   Rampa Boost 0 1, Rampa Boost 1 0, Recta Terra 0,   Recta Terra 0,   Recta Terra 0, Recta Terra 0],
                 [Recta Terra 0,  Recta Terra 0, Recta Terra 0, Recta Terra 0,   Rampa Terra 0 1, Rampa Boost 1 0,   Recta Terra 0,   Recta Terra 0,   Rampa Terra 0 1, Rampa Boost 1 2, Recta Lama 2,  Recta Terra 2,   Rampa Boost 2 1, Rampa Terra 1 0, Recta Terra 0, Recta Lama 0,    Recta Lama 0,      Recta Lama 0,    Recta Lama 0,    Recta Lama 0,    Recta Lama 0,    Recta Lama 0,    Recta Lama 0,    Recta Lama 0,    Recta Lama 0,    Recta Lama 0,    Recta Lama 0,    Recta Lama 0,  Recta Lama 0]]
                 [Jogador 0 0 0 5 (Chao False), Jogador 1 0 0 5 (Chao False), Jogador 2 0 0 5 (Chao False), Jogador 3 0 0 5 (Chao False)]
nivel 1 = Estado (gera 4 40 435) [Jogador 0 0 0 5 (Chao False), Jogador 1 0 0 5 (Chao False), Jogador 2 0 0 5 (Chao False), Jogador 3 0 0 5 (Chao False)]
nivel 2 = Estado (gera 5 40 312) [Jogador 0 0 0 5 (Chao False), Jogador 1 0 0 5 (Chao False), Jogador 2 0 0 5 (Chao False), Jogador 3 0 0 5 (Chao False), Jogador 4 0 0 5 (Chao False)]
nivel 3 = Estado (gera 3 50 777) [Jogador 0 0 0 5 (Chao False), Jogador 2 0 0 5 (Chao False)]


-- | FUnção que dado um índice de um nivel devolve a lista de nomes de motas correspondente a esse nivel
criaMotasCampanha :: Int -> [String]
criaMotasCampanha 0 = "redbike":(replicate 3 "cpubike")
criaMotasCampanha 1 = "redbike":(replicate 3 "cpubike")
criaMotasCampanha 2 = "redbike":(replicate 4 "cpubike")
criaMotasCampanha 3 = "redbike":(replicate 1 "cpubike")



-- | Menu que apenas contém a imagem com os controlos do Jogo.

menuControlos:: Elemento
menuControlos = Elemento
                "controlos"                                    --  ID Especial
                True                                        -- Fullscreen
                (0,0)                                       --  Posição
                (Botao                                      --  Construtor de Tipo de Menu
                "controlos")                                   --  Sprite
