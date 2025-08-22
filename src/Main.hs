{- |
Module      : Main
Description : Parte gráfica do jogo
Copyright   : Eduardo de Oliveira Sousa Faria <a104353@alunos.uminho.pt>
              Nuno Miguel Ribeiro da Silva <a104089@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}


module Main where

import LI12223
import Tarefa1_2022li1g018
import Tarefa2_2022li1g018
import Tarefa3_2022li1g018
import Tarefa4_2022li1g018
import Tarefa5_2022li1g018
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color
import System.Random
import LI12223
import Data.Char (digitToInt)







----------------------------------------------------------- Variáveis ------------------------------------------------------
{- | Largura da janela, em pixeis -}
wW :: Float 
wW = 1100
{- | Altura da janela, em pixeis -}
wH :: Float
wH = 1100

{- | Número de quadrados que preenchem uma linha do jogo -}
numeroTiles :: Int 
numeroTiles = 11

{- | Largura dos quadrados, em proporção com a janela -}
l :: Float 
l = wW/fromIntegral numeroTiles

{- | Mapa que representa as primeiras cinco linhas (imutáveis) do jogo -}
mapa :: Mapa 
mapa = Mapa numeroTiles [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum]),
                         (Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum]), 
                         (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),
                         (Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
                         (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])]
----------------------------------------------------------------------------------------------------------------------------







----------------------------------------------------------- Data/Type ------------------------------------------------------
{- | Data que define o estado de jogo -}
data EstadoJogo = Menu       EstadoJogo EstadoJogo EstadoJogo  -- Menu recebe três estados de jogo: Jogar, Skins e Opçoes
                | Jogar      Botoes Numeros Player Fundo FundoAtivo SkinsMapa SkinMapaAtivo SkinsJogador SkinJogadorAtivo  -- Os estados de jogo Jogar, Skins e Opçoes necessitam de receber, as mesmas variáveis para estas se manterem durante as trocas entre estados de jogo
                | Skins      Botoes Numeros Player Fundo FundoAtivo SkinsMapa SkinMapaAtivo SkinsJogador BotaoSkinsSelecionado SkinJogadorAtivo  -- O estado de jogo Skins recebe, adicionalmente, o data BotaoSkinsSelecionado que serve de base para assinalar o que está a ser selecionada, naquele instante, no menu
                | Opçoes     Botoes Numeros Player Fundo FundoAtivo SkinsMapa BotaoOpçoesSelecionado SkinMapaAtivo SkinsJogador SkinJogadorAtivo  -- O estado de jogo Opçoes recebe, adicionalmente, o data BotaoOpçoesSelecionado que serve de base para assinalar o que está a ser selecionado, naquele instante, no menu
                | Pausa      Botoes BotaoPausaSlecionado   Numeros Player Fundo FundoAtivo SkinsMapa SkinMapaAtivo SkinsJogador SkinJogadorAtivo  -- BotaoPausaSlecionado é um data semelhante aos anteriores
                | PerdeuJogo Botoes BotaoPerdeuSelecionado Numeros Player Fundo FundoAtivo SkinsMapa SkinMapaAtivo SkinsJogador SkinJogadorAtivo  -- BotaoPerdeuSelecionado é um data semelhante aos anteriores




{- | Imagens Bitmap de todos os botões -}
type Botoes = [Picture] 
{- | Imagens Bitmap de todos os números -}
type Numeros = [Picture]  




{- | Player (Posição do jogador e matriz do mapa) (Ângulo do jogador) (Acumulador que permite atualizar step) (Lista de números aleatórios) (Score) (Valor que permite mudar a velocidade do ecrã) (HighScore) -}
data Player = Player Jogo Float Float [Int] Int Float Int 




{- | Imagens Bitmap de todos os fundos -}
type Fundo = [Picture] 
{- | Fundo que está ativo no jogo -}
type FundoAtivo = Picture  




{- | Conjunto de Imagens Bitmap de uma só skin -}
type SkinJogador = [Picture]  
{- | Todas as skins -}
type SkinsJogador = [SkinJogador]  
{- | Skin ativa -}
type SkinJogadorAtivo = [Picture]  




{- | Conjunto de Imagens Bitmap de um só mapa -}
type SkinMapa = [Picture]  
{- | Todos os mapas -}
type SkinsMapa = [SkinMapa]  
{- | Mapa ativo -}
type SkinMapaAtivo = [Picture] 




{- | Data que define o botão selecionado no menu das opções -}
data BotaoOpçoesSelecionado = Mapa1
                            | Mapa2
                            | Mapa3
                            | Mapa4
                            | LightMode
                            | DarkMode
                            | Facil
                            | Medio
                            | Dificil
                            | VoltarOpcao

{- | Data que define o botão selecionado no menu das skins -}
data BotaoSkinsSelecionado = Skin1
                           | Skin2
                           | Skin3
                           | Skin4
                           | Skin5
                           | Skin6
                           | VoltarSkin

{- | Data que define o botão selecionado no menu de pausa -}
data BotaoPausaSlecionado = ReiniciarPausa
                          | MenuPausa

{- | Data que define o botão selecionado no menu do jogo perdido -}
data BotaoPerdeuSelecionado = ReiniciarPerdeu
                            | MenuPerdeu
----------------------------------------------------------------------------------------------------------------------------







------------------------------------------------------- Funções Auxiliares -------------------------------------------------
{- | A função recebe um caractere, que representa um algarismo, e um estado de jogo e, tendo
em conta as imagens bitmap presentes na lista de algarismos, devolve a imagem correspondente
a esse mesmo algarismo -}
charToPicture :: Char -> EstadoJogo -> Picture
charToPicture n (Jogar _ lnumeros _ _ _ _ _ _ _ )        = (!!) lnumeros (digitToInt n)
charToPicture n (Pausa _ _ lnumeros _ _ _ _ _ _ _ )      = (!!) lnumeros (digitToInt n)
charToPicture n (PerdeuJogo _ _ lnumeros _ _ _ _ _ _ _ ) = (!!) lnumeros (digitToInt n)

{- | A função recebe uma string, que representa um número, um estado de jogo e um valor,
correspondente ao espaçamento entre algarismos, e devolve a imagem correspondente a esse
mesmo número -}
transformaScore :: String -> EstadoJogo -> Float -> Picture
transformaScore [] _ _ = Blank
transformaScore (h:t) ej n = Pictures (Translate (n-25*fromIntegral (length (h:t)-1)) 0 (charToPicture h ej) : [transformaScore t ej (n+25)]) -- n começa em 0 e, de acordo com o número de algarismos, este permite centrá-los e espaçá-los  



{- | A função recebe um valor, que servirá de acumulador, e uma imagem, i.é., um tile,
e devolve uma linha/lista composta pela consecutiva repetição dessa imagem, da esquerda
para a direita, ao longo da largura do ecrã -}
repeatTiles :: Float -> Picture -> [Picture]
repeatTiles n p
    | -wW/2 + n > wW/2 = []  -- caso de paragem, para quando uma imagem fosse sair do ecrã
    | otherwise = Translate (-wW/2 + n) 0 p : repeatTiles (n+l) p  -- o n é incrementado por l, ou seja pela largura de um tile

{- | A função recebe uma imagem, i.é, um tile e, utilizando a função repeatTiles, transforma
a linha/lista numa só imagem -}
juntaTiles :: Picture -> Picture
juntaTiles p = Pictures (repeatTiles (l/2) p)adBMP "Mapas/MapaNatal/MapaNatal_Arvore.bmp"

    numeroZ <- loadBMP "Numeros/Zero.bmp"
    numeroU <- loadBMP "Numeros/Um.bmp"
    numeroD <- loadBMP "Numeros/Dois.bmp"
    numeroT <- loadBMP "Numeros/Tres.bmp"
    numeroQ <- loadBMP "Numeros/Quatro.bmp"
    numeroC <- loadBMP "Numeros/Cinco.bmp"
    numeroSei <- loadBM
----------------------------------------------------------------------------------------------------------------------------







------------------------------------------------------- Desenho do mapa ----------------------------------------------------
{- | Esta função recebe um estado de jogo, que contem um mapa, e um valor, que servirá de acumulador
e, utilizando as funções transformaTerreno e transformaObstaculos, desenha todo esse mapa, de cima
para baixo, ao longo do ecrã. Nesta função é necessário o pattern matching para os estados de jogo
Jogar, Pausa e PerdeuJogo, uma vez que todos estes requerem os desenhos das mesmas imagens -}
transformaMatriz :: EstadoJogo -> Float -> Picture

transformaMatriz (Jogar _ _ (Player (Jogo _ (Mapa _ [])) _ _ _ _ _ _) _ _ _ _ _ _) _ = Blank

transformaMatriz ej@(Jogar lbotoes lnumeros (Player (Jogo jogador (Mapa largura ((Estrada v,obstaculos):t))) a contador r score dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) n = Pictures (Translate 0 (wH/2-n) (transformaTerreno (Estrada v) ej) : Translate 0 (wH/2-n) (transformaObstaculos obstaculos v (l/2) ej) : [transformaMatriz (Jogar lbotoes lnumeros (Player (Jogo jogador (Mapa largura t)) a contador r score dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) (n+l)])
transformaMatriz ej@(Jogar lbotoes lnumeros (Player (Jogo jogador (Mapa largura ((Rio v,obstaculos):t))) a contador r score dificuldade highScore)     fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) n = Pictures (Translate 0 (wH/2-n) (transformaTerreno (Rio v) ej)     : Translate 0 (wH/2-n) (transformaObstaculos obstaculos v (l/2) ej) : [transformaMatriz (Jogar lbotoes lnumeros (Player (Jogo jogador (Mapa largura t)) a contador r score dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) (n+l)])
transformaMatriz ej@(Jogar lbotoes lnumeros (Player (Jogo jogador (Mapa largura ((Relva,obstaculos):t))) a contador r score dificuldade highScore)     fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) n = Pictures (Translate 0 (wH/2-n) (transformaTerreno Relva ej)       : Translate 0 (wH/2-n) (transformaObstaculos obstaculos 0 (l/2) ej) : [transformaMatriz (Jogar lbotoes lnumeros (Player (Jogo jogador (Mapa largura t)) a contador r score dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) (n+l)])

transformaMatriz (Pausa _ _ _ (Player (Jogo _ (Mapa _ [])) _ _ _ _ _ _) _ _ _ _ _ _) _ = Blank

transformaMatriz ej@(Pausa lbotoes botaoSelecionado lnumeros (Player (Jogo jogador (Mapa largura ((Estrada v,obstaculos):t))) a contador r score dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) n = Pictures (Translate 0 (wH/2-n) (transformaTerreno (Estrada v) ej) : Translate 0 (wH/2-n) (transformaObstaculos obstaculos v (l/2) ej) : [transformaMatriz (Pausa lbotoes botaoSelecionado lnumeros (Player (Jogo jogador (Mapa largura t)) a contador r score dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) (n+l)])
transformaMatriz ej@(Pausa lbotoes botaoSelecionado lnumeros (Player (Jogo jogador (Mapa largura ((Rio v,obstaculos):t))) a contador r score dificuldade highScore)     fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) n = Pictures (Translate 0 (wH/2-n) (transformaTerreno (Rio v) ej)     : Translate 0 (wH/2-n) (transformaObstaculos obstaculos v (l/2) ej) : [transformaMatriz (Pausa lbotoes botaoSelecionado lnumeros (Player (Jogo jogador (Mapa largura t)) a contador r score dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) (n+l)])
transformaMatriz ej@(Pausa lbotoes botaoSelecionado lnumeros (Player (Jogo jogador (Mapa largura ((Relva,obstaculos):t))) a contador r score dificuldade highScore)     fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) n = Pictures (Translate 0 (wH/2-n) (transformaTerreno Relva ej)       : Translate 0 (wH/2-n) (transformaObstaculos obstaculos 0 (l/2) ej) : [transformaMatriz (Pausa lbotoes botaoSelecionado lnumeros (Player (Jogo jogador (Mapa largura t)) a contador r score dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) (n+l)])


transformaMatriz (PerdeuJogo _ _ _ (Player (Jogo _ (Mapa _ [])) _ _ _ _ _ _) _ _ _ _ _ _) _ = Blank

transformaMatriz ej@(PerdeuJogo lbotoes botaoSelecionado lnumeros (Player (Jogo jogador (Mapa largura ((Estrada v,obstaculos):t))) a contador r score dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) n = Pictures (Translate 0 (wH/2-n) (transformaTerreno (Estrada v) ej) : Translate 0 (wH/2-n) (transformaObstaculos obstaculos v (l/2) ej) : [transformaMatriz (PerdeuJogo lbotoes botaoSelecionado lnumeros (Player (Jogo jogador (Mapa largura t)) a contador r score dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) (n+l)])
transformaMatriz ej@(PerdeuJogo lbotoes botaoSelecionado lnumeros (Player (Jogo jogador (Mapa largura ((Rio v,obstaculos):t))) a contador r score dificuldade highScore)     fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) n = Pictures (Translate 0 (wH/2-n) (transformaTerreno (Rio v) ej)     : Translate 0 (wH/2-n) (transformaObstaculos obstaculos v (l/2) ej) : [transformaMatriz (PerdeuJogo lbotoes botaoSelecionado lnumeros (Player (Jogo jogador (Mapa largura t)) a contador r score dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) (n+l)])
transformaMatriz ej@(PerdeuJogo lbotoes botaoSelecionado lnumeros (Player (Jogo jogador (Mapa largura ((Relva,obstaculos):t))) a contador r score dificuldade highScore)     fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) n = Pictures (Translate 0 (wH/2-n) (transformaTerreno Relva ej)       : Translate 0 (wH/2-n) (transformaObstaculos obstaculos 0 (l/2) ej) : [transformaMatriz (PerdeuJogo lbotoes botaoSelecionado lnumeros (Player (Jogo jogador (Mapa largura t)) a contador r score dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinsJogador skinJogadorAtivo) (n+l)])




{- | Esta função recebe um terreno e um estado de jogo e, consoante o tipo de terreno
e o mapa ativo, utiliza a função juntaTiles para desenhar uma linha desse terreno -}
transformaTerreno :: Terreno -> EstadoJogo -> Picture

transformaTerreno (Rio _)     (Jogar _ _ _ _ _ _ skinMapaAtivo _ _)        = juntaTiles (scale (1/3) (1/3) ((!!) skinMapaAtivo 0))
transformaTerreno (Estrada _) (Jogar _ _ _ _ _ _ skinMapaAtivo _ _)        = juntaTiles (scale (1/3) (1/3) ((!!) skinMapaAtivo 1))
transformaTerreno Relva       (Jogar _ _ _ _ _ _ skinMapaAtivo _ _)        = juntaTiles (scale (1/3) (1/3) ((!!) skinMapaAtivo 2))

transformaTerreno (Rio _)     (Pausa _ _ _ _ _ _ _ skinMapaAtivo _ _)      = juntaTiles (scale (1/3) (1/3) ((!!) skinMapaAtivo 0))
transformaTerreno (Estrada _) (Pausa _ _ _ _ _ _ _ skinMapaAtivo _ _)      = juntaTiles (scale (1/3) (1/3) ((!!) skinMapaAtivo 1))
transformaTerreno Relva       (Pausa _ _ _ _ _ _ _ skinMapaAtivo _ _)      = juntaTiles (scale (1/3) (1/3) ((!!) skinMapaAtivo 2))

transformaTerreno (Rio _)     (PerdeuJogo _ _ _ _ _ _ _ skinMapaAtivo _ _) = juntaTiles (scale (1/3) (1/3) ((!!) skinMapaAtivo 0))
transformaTerreno (Estrada _) (PerdeuJogo _ _ _ _ _ _ _ skinMapaAtivo _ _) = juntaTiles (scale (1/3) (1/3) ((!!) skinMapaAtivo 1))
transformaTerreno Relva       (PerdeuJogo _ _ _ _ _ _ _ skinMapaAtivo _ _) = juntaTiles (scale (1/3) (1/3) ((!!) skinMapaAtivo 2))




{- | Esta função recebe uma lista de obstáculos, um valor que representa a velocidade,
um valor que servirá de acumulador, e um estado de jogo e, consoante o tipo de obstáculo
e o mapa ativo, desenha uma linha formada pelos obstáculos, da esquerda para a direita, ao
longo do ecrã -}
transformaObstaculos :: [Obstaculo] -> Int -> Float -> EstadoJogo -> Picture

transformaObstaculos [] _ _ _ = Blank
transformaObstaculos (Nenhum:t) v n ej = Pictures [transformaObstaculos t v (n+l) ej]  -- quando não há um obstáculo nada é desenhado 


transformaObstaculos (Tronco:t) v n ej@(Jogar _ _ _ _ _ _ skinMapaAtivo _ _) = Pictures (Translate (-wW/2 + n) 0 (scale (1/3) (1/3) ((!!) skinMapaAtivo 3)) : [transformaObstaculos t v (n+l) ej])  -- o n é incrementado por l, ou seja pela largura de um tile
transformaObstaculos (Carro:t)  v n ej@(Jogar _ _ _ _ _ _ skinMapaAtivo _ _)
    | v > 0 = Pictures (Translate (-wW/2 + n) 0 (scale (1/3) (1/3) ((!!) skinMapaAtivo 4))  : [transformaObstaculos t v (n+l) ej])  -- se os carros assumirem a direção direita (sinal da velocidade positivo), é escolhida a imagem de índice 4 para representar os mesmos
    | v < 0 = Pictures (Translate (-wW/2 + n) 0 (scale (1/3) (1/3) ((!!) skinMapaAtivo 5))  : [transformaObstaculos t v (n+l) ej])  -- se os carros assumirem a direção esquerda (sinal da velocidade negativo), é escolhida a imagem de índice 5 para representar os mesmos
transformaObstaculos (Arvore:t) v n ej@(Jogar _ _ _ _ _ _ skinMapaAtivo _ _) = Pictures (Translate (-wW/2 + n) 0 (Translate 0 (0.20*l) (scale (1/3) (2/5) ((!!) skinMapaAtivo 6))) : [transformaObstaculos t v (n+l) ej])


transformaObstaculos (Tronco:t) v n ej@(Pausa _ _ _ _ _ _ _ skinMapaAtivo _ _) = Pictures (Translate (-wW/2 + n) 0 (scale (1/3) (1/3) ((!!) skinMapaAtivo 3)) : [transformaObstaculos t v (n+l) ej])
transformaObstaculos (Carro:t)  v n ej@(Pausa _ _ _ _ _ _ _ skinMapaAtivo _ _)
    | v > 0 = Pictures (Translate (-wW/2 + n) 0 (scale (1/3) (1/3) ((!!) skinMapaAtivo 4))  : [transformaObstaculos t v (n+l) ej])
    | v < 0 = Pictures (Translate (-wW/2 + n) 0 (scale (1/3) (1/3) ((!!) skinMapaAtivo 5))  : [transformaObstaculos t v (n+l) ej])
transformaObstaculos (Arvore:t) v n ej@(Pausa _ _ _ _ _ _ _ skinMapaAtivo _ _) = Pictures (Translate (-wW/2 + n) 0 (Translate 0 (0.20*l) (scale (1/3) (2/5) ((!!) skinMapaAtivo 6))) : [transformaObstaculos t v (n+l) ej])

transformaObstaculos (Tronco:t) v n ej@(PerdeuJogo _ _ _ _ _ _ _ skinMapaAtivo _ _) = Pictures (Translate (-wW/2 + n) 0 (scale (1/3) (1/3) ((!!) skinMapaAtivo 3)) : [transformaObstaculos t v (n+l) ej])
transformaObstaculos (Carro:t)  v n ej@(PerdeuJogo _ _ _ _ _ _ _ skinMapaAtivo _ _)
    | v > 0 = Pictures (Translate (-wW/2 + n) 0 (scale (1/3) (1/3) ((!!) skinMapaAtivo 4))  : [transformaObstaculos t v (n+l) ej])
    | v < 0 = Pictures (Translate (-wW/2 + n) 0 (scale (1/3) (1/3) ((!!) skinMapaAtivo 5))  : [transformaObstaculos t v (n+l) ej])
transformaObstaculos (Arvore:t) v n ej@(PerdeuJogo _ _ _ _ _ _ _ skinMapaAtivo _ _) = Pictures (Translate (-wW/2 + n) 0 (Translate 0 (0.20*l) (scale (1/3) (2/5) ((!!) skinMapaAtivo 6))) : [transformaObstaculos t v (n+l) ej])




{- | Esta função recebe um mapa, uma lista de inteiros e um valor n e, utilizando a função estendeMapa
recursivamente, gera e adiciona, ao mapa inicial, n linhas aleatórias -}
estendeMapaRecursiva :: Mapa -> [Int] -> Int -> Mapa
estendeMapaRecursiva mapa _ 0 = mapa
estendeMapaRecursiva mapa lrandom n = estendeMapaRecursiva (estendeMapa mapa lrandom) (drop numeroTiles lrandom) (n-1)  -- para garantir a aleatoriedade numa linha, dá-se o drop do numeroTiles (11) cada vez que uma nova linha é formada. Tal estratégia também é utilizada na função step
----------------------------------------------------------------------------------------------------------------------------







--------------------------------------------------- Funções Principais -----------------------------------------------------

{- | Esta função recebe um estado de jogo e, sobrepondo sucessivamente imagens bitmap, 
devolve o desenho desse respetivo estado. Em alguns estados de jogo, é necessário recorrer
ao pattern matching para demonstrar qual o botão/skin/mapa que está a ser selecionado -}
drawWorld :: EstadoJogo -> Picture


-- Desenho do Menu

drawWorld (Menu (Jogar lbotoes _ _ fundos fundoAtivo _ _ _ _) _ _)     = Pictures (fundoAtivo : Translate 0 (-30)  ((!!) lbotoes 1) : Translate 0 (-150) ((!!) lbotoes 4) : Translate 0 (-270) ((!!) lbotoes 6) : [Translate 0 270 ((!!) fundos 2)])  -- botão Jogar selecionado
drawWorld (Menu (Skins lbotoes _ _ fundos fundoAtivo _ _ _ _ _)  _ _)  = Pictures (fundoAtivo : Translate 0 (-30)  ((!!) lbotoes 2) : Translate 0 (-150) ((!!) lbotoes 3) : Translate 0 (-270) ((!!) lbotoes 6) : [Translate 0 270 ((!!) fundos 2)])  -- botão Skins selecionado
drawWorld (Menu (Opçoes lbotoes _ _ fundos fundoAtivo _ _ _ _ _) _ _)  = Pictures (fundoAtivo : Translate 0 (-30)  ((!!) lbotoes 2) : Translate 0 (-150) ((!!) lbotoes 4) : Translate 0 (-270) ((!!) lbotoes 5) : [Translate 0 270 ((!!) fundos 2)])  -- botão Opçoes selecionado




-- Desenho do Jogar

drawWorld ej@(Jogar _ lnumeros (Player (Jogo (Jogador (x,y)) _) a n _ score _ _) _ fundoAtivo _ skinMapaAtivo _ skinJogadorAtivo) = Pictures (desenhoBkg : desenhoMapa : desenhoJogador : desenhoScore)
    where desenhoBkg  = fundoAtivo
          desenhoMapa = Translate 0 (n+l) (transformaMatriz ej (l/2))  -- o contador n, que diminui segundo a função step, é utilizado, de igual forma, nas ordenadas dos Translates associados ao mapa e ao jogador, fazendo com que ambos sofram translações, consecutivas, para baixo, provocando uma sensação de deslize da câmara. Dá-se também um translate por l no y (do mapa e do jogador) para assegurar que nunca é vista a criação de uma nova linha
          desenhoJogador  -- em todos os casos, a posição do jogador leva translate por (-wW/2+l/2 + fromIntegral x*l) no x e por (wH/2-l/2 + n - fromIntegral y*l + l) no y. Estas expressões garantem que o jogador esteje alinhado com a grelha e que as suas coordendas possam ser expressas pela posição do quadrado em que se encontra, onde as coordenadas (0,0) corresponde ao centro do tile que se encontra no canto superior esquerdo
            | length skinJogadorAtivo == 2 && (a == 0 || a == 90)      = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 0)))  -- quando a length de skinJogadorAtivo é 2 e o ângulo é 0 ou 90 graus, o jogador vira-se para a direita
            | length skinJogadorAtivo == 2 && (a == 180 || a == (-90)) = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 1)))  -- quando a length de skinJogadorAtivo é 2 e o ângulo é -90 ou 180 graus, o jogador vira-se para a esquerda
            | a == 0     = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 0)))  -- quando a length de skinJogadorAtivo é 4 e o ângulo é 0 graus, o jogador vira-se para cima
            | a == 180   = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 1)))  -- quando a length de skinJogadorAtivo é 4 e o ângulo é 180 graus, o jogador vira-se para baixo
            | a == (-90) = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 2)))  -- quando a length de skinJogadorAtivo é 4 e o ângulo é -90 graus, o jogador vira-se para a esquerda
            | a == 90    = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 3)))  -- quando a length de skinJogadorAtivo é 4 e o ângulo é 90 graus, o jogador vira-se para a direita
          desenhoScore   = [Translate 0 (wH/3) (transformaScore (show (div score 30)) ej 0)]  -- uma vez que o jogo atualiza 120 vezes por segundo, é feito um div na variavel s, por 30, para causar um aumento mais lento do score




-- Desenho da Pausa

drawWorld ej@(Pausa lbotoes ReiniciarPausa lnumeros (Player (Jogo (Jogador (x,y)) _) a n _ score _ _) _ fundoAtivo _ skinMapaAtivo _ skinJogadorAtivo) = Pictures (desenhoBkg : desenhoMapa : desenhoJogador : desenhoPausa : desenhoScore)  -- botão Reiniciar selecionado
    where desenhoBkg  = fundoAtivo
          desenhoMapa = Translate 0 (l+n) (transformaMatriz ej (l/2))  -- no menu pausa, não é necessário o uso do valor n para dar translate ao jogador e ao mapa, uma vez que este não varia
          desenhoJogador  
            | length skinJogadorAtivo == 2 && (a == 0 || a == 90)      = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 0)))
            | length skinJogadorAtivo == 2 && (a == 180 || a == (-90)) = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 1)))
            | a == 0     = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 0)))
            | a == 180   = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 1)))
            | a == (-90) = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 2)))
            | a == 90    = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 3)))        
          desenhoPausa   = Pictures (Color (makeColorI 255 255 255 100) (rectangleSolid wW wH) : Translate 0 250 ((!!) lbotoes 0) : Translate 0 (-180) ((!!) lbotoes 9) : [Translate 0 (-300) ((!!) lbotoes 8)])  -- desenha-se o menu de pausa sobreposto ao mapa que está a ser jogado
          desenhoScore   = [scale (1.5) (1.5) (transformaScore (show (div score 30)) ej 0)]  -- o score já nao se encontra junto com o mapa, mas sim no menu de pausa

drawWorld ej@(Pausa lbotoes MenuPausa lnumeros (Player (Jogo (Jogador (x,y)) _) a n _ score _ _) _ fundoAtivo _ skinMapaAtivo _ skinJogadorAtivo) = Pictures (desenhoBkg : desenhoMapa : desenhoJogador : desenhoPausa : desenhoScore)  -- botão Menu selecionado
    where desenhoBkg  = fundoAtivo
          desenhoMapa = Translate 0 (l+n) (transformaMatriz ej (l/2))
          desenhoJogador
            | length skinJogadorAtivo == 2 && (a == 0 || a == 90)      = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 0)))
            | length skinJogadorAtivo == 2 && (a == 180 || a == (-90)) = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 1)))
            | a == 0     = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 0)))
            | a == 180   = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 1)))
            | a == (-90) = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 2)))
            | a == 90    = Translate (-wW/2+l/2 + fromIntegral x*l) (wH/2-l/2 + n - fromIntegral y*l + l) (Translate 0 ((1/9)*l) (scale (0.2) (0.2) ((!!) skinJogadorAtivo 3)))        
          desenhoPausa   = Pictures (Color (makeColorI 255 255 255 100) (rectangleSolid wW wH) : Translate 0 250 ((!!) lbotoes 0) : Translate 0 (-180) ((!!) lbotoes 10) : [Translate 0 (-300) ((!!) lbotoes 7)])   
          desenhoScore   = [scale (1.5) (1.5) (transformaScore (show (div score 30)) ej 0)]




-- Desenho do Game Over

drawWorld ej@(PerdeuJogo lbotoes ReiniciarPerdeu lnumeros (Player (Jogo (Jogador (x,y)) _) a n _ score _ highScore) fundos fundoAtivo _ skinMapaAtivo _ skinJogadorAtivo) = Pictures (desenhoBkg : desenhoMapa : desenhoPerdeu : desenhoScore : desenhoHighScore)  -- botão Reiniciar selecionado
    where desenhoBkg  = fundoAtivo
          desenhoMapa = Translate 0 (l+n) (transformaMatriz ej (l/2))        
          desenhoPerdeu  = Pictures (Color (makeColorI 255 255 255 100) (rectangleSolid wW wH) : Translate 0 300 ((!!) fundos 7) : Translate 0 (-180) ((!!) lbotoes 9) : [Translate 0 (-300) ((!!) lbotoes 8)])  -- desenha-se o menu de game over sobreposto ao mapa que estava a ser jogado
          desenhoScore   = Pictures (Translate (-wH/6) (-50) (Scale (1.2) (1.2) (transformaScore (show (div score 30)) ej 0)) : [Translate (-wH/6) (30) ((!!) fundos 8)])  -- o score já nao se encontra junto com o mapa, mas sim no menu de game over
          desenhoHighScore
             | score >= highScore = Translate (wH/6) (-50) (Scale (1.2) (1.2) (transformaScore (show (div score 30)) ej 0)) : [Translate (wH/6) (30) ((!!) fundos 9)]  -- caso o valor do score seja superior ou igual ao high score, este passa a ser o novo high score
             | otherwise = Translate (wH/6) (-50) (Scale (1.2) (1.2) (transformaScore (show (div highScore 30)) ej 0)) : [Translate (wH/6) (30) ((!!) fundos 9)]  -- caso o valor do high score seja superior ao score, este permanece igual

drawWorld ej@(PerdeuJogo lbotoes MenuPerdeu lnumeros (Player (Jogo (Jogador (x,y)) _) a n _ score _ highScore) fundos fundoAtivo _ skinMapaAtivo _ skinJogadorAtivo) = Pictures (desenhoBkg : desenhoMapa : desenhoPerdeu : desenhoScore : desenhoHighScore)  -- botão Menu selecionado
    where desenhoBkg  = fundoAtivo
          desenhoMapa = Translate 0 (l+n) (transformaMatriz ej (l/2))        
          desenhoPerdeu  = Pictures (Color (makeColorI 255 255 255 100) (rectangleSolid wW wH) : Translate 0 300 ((!!) fundos 7) : Translate 0 (-180) ((!!) lbotoes 10) : [Translate 0 (-300) ((!!) lbotoes 7)])   
          desenhoScore   = Pictures (Translate (-wH/6) (-50) (Scale (1.2) (1.2) (transformaScore (show (div score 30)) ej 0)) : [Translate (-wH/6) (30) ((!!) fundos 8)])
          desenhoHighScore   
             | score >= highScore = Translate (wH/6) (-50) (Scale (1.2) (1.2) (transformaScore (show (div score 30)) ej 0)) : [Translate (wH/6) (30) ((!!) fundos 9)]
             | otherwise = Translate (wH/6) (-50) (Scale (1.2) (1.2) (transformaScore (show (div highScore 30)) ej 0)) : [Translate (wH/6) (30) ((!!) fundos 9)]




-- Desenho do Menu Skins (skins selecionadas encontram-se maiores)

drawWorld (Skins lbotoes _ _ _ fundoAtivo _ _ skinsJogador Skin1 skinJogadorativo)  -- Skin1 selecionada        ------------ fundo ------------------------------ retângulo transparente -------------------------------------------------- seta da skin selecionada ----------------------------- seta da skin ativa ----------------------------------------------------------- skin1 --------------------------------------------------------------------- skin2 -------------------------------------------------------------------------------------- skin3 ----------------------------------------------------------------------- skin4 ---------------------------------------------------------------------- skin5 ------------------------------------------------------------------------------ skin6 ------------------------------------------------ botão voltar -------------
                                                                    | skinJogadorativo == (!!) skinsJogador 0 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (-wW*0.35) (wH*0.2) ((!!) lbotoes 13)  : Translate (-wW*0.35) (wH*0.2) ((!!) lbotoes 14)  : Translate (-wW*0.35) (wH*0.3) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 1 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (-wW*0.35) (wH*0.2) ((!!) lbotoes 13)  : Translate 0 (wH*0.2) ((!!) lbotoes 14)           : Translate (-wW*0.35) (wH*0.3) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 2 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (-wW*0.35) (wH*0.2) ((!!) lbotoes 13)  : Translate (wW*0.35) (wH*0.2) ((!!) lbotoes 14)   : Translate (-wW*0.35) (wH*0.3) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 3 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (-wW*0.35) (wH*0.2) ((!!) lbotoes 13)  : Translate (-wW*0.35) (-wH*0.1) ((!!) lbotoes 14) : Translate (-wW*0.35) (wH*0.3) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 4 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (-wW*0.35) (wH*0.2) ((!!) lbotoes 13)  : Translate 0 (-wH*0.1) ((!!) lbotoes 14)          : Translate (-wW*0.35) (wH*0.3) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 5 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (-wW*0.35) (wH*0.2) ((!!) lbotoes 13)  : Translate (wW*0.35) (-wH*0.1) ((!!) lbotoes 14)  : Translate (-wW*0.35) (wH*0.3) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
drawWorld (Skins lbotoes _ _ _ fundoAtivo _ _ skinsJogador Skin2 skinJogadorativo)  -- Skin2 selecionada
                                                                    | skinJogadorativo == (!!) skinsJogador 0 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate 0 (wH*0.2) ((!!) lbotoes 13)           : Translate (-wW*0.35) (wH*0.2) ((!!) lbotoes 14)  : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 1 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate 0 (wH*0.2) ((!!) lbotoes 13)           : Translate 0 (wH*0.2) ((!!) lbotoes 14)           : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 2 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate 0 (wH*0.2) ((!!) lbotoes 13)           : Translate (wW*0.35) (wH*0.2) ((!!) lbotoes 14)   : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 3 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate 0 (wH*0.2) ((!!) lbotoes 13)           : Translate (-wW*0.35) (-wH*0.1) ((!!) lbotoes 14) : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 4 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate 0 (wH*0.2) ((!!) lbotoes 13)           : Translate 0 (-wH*0.1) ((!!) lbotoes 14)          : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 5 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate 0 (wH*0.2) ((!!) lbotoes 13)           : Translate (wW*0.35) (-wH*0.1) ((!!) lbotoes 14)  : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
drawWorld (Skins lbotoes _ _ _ fundoAtivo _ _ skinsJogador Skin3 skinJogadorativo)  -- Skin3 selecionada
                                                                    | skinJogadorativo == (!!) skinsJogador 0 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (wW*0.35) (wH*0.2) ((!!) lbotoes 13)   : Translate (-wW*0.35) (wH*0.2) ((!!) lbotoes 14)  : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 1 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (wW*0.35) (wH*0.2) ((!!) lbotoes 13)   : Translate 0 (wH*0.2) ((!!) lbotoes 14)           : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 2 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (wW*0.35) (wH*0.2) ((!!) lbotoes 13)   : Translate (wW*0.35) (wH*0.2) ((!!) lbotoes 14)   : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 3 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (wW*0.35) (wH*0.2) ((!!) lbotoes 13)   : Translate (-wW*0.35) (-wH*0.1) ((!!) lbotoes 14) : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 4 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (wW*0.35) (wH*0.2) ((!!) lbotoes 13)   : Translate 0 (-wH*0.1) ((!!) lbotoes 14)          : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 5 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (wW*0.35) (wH*0.2) ((!!) lbotoes 13)   : Translate (wW*0.35) (-wH*0.1) ((!!) lbotoes 14)  : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
drawWorld (Skins lbotoes _ _ _ fundoAtivo _ _ skinsJogador Skin4 skinJogadorativo)  -- Skin4 selecionada
                                                                    | skinJogadorativo == (!!) skinsJogador 0 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (-wW*0.35) (-wH*0.1) ((!!) lbotoes 13) : Translate (-wW*0.35) (wH*0.2) ((!!) lbotoes 14)  : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 1 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (-wW*0.35) (-wH*0.1) ((!!) lbotoes 13) : Translate 0 (wH*0.2) ((!!) lbotoes 14)           : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 2 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (-wW*0.35) (-wH*0.1) ((!!) lbotoes 13) : Translate (wW*0.35) (wH*0.2) ((!!) lbotoes 14)   : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 3 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (-wW*0.35) (-wH*0.1) ((!!) lbotoes 13) : Translate (-wW*0.35) (-wW*0.1) ((!!) lbotoes 14) : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 4 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (-wW*0.35) (-wH*0.1) ((!!) lbotoes 13) : Translate 0 (-wW*0.1) ((!!) lbotoes 14)          : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 5 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (-wW*0.35) (-wH*0.1) ((!!) lbotoes 13) : Translate (wW*0.35) (-wW*0.1) ((!!) lbotoes 14)  : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
drawWorld (Skins lbotoes _ _ _ fundoAtivo _ _ skinsJogador Skin5 skinJogadorativo)  -- Skin5 selecionada
                                                                    | skinJogadorativo == (!!) skinsJogador 0 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate 0 (-wH*0.1) ((!!) lbotoes 13)          : Translate (-wW*0.35) (wH*0.2) ((!!) lbotoes 14)  : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 1 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate 0 (-wH*0.1) ((!!) lbotoes 13)          : Translate 0 (wH*0.2) ((!!) lbotoes 14)           : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 2 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate 0 (-wH*0.1) ((!!) lbotoes 13)          : Translate (wW*0.35) (wH*0.2) ((!!) lbotoes 14)   : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 3 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate 0 (-wH*0.1) ((!!) lbotoes 13)          : Translate (-wW*0.35) (-wW*0.1) ((!!) lbotoes 14) : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 4 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate 0 (-wH*0.1) ((!!) lbotoes 13)          : Translate 0 (-wW*0.1) ((!!) lbotoes 14)          : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 5 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate 0 (-wH*0.1) ((!!) lbotoes 13)          : Translate (wW*0.35) (-wW*0.1) ((!!) lbotoes 14)  : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
drawWorld (Skins lbotoes _ _ _ fundoAtivo _ _ skinsJogador Skin6 skinJogadorativo)  -- Skin6 selecionada
                                                                    | skinJogadorativo == (!!) skinsJogador 0 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (wW*0.35) (-wH*0.1) ((!!) lbotoes 13)  : Translate (-wW*0.35) (wH*0.2) ((!!) lbotoes 14)  : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 1 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (wW*0.35) (-wH*0.1) ((!!) lbotoes 13)  : Translate 0 (wH*0.2) ((!!) lbotoes 14)           : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 2 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (wW*0.35) (-wH*0.1) ((!!) lbotoes 13)  : Translate (wW*0.35) (wH*0.2) ((!!) lbotoes 14)   : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 3 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (wW*0.35) (-wH*0.1) ((!!) lbotoes 13)  : Translate (-wW*0.35) (-wW*0.1) ((!!) lbotoes 14) : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 4 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (wW*0.35) (-wH*0.1) ((!!) lbotoes 13)  : Translate 0 (-wW*0.1) ((!!) lbotoes 14)          : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
                                                                    | skinJogadorativo == (!!) skinsJogador 5 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6))) : Translate (wW*0.35) (-wH*0.1) ((!!) lbotoes 13)  : Translate (wW*0.35) (-wW*0.1) ((!!) lbotoes 14)  : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.35) (0.35) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 12)])
drawWorld (Skins lbotoes _ _ _ fundoAtivo _ _ skinsJogador VoltarSkin skinJogadorativo)  -- botao Voltar selecionado
                                                                    | skinJogadorativo == (!!) skinsJogador 0 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6)))                                                    : Translate (-wW*0.35) (wH*0.2) ((!!) lbotoes 14)  : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 11)])
                                                                    | skinJogadorativo == (!!) skinsJogador 1 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6)))                                                    : Translate 0 (wH*0.2) ((!!) lbotoes 14)           : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 11)])
                                                                    | skinJogadorativo == (!!) skinsJogador 2 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6)))                                                    : Translate (wW*0.35) (wH*0.2) ((!!) lbotoes 14)   : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 11)])
                                                                    | skinJogadorativo == (!!) skinsJogador 3 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6)))                                                    : Translate (-wW*0.35) (-wW*0.1) ((!!) lbotoes 14) : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 11)])
                                                                    | skinJogadorativo == (!!) skinsJogador 4 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6)))                                                    : Translate 0 (-wW*0.1) ((!!) lbotoes 14)          : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 11)])
                                                                    | skinJogadorativo == (!!) skinsJogador 5 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/4) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.6)))                                                    : Translate (wW*0.35) (-wW*0.1) ((!!) lbotoes 14)  : Translate (-wW*0.35) (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 0) 1)) : Translate 0 (wH*0.3) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 1) 1)) : Translate (wW*0.35) (wH*0.3-(0.15*l)) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 2) 1)) : Translate (-wW*0.35) 0 (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 3) 1)) : Translate 0 (-0.12*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 4) 0)) : Translate (wW*0.35) (-0.05*l) (scale (0.25) (0.25) ((!!) ((!!) skinsJogador 5) 0)) : [ Translate 0 (-390) ((!!) lbotoes 11)])




-- Desenho do Menu Opçoes (Mapas/BotõesFundo/BotõesDificuldade selecionadas encontram-se maiores)

-- 1.) Opções --> Mapas
drawWorld (Opçoes lbotoes _ _ fundo fundoAtivo skinsMapa Mapa1 skinMapaAtivo _ _)  -- mapa1 selecionado       ------------ fundo ------------------------------ retângulo transparente ------------------------------------------------- seta do mapa/botão selecionado ---------------------- seta do mapa/botão ativo ---------------------------------------------- mapa1 --------------------------------------------------------------- mapa2 -------------------------------------------------------------- mapa3 -------------------------------------------------------------- mapa4 ------------------------------------------------------------ botão fundo light ------------------------------------------------------ botao fundo dark ------------------------------------------------ botão dificuldade fácil ------------------------------------------ botão dificuldade média --------------------------------------- botão dificuldade difícil --------------------------------- botao voltar -------------
                                                                        | skinMapaAtivo == (!!) skinsMapa 0 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (-wW*0.375) (wH*0.18) ((!!) lbotoes 13)  : Translate (-wW*0.375) (wH*0.18) ((!!) lbotoes 14)  : Translate (-wW*0.375) (wH*0.3) (scale (0.18) (0.18) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                    : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | skinMapaAtivo == (!!) skinsMapa 1 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (-wW*0.375) (wH*0.18) ((!!) lbotoes 13)  : Translate (-wW*0.125) (wH*0.18) ((!!) lbotoes 14)  : Translate (-wW*0.375) (wH*0.3) (scale (0.18) (0.18) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                    : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | skinMapaAtivo == (!!) skinsMapa 2 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (-wW*0.375) (wH*0.18) ((!!) lbotoes 13)  : Translate (wW*0.125) (wH*0.18) ((!!) lbotoes 14)   : Translate (-wW*0.375) (wH*0.3) (scale (0.18) (0.18) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                    : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | skinMapaAtivo == (!!) skinsMapa 3 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (-wW*0.375) (wH*0.18) ((!!) lbotoes 13)  : Translate (wW*0.375) (wH*0.18) ((!!) lbotoes 14)   : Translate (-wW*0.375) (wH*0.3) (scale (0.18) (0.18) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                    : [Translate 0 (-390) ((!!) lbotoes 12)])
drawWorld (Opçoes lbotoes _ _ fundo fundoAtivo skinsMapa Mapa2 skinMapaAtivo _ _)  -- mapa2 selecionado   
                                                                        | skinMapaAtivo == (!!) skinsMapa 0 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (-wW*0.125) (wH*0.18) ((!!) lbotoes 13)  : Translate (-wW*0.375) (wH*0.18) ((!!) lbotoes 14)  : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.18) (0.18) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                    : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | skinMapaAtivo == (!!) skinsMapa 1 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (-wW*0.125) (wH*0.18) ((!!) lbotoes 13)  : Translate (-wW*0.125) (wH*0.18) ((!!) lbotoes 14)  : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.18) (0.18) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                    : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | skinMapaAtivo == (!!) skinsMapa 2 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (-wW*0.125) (wH*0.18) ((!!) lbotoes 13)  : Translate (wW*0.125) (wH*0.18) ((!!) lbotoes 14)   : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.18) (0.18) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                    : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | skinMapaAtivo == (!!) skinsMapa 3 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (-wW*0.125) (wH*0.18) ((!!) lbotoes 13)  : Translate (wW*0.375) (wH*0.18) ((!!) lbotoes 14)   : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.18) (0.18) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                    : [Translate 0 (-390) ((!!) lbotoes 12)])
drawWorld (Opçoes lbotoes _ _ fundo fundoAtivo skinsMapa Mapa3 skinMapaAtivo _ _)  -- mapa3 selecionado    
                                                                        | skinMapaAtivo == (!!) skinsMapa 0 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (wW*0.125) (wH*0.18) ((!!) lbotoes 13)   : Translate (-wW*0.375) (wH*0.18) ((!!) lbotoes 14)  : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.18) (0.18) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                    : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | skinMapaAtivo == (!!) skinsMapa 1 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (wW*0.125) (wH*0.18) ((!!) lbotoes 13)   : Translate (-wW*0.125) (wH*0.18) ((!!) lbotoes 14)  : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.18) (0.18) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                    : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | skinMapaAtivo == (!!) skinsMapa 2 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (wW*0.125) (wH*0.18) ((!!) lbotoes 13)   : Translate (wW*0.125) (wH*0.18) ((!!) lbotoes 14)   : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.18) (0.18) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                    : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | skinMapaAtivo == (!!) skinsMapa 3 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (wW*0.125) (wH*0.18) ((!!) lbotoes 13)   : Translate (wW*0.375) (wH*0.18) ((!!) lbotoes 14)   : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.18) (0.18) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                    : [Translate 0 (-390) ((!!) lbotoes 12)])
drawWorld (Opçoes lbotoes _ _ fundo fundoAtivo skinsMapa Mapa4 skinMapaAtivo _ _)  -- mapa4 selecionado   
                                                                        | skinMapaAtivo == (!!) skinsMapa 0 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (wW*0.375) (wW*0.18) ((!!) lbotoes 13)   : Translate (-wW*0.375) (wH*0.18) ((!!) lbotoes 14)  : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.18) (0.18) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                    : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | skinMapaAtivo == (!!) skinsMapa 1 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (wW*0.375) (wW*0.18) ((!!) lbotoes 13)   : Translate (-wW*0.125) (wH*0.18) ((!!) lbotoes 14)  : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.18) (0.18) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                    : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | skinMapaAtivo == (!!) skinsMapa 2 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (wW*0.375) (wW*0.18) ((!!) lbotoes 13)   : Translate (wW*0.125) (wH*0.18) ((!!) lbotoes 14)   : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.18) (0.18) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                    : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | skinMapaAtivo == (!!) skinsMapa 3 = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (wW*0.375) (wW*0.18) ((!!) lbotoes 13)   : Translate (wW*0.375) (wH*0.18) ((!!) lbotoes 14)   : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.18) (0.18) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                    : [Translate 0 (-390) ((!!) lbotoes 12)])

-- 2.) Opções --> Fundos
drawWorld (Opçoes lbotoes _ _ fundo fundoAtivo skinsMapa LightMode skinMapaAtivo _ _)  -- botão fundo light selecionado
                                                                        | fundoAtivo == (!!) fundo 0        = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (-wW*0.125) (-wH*0.01) ((!!) lbotoes 13) : Translate (-wW*0.125) (-wH*0.01) ((!!) lbotoes 14) : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) (scale (1.2) (1.2) ((!!) lbotoes 18)) : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                     : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | fundoAtivo == (!!) fundo 1        = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (-wW*0.125) (-wH*0.01) ((!!) lbotoes 13) : Translate (wW*0.125) (-wH*0.01) ((!!) lbotoes 14)  : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) (scale (1.2) (1.2) ((!!) lbotoes 18)) : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                     : [Translate 0 (-390) ((!!) lbotoes 12)])
drawWorld (Opçoes lbotoes _ _ fundo fundoAtivo skinsMapa DarkMode skinMapaAtivo _ _)  -- botão fundo dark selecionado
                                                                        | fundoAtivo == (!!) fundo 0        = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (wW*0.125) (-wH*0.01) ((!!) lbotoes 13)  : Translate (-wW*0.125) (-wH*0.01) ((!!) lbotoes 14) : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) (scale (1.2) (1.2) ((!!) lbotoes 19)) : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                     : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | fundoAtivo == (!!) fundo 1        = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (wW*0.125) (-wH*0.01) ((!!) lbotoes 13)  : Translate (wW*0.125) (-wH*0.01) ((!!) lbotoes 14)  : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) (scale (1.2) (1.2) ((!!) lbotoes 19)) : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                     : [Translate 0 (-390) ((!!) lbotoes 12)])

-- 3.) Opções --> Dificuldade
drawWorld (Opçoes lbotoes _ (Player _ _ _ _ _ dificuldade _) fundo fundoAtivo skinsMapa Facil skinMapaAtivo _ _)  -- botão de dificuldade fácil selecionado
                                                                        | dificuldade == 0                  = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (-wW*0.3) (-wH*0.16) ((!!) lbotoes 13)   : Translate (-wW*0.3) (-wH*0.16) ((!!) lbotoes 14)   : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) (scale (1.2) (1.2) ((!!) lbotoes 15)) : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                     : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | dificuldade == 0.25               = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (-wW*0.3) (-wH*0.16) ((!!) lbotoes 13)   : Translate 0 (-wH*0.16) ((!!) lbotoes 14)           : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) (scale (1.2) (1.2) ((!!) lbotoes 15)) : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                     : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | dificuldade == 0.50               = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (-wW*0.3) (-wH*0.16) ((!!) lbotoes 13)   : Translate (wW*0.3) (-wH*0.16) ((!!) lbotoes 14)    : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) (scale (1.2) (1.2) ((!!) lbotoes 15)) : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                     : [Translate 0 (-390) ((!!) lbotoes 12)])
drawWorld (Opçoes lbotoes _ (Player _ _ _ _ _ dificuldade _) fundo fundoAtivo skinsMapa Medio skinMapaAtivo _ _)  -- botão de dificuldade fácil selecionado
                                                                        | dificuldade == 0                  = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate 0 (-wH*0.16) ((!!) lbotoes 13)           : Translate (-wW*0.3) (-wH*0.16) ((!!) lbotoes 14)   : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) (scale (1.2) (1.2) ((!!) lbotoes 16)) : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                     : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | dificuldade == 0.25               = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate 0 (-wH*0.16) ((!!) lbotoes 13)           : Translate 0 (-wH*0.16) ((!!) lbotoes 14)           : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) (scale (1.2) (1.2) ((!!) lbotoes 16)) : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                     : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | dificuldade == 0.50               = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate 0 (-wH*0.16) ((!!) lbotoes 13)           : Translate (wW*0.3) (-wH*0.16) ((!!) lbotoes 14)    : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) (scale (1.2) (1.2) ((!!) lbotoes 16)) : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17)                     : [Translate 0 (-390) ((!!) lbotoes 12)])
drawWorld (Opçoes lbotoes _ (Player _ _ _ _ _ dificuldade _) fundo fundoAtivo skinsMapa Dificil skinMapaAtivo _ _)  -- botão de dificuldade fácil selecionado
                                                                        | dificuldade == 0                  = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (wW*0.3) (-wH*0.16) ((!!) lbotoes 13)    : Translate (-wW*0.3) (-wH*0.16) ((!!) lbotoes 14)   : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) (scale (1.2) (1.2) ((!!) lbotoes 17)) : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | dificuldade == 0.25               = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (wW*0.3) (-wH*0.16) ((!!) lbotoes 13)    : Translate 0 (-wH*0.16) ((!!) lbotoes 14)           : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) (scale (1.2) (1.2) ((!!) lbotoes 17)) : [Translate 0 (-390) ((!!) lbotoes 12)])
                                                                        | dificuldade == 0.50               = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (wW*0.3) (-wH*0.16) ((!!) lbotoes 13)    : Translate (wW*0.3) (-wH*0.16) ((!!) lbotoes 14)    : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18)                     : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19)                     : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15)                     : Translate 0 (-wH*0.1) ((!!) lbotoes 16)                     : Translate (wW*0.3) (-wH*0.1) (scale (1.2) (1.2) ((!!) lbotoes 17)) : [Translate 0 (-390) ((!!) lbotoes 12)])

-- 4.) Opções --> Voltar                                                                -------------- fundo ------------------------------ retângulo transparente ---------------------------------------------------------------------------- mapa1 -------------------------------------------------------- mapa2 -------------------------------------------------------------- mapa3 -------------------------------------------------------------- mapa4 ------------------------------------------------- botão fundo light --------------------------------- botao fundo dark ------------------------------- botão dificuldade fácil --------------------- botão dificuldade média --------------------- botão dificuldade difícil ---------------------- botao voltar -------------
drawWorld (Opçoes lbotoes _ _ fundo fundoAtivo skinsMapa VoltarOpcao skinMapaAtivo _ _) = Pictures (fundoAtivo : Translate 0 ((wH*0.5)/5) (Color (makeColorI 0 0 0 100) (rectangleSolid (wW) (wH*0.7))) : Translate (-wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 3)) : Translate (-wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 4)) : Translate (wW*0.125) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 5)) : Translate (wW*0.375) (wH*0.3) (scale (0.15) (0.15) ((!!) fundo 6)) : Translate (-wW*0.125) (wH*0.065) ((!!) lbotoes 18) : Translate (wW*0.125) (wH*0.065) ((!!) lbotoes 19) : Translate (-wW*0.3) (-wH*0.1) ((!!) lbotoes 15) : Translate 0 (-wH*0.1) ((!!) lbotoes 16) : Translate (wW*0.3) (-wH*0.1) ((!!) lbotoes 17) : [Translate 0 (-390) ((!!) lbotoes 11)])







{- | Esta função recebe um evento e um estado de jogo e, dependendo do tecla pressionada e do estado 
de jogo em que sem encontra, devolve esse mesmo atualizado ou um outro -}
handleEvent :: Event -> EstadoJogo -> EstadoJogo



-- Menu

handleEvent (EventKey (SpecialKey KeyDown) Down _ _) (Menu ej1@(Jogar {}) ej2@(Skins {}) ej3@(Opçoes {})) = Menu ej2 ej1 ej3  -- BotaoJogar --> BotaoSkins

handleEvent (EventKey (SpecialKey KeyUp) Down _ _)   (Menu ej2@(Skins {}) ej1@(Jogar {}) ej3@(Opçoes {})) = Menu ej1 ej2 ej3  -- BotaoSkins --> BotaoJogar
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) (Menu ej2@(Skins {}) ej1@(Jogar {}) ej3@(Opçoes {})) = Menu ej3 ej1 ej2  -- BotaoSkins --> BotaoOpçoes

handleEvent (EventKey (SpecialKey KeyUp) Down _ _)   (Menu ej3@(Opçoes {}) ej1@(Jogar {}) ej2@(Skins {})) = Menu ej2 ej1 ej3  -- BotaoOpçoes --> BotaoSkins



-- Jogar

handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Menu (Jogar lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo) _ _)  = Jogar lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo  -- clicando no botão jogar do menu, o jogo é iniciado com as variáveis que o estado de jogo possui 

handleEvent (EventKey (SpecialKey KeyUp) Down _ _)   (Jogar lbotoes lnumeros (Player j@(Jogo (Jogador (x,y)) mapa) _ n random score dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo)  -- jogador vira-se para cima
    | barreiraChecker mapa (Jogador (x,y)) (Move Cima) = Jogar lbotoes lnumeros (Player j 0 n random score dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo  -- caso a barreiraChecker detete que há uma barreia, o jogador não se move, atualiando apenas a sua direção
    | otherwise = Jogar lbotoes lnumeros (Player (Jogo (Jogador (x,y-1)) mapa) 0 n random score dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo  -- caso contrário, o jogador move-se por um tile 

handleEvent (EventKey (SpecialKey KeyDown) Down _ _) (Jogar lbotoes lnumeros (Player j@(Jogo (Jogador (x,y)) mapa) _ n random score dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo)  -- jogador vira-se para baixo
    | barreiraChecker mapa (Jogador (x,y)) (Move Baixo) = Jogar lbotoes lnumeros (Player j 180 n random score dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo
    | otherwise = Jogar lbotoes lnumeros (Player (Jogo (Jogador (x,y+1)) mapa) 180 n random score dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (Jogar lbotoes lnumeros (Player j@(Jogo (Jogador (x,y)) mapa) _ n random score dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo)  -- jogador vira-se para a direita
    | barreiraChecker mapa (Jogador (x,y)) (Move Direita) = Jogar lbotoes lnumeros (Player j 90 n random score dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo
    | otherwise = Jogar lbotoes lnumeros (Player (Jogo (Jogador (x+1,y)) mapa) 90 n random score dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _)  (Jogar lbotoes lnumeros (Player j@(Jogo (Jogador (x,y)) mapa) _ n random score dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo)  -- jogador vira-se para a esquerda
    | barreiraChecker mapa (Jogador (x,y)) (Move Esquerda) = Jogar lbotoes lnumeros (Player j (-90) n random score dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo
    | otherwise = Jogar lbotoes lnumeros (Player (Jogo (Jogador (x-1,y)) mapa) (-90) n random score dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo

handleEvent (EventKey (SpecialKey KeySpace) Down _ _) (Jogar lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo) = Pausa lbotoes ReiniciarPausa lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo  -- clicando no espaço, o estado de jogo muda de Jogar para Pausa



-- Pausa
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (Pausa lbotoes ReiniciarPausa lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo) = Pausa lbotoes MenuPausa lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo  -- BotaoReiniciar --> BotaoMenu
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa lbotoes ReiniciarPausa lnumeros (Player _ _ _ (h:t) _ dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo) = Jogar lbotoes lnumeros (Player (Jogo (Jogador (5,7)) (estendeMapaRecursiva mapa t 6)) 0 0 t 0 dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo  -- Reinicia todas as variáveis com a exceção das skins, do mapa, do fundo, da dificuldade e do highscore
handleEvent (EventKey (SpecialKey KeyUp) Down _ _)    (Pausa lbotoes MenuPausa lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo) = Pausa lbotoes ReiniciarPausa lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo  -- BotaoMenu --> BotaoReiniciar
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa lbotoes MenuPausa lnumeros (Player _ _ _ (h:t) _ dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo) = Menu (Jogar lbotoes lnumeros (Player (Jogo (Jogador (5,7)) (estendeMapaRecursiva mapa t 6)) 0 0 t 0 dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo) (Skins lbotoes lnumeros (Player (Jogo (Jogador (5,7)) (estendeMapaRecursiva mapa t 6)) 0 0 t 0 dificuldade highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin1 skinJogadorAtivo) (Opçoes lbotoes lnumeros (Player (Jogo (Jogador (5,7)) (estendeMapaRecursiva mapa t 6)) 0 0 t 0 dificuldade highScore) fundo fundoAtivo skinMapa Mapa1 skinMapaAtivo skinJogador skinJogadorAtivo)  -- clicando no botão enter, altera-se o estado de jogo entrando no menu, seguindo a mesma lógica do botão reiniciar
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) (Pausa lbotoes _ lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo) = Jogar lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo  -- clicando no espaço, o estado de jogo muda de Pausa para Jogar






-- Skins
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Menu (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador botaoSkinsSelecionado skinJogadorAtivo) _ _)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador botaoSkinsSelecionado skinJogadorAtivo  -- clicando no botão Skins do menu, entra-se no menu das skins com todas as variáveis que o estado de jogo possui

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin1 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin2 skinJogadorAtivo  -- Skin1 --> Skin2
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin1 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin4 skinJogadorAtivo  -- Skin1 --> Skin4
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin1 _)                 = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin1 ((!!) skinJogador 0)  -- Clicando no enter, a Skin1 passa a ser a skin ativa

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _)  (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin2 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin1 skinJogadorAtivo  -- Skin2 --> Skin1
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin2 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin5 skinJogadorAtivo  -- Skin2 --> Skin5
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin2 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin3 skinJogadorAtivo  -- Skin2 --> Skin3
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin2 _)                 = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin2 ((!!) skinJogador 1)  -- Clicando no enter, a Skin2 passa a ser a skin ativa

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _)  (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin3 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin2 skinJogadorAtivo  -- Skin3 --> Skin2
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin3 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin6 skinJogadorAtivo  -- Skin3 --> Skin6
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin3 _)                 = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin3 ((!!) skinJogador 2)  -- Clicando no enter, a Skin3 passa a ser a skin ativa

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin4 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin5 skinJogadorAtivo  -- Skin4 --> Skin5
handleEvent (EventKey (SpecialKey KeyUp) Down _ _)    (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin4 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin1 skinJogadorAtivo  -- Skin4 --> Skin1
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin4 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador VoltarSkin skinJogadorAtivo  -- Skin4 --> BotaoVoltar
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin4 _)                 = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin4 ((!!) skinJogador 3)  -- Clicando no enter, a Skin4 passa a ser a skin ativa

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _)  (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin5 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin4 skinJogadorAtivo  -- Skin5 --> Skin4
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin5 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin6 skinJogadorAtivo  -- Skin5 --> Skin6
handleEvent (EventKey (SpecialKey KeyUp) Down _ _)    (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin5 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin2 skinJogadorAtivo  -- Skin5 --> Skin2
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin5 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador VoltarSkin skinJogadorAtivo  -- Skin5 --> BotaoVoltar
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin5 _)                 = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin5 ((!!) skinJogador 4)  -- Clicando no enter, a Skin5 passa a ser a skin ativa

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _)  (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin6 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin5 skinJogadorAtivo  -- Skin6 --> Skin5
handleEvent (EventKey (SpecialKey KeyUp) Down _ _)    (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin6 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin3 skinJogadorAtivo  -- Skin6 --> Skin3
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin6 skinJogadorAtivo)  = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador VoltarSkin skinJogadorAtivo  -- Skin6 --> BotaoVoltar
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin6 _)                 = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin6 ((!!) skinJogador 5)  -- Clicando no enter, a Skin6 passa a ser a skin ativa


handleEvent (EventKey (SpecialKey KeyUp) Down _ _)    (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador VoltarSkin skinJogadorAtivo) = Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin5 skinJogadorAtivo  -- BotaoVoltar --> Skin5
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador VoltarSkin skinJogadorAtivo) = Menu (Skins lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador Skin1 skinJogadorAtivo) (Jogar lbotoes lnumeros player fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo) (Opçoes lbotoes lnumeros player fundo fundoAtivo skinMapa Mapa1 skinMapaAtivo skinJogador skinJogadorAtivo)  -- clicando no botão Voltar das skins, entra-se no Menu com todas as variáveis que o estado de jogo possui





-- Opções

-- Opções/Mapas
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Menu (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa botaoOpçoesSelecionado skinMapaAtivo skinJogador skinJogadorAtivo) _ _) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa botaoOpçoesSelecionado skinMapaAtivo skinJogador skinJogadorAtivo  -- clicando no botão Opçoes do menu, entra-se no menu das Opçoes com todas as variáveis que o estado de jogo possui

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa1 skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa2 skinMapaAtivo skinJogador skinJogadorAtivo  -- Mapa1 --> Mapa2
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa1 skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa LightMode skinMapaAtivo skinJogador skinJogadorAtivo  -- Mapa1 --> LightMode
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa1 skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa1 ((!!) skinsMapa 0) skinJogador skinJogadorAtivo  -- Clicando no enter, o Mapa1 passa a ser o mapa ativo

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _)  (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa2 skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa1 skinMapaAtivo skinJogador skinJogadorAtivo  -- Mapa2 --> Mapa1
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa2 skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa3 skinMapaAtivo skinJogador skinJogadorAtivo  -- Mapa2 --> Mapa3
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa2 skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa LightMode skinMapaAtivo skinJogador skinJogadorAtivo  -- Mapa2 --> LightMode
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa2 skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa2 ((!!) skinsMapa 1) skinJogador skinJogadorAtivo  -- Clicando no enter, o Mapa2 passa a ser o mapa ativo


handleEvent (EventKey (SpecialKey KeyLeft) Down _ _)  (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa3 skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa2 skinMapaAtivo skinJogador skinJogadorAtivo  -- Mapa3 --> Mapa2
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa3 skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa4 skinMapaAtivo skinJogador skinJogadorAtivo  -- Mapa3 --> Mapa4
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa3 skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa DarkMode skinMapaAtivo skinJogador skinJogadorAtivo  -- Mapa3 --> DarkMode
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa3 skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa3 ((!!) skinsMapa 2) skinJogador skinJogadorAtivo  -- Clicando no enter, o Mapa3 passa a ser o mapa ativo


handleEvent (EventKey (SpecialKey KeyLeft) Down _ _)  (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa4 skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa3 skinMapaAtivo skinJogador skinJogadorAtivo  -- Mapa4 --> Mapa3
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa4 skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa DarkMode skinMapaAtivo skinJogador skinJogadorAtivo  -- Mapa4 --> DarkMode
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa4 skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa4 ((!!) skinsMapa 3) skinJogador skinJogadorAtivo  -- Clicando no enter, o Mapa4 passa a ser o mapa ativo


-- Opções/Fundos
handleEvent (EventKey (SpecialKey KeyUp) Down _ _)    (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa LightMode skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa2 skinMapaAtivo skinJogador skinJogadorAtivo  -- LightMode --> Mapa2
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa LightMode skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa DarkMode skinMapaAtivo skinJogador skinJogadorAtivo  -- LightMode --> DarkMode
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa LightMode skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Medio skinMapaAtivo skinJogador skinJogadorAtivo  -- LightMode --> DificuldadeMedia
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa LightMode skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo ((!!) fundo 0) skinsMapa LightMode skinMapaAtivo skinJogador skinJogadorAtivo  -- Clicando no enter, o FundoBright passa a ser o fundo ativo


handleEvent (EventKey (SpecialKey KeyUp) Down _ _)    (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa DarkMode skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa3 skinMapaAtivo skinJogador skinJogadorAtivo  -- DarkMode --> Mapa3
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _)  (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa DarkMode skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa LightMode skinMapaAtivo skinJogador skinJogadorAtivo  -- DarkMode --> LightMode
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa DarkMode skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Medio skinMapaAtivo skinJogador skinJogadorAtivo  -- DarkMode --> DificuldadeMedia
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa DarkMode skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo ((!!) fundo 1) skinsMapa DarkMode skinMapaAtivo skinJogador skinJogadorAtivo  -- Clicando no enter, o FundoDark passa a ser o fundo ativo


-- Opções/Dificuldades
handleEvent (EventKey (SpecialKey KeyUp) Down _ _)    (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Facil skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa LightMode skinMapaAtivo skinJogador skinJogadorAtivo  -- DificuladeFacil --> LightMode
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Facil skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Medio skinMapaAtivo skinJogador skinJogadorAtivo  -- DificuladeFacil --> DificuladeMedia
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Facil skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa VoltarOpcao skinMapaAtivo skinJogador skinJogadorAtivo  -- DificuladeFacil --> BotaoVoltar
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Opçoes lbotoes lnumeros (Player jogo angulo acc random score _ highScore) fundo fundoAtivo skinsMapa Facil skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros (Player jogo angulo acc random score 0 highScore) fundo fundoAtivo skinsMapa Facil skinMapaAtivo skinJogador skinJogadorAtivo  -- Clicando no enter, a dificuldade fácil passa a ser a dificuldade ativa


handleEvent (EventKey (SpecialKey KeyUp) Down _ _)    (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Medio skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa LightMode skinMapaAtivo skinJogador skinJogadorAtivo  -- DificuladeMedia --> LightMode
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _)  (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Medio skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Facil skinMapaAtivo skinJogador skinJogadorAtivo  -- DificuladeMedia --> DificuladeFacil
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Medio skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Dificil skinMapaAtivo skinJogador skinJogadorAtivo  -- DificuladeMedia --> DificuladeDificil
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Medio skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa VoltarOpcao skinMapaAtivo skinJogador skinJogadorAtivo  -- DificuladeMedia --> BotaoVoltar
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Opçoes lbotoes lnumeros (Player jogo angulo acc random score _ highScore) fundo fundoAtivo skinsMapa Medio skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros (Player jogo angulo acc random score 0.25 highScore) fundo fundoAtivo skinsMapa Medio skinMapaAtivo skinJogador skinJogadorAtivo  -- Clicando no enter, a dificuldade média passa a ser a dificuldade ativa


handleEvent (EventKey (SpecialKey KeyUp) Down _ _)    (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Dificil skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa DarkMode skinMapaAtivo skinJogador skinJogadorAtivo  -- DificuladeDificil --> DarkMode
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _)  (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Dificil skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Medio skinMapaAtivo skinJogador skinJogadorAtivo  -- DificuladeDificil --> DificuladeMedia
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Dificil skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa VoltarOpcao skinMapaAtivo skinJogador skinJogadorAtivo  -- DificuladeDificil --> BotaoVoltar
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Opçoes lbotoes lnumeros (Player jogo angulo acc random score _ highScore) fundo fundoAtivo skinsMapa Dificil skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros (Player jogo angulo acc random score 0.50 highScore) fundo fundoAtivo skinsMapa Dificil skinMapaAtivo skinJogador skinJogadorAtivo  -- Clicando no enter, a dificuldade difícil passa a ser a dificuldade ativa


-- Opções/Voltar
handleEvent (EventKey (SpecialKey KeyUp) Down _ _)    (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa VoltarOpcao skinMapaAtivo skinJogador skinJogadorAtivo) = Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Medio skinMapaAtivo skinJogador skinJogadorAtivo  -- BotaoVoltar --> DificuladeMedia
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa VoltarOpcao skinMapaAtivo skinJogador skinJogadorAtivo) = Menu (Opçoes lbotoes lnumeros player fundo fundoAtivo skinsMapa Mapa3 skinMapaAtivo skinJogador skinJogadorAtivo) (Jogar lbotoes lnumeros player fundo fundoAtivo skinsMapa skinMapaAtivo skinJogador skinJogadorAtivo) (Skins lbotoes lnumeros player fundo fundoAtivo skinsMapa skinMapaAtivo skinJogador Skin1 skinJogadorAtivo)  -- clicando no botão Voltar das Opçoes, entra-se no Menu com todas as variáveis que o estado de jogo possui







-- PERDEU
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  (PerdeuJogo lbotoes ReiniciarPerdeu lnumeros player fundo fundoAtivo skinsMapa skinMapaAtivo skinJogador skinJogadorAtivo) = PerdeuJogo lbotoes MenuPerdeu lnumeros player fundo fundoAtivo skinsMapa skinMapaAtivo skinJogador skinJogadorAtivo  -- BotaoReiniciar --> BotaoMenu
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo lbotoes ReiniciarPerdeu lnumeros (Player _ _ _ (h:t) score dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinJogador skinJogadorAtivo)  -- Reinicia todas as variáveis com a exceção das skins, do mapa, do fundo, da dificuldade e do highscore 
                                                                                                                                                                                                                    | score >= highScore = Jogar lbotoes lnumeros (Player (Jogo (Jogador (5,7)) (estendeMapaRecursiva mapa t 6)) 0 0 t 0 dificuldade score) fundo fundoAtivo skinsMapa skinMapaAtivo skinJogador skinJogadorAtivo  -- caso o highscore seja inferior ao score conseguido, o highscore é alterado para o valor do score 
                                                                                                                                                                                                                    | otherwise = Jogar lbotoes lnumeros (Player (Jogo (Jogador (5,7)) (estendeMapaRecursiva mapa t 6)) 0 0 t 0 dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinJogador skinJogadorAtivo  -- caso contrário, o highscore mantém-se

handleEvent (EventKey (SpecialKey KeyUp) Down _ _)    (PerdeuJogo lbotoes MenuPerdeu lnumeros player fundo fundoAtivo skinsMapa skinMapaAtivo skinJogador skinJogadorAtivo) = PerdeuJogo lbotoes ReiniciarPerdeu lnumeros player fundo fundoAtivo skinsMapa skinMapaAtivo skinJogador skinJogadorAtivo  -- BotaoMenu --> BotaoReiniciar
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo lbotoes MenuPerdeu lnumeros (Player _ _ _ (h:t) score dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinJogador skinJogadorAtivo)  -- clicando no botão enter, altera-se o estado de jogo entrando no menu, seguindo a mesma lógica do botão reiniciar
                                                                                                                                                                                                                    | score >= highScore = Menu (Jogar lbotoes lnumeros (Player (Jogo (Jogador (5,7)) (estendeMapaRecursiva mapa t 6)) 0 0 t 0 dificuldade score) fundo fundoAtivo skinsMapa skinMapaAtivo skinJogador skinJogadorAtivo) (Skins lbotoes lnumeros (Player (Jogo (Jogador (5,7)) (estendeMapaRecursiva mapa t 6)) 0 0 t 0 dificuldade score) fundo fundoAtivo skinsMapa skinMapaAtivo skinJogador Skin1 skinJogadorAtivo) (Opçoes lbotoes lnumeros (Player (Jogo (Jogador (5,7)) (estendeMapaRecursiva mapa t 6)) 0 0 t 0 dificuldade score) fundo fundoAtivo skinsMapa Mapa1 skinMapaAtivo skinJogador skinJogadorAtivo)
                                                                                                                                                                                                                    | otherwise = Menu (Jogar lbotoes lnumeros (Player (Jogo (Jogador (5,7)) (estendeMapaRecursiva mapa t 6)) 0 0 t 0 dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinJogador skinJogadorAtivo) (Skins lbotoes lnumeros (Player (Jogo (Jogador (5,7)) (estendeMapaRecursiva mapa t 6)) 0 0 t 0 dificuldade highScore) fundo fundoAtivo skinsMapa skinMapaAtivo skinJogador Skin1 skinJogadorAtivo) (Opçoes lbotoes lnumeros (Player (Jogo (Jogador (5,7)) (estendeMapaRecursiva mapa t 6)) 0 0 t 0 dificuldade highScore) fundo fundoAtivo skinsMapa Mapa1 skinMapaAtivo skinJogador skinJogadorAtivo)





-- Outros eventos
handleEvent _ j = j -- nada acontece quando outra tecla (não definida) é utilizada






------------------------------------------------------------------------------------------------------------------

{- | Esta função recebe um float e um estado de jogo e, tendo por base as funções deslizeMapa, 
movimentoEnquantoParado, movimentoObstaculos e jogoTerminou, atualiza o estado de jogo 120 vezes,
uma vez que a frame rate escolhida para o jogo é 120 -}
step :: Float -> EstadoJogo -> EstadoJogo



step _ (Jogar lbotoes lnumeros p@(Player j@(Jogo (Jogador (x,y)) (Mapa largura to)) a n randoms score dif highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo)
    | jogoTerminou j = PerdeuJogo lbotoes ReiniciarPerdeu lnumeros p fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo  -- a função jogoTerminou, verifica, constantemente, se o jogador perdeu, para poder devolver o estado de jogo PerdeuJogo quando necessário
    | n < -l = Jogar lbotoes lnumeros (Player (deslizeMapa randoms (Jogo (movimentoEnquantoParado (Jogador (x,y)) to) (Mapa largura (movimentoObstaculos (Jogador (x,y)) to 0 0)))) a 0 (drop numeroTiles randoms) (score+1) dif highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo  -- quando o contador n atinge (negativamente) o valor de um tile, este volta a ser 0 e todo o mapa atualiza, i.é, todos os obstáculos movem-se, cria-se uma nova linha no topo do mapa e retira-se a linha de baixo e é provocado um movimento do jogador sempre que este se encontra em cima de um tronco
    | otherwise = Jogar lbotoes lnumeros (Player j a (n - 1.00001^score + 0.5 - dif) randoms (score+1) dif highScore) fundo fundoAtivo skinMapa skinMapaAtivo skinJogador skinJogadorAtivo  -- enquanto o contador n nao atinge o valor -l, este vai diminuindo, segundo uma função exponencial, sendo o expoente dado pelo contador score (que aumenta infinitamente, 120 unidades por segundo) e pelo o valor inicial dado pela adição da constante 0.5-dif à função exponencial -1.00001^score, sendo dif um valor que varia com a dificuldade do jogo

step _ ej = ej  -- quando o estado de jogo não é Jogar, a função step devolve o estado de jogo sem qualquer alteração







{- | Tipo de display -}
win :: Display
win = FullScreen

{- | Cor do background -}
bkg :: Color
bkg = white


{- | A função que roda o jogo -}
main :: IO()
main = do
    botaoP <- loadBMP "src/Botoes/Pausa.bmp" 
    botaoJA <- loadBMP "src/Botoes/BotaoJogar_Ativo.bmp" 
    botaoJ <- loadBMP "src/Botoes/BotaoJogar.bmp" 
    botaoMA <- loadBMP "src/Botoes/BotaoMenu_Ativo.bmp" 
    botaoM <- loadBMP "src/Botoes/BotaoMenu.bmp" 
    botaoOA <- loadBMP "src/Botoes/BotaoOpcoes_Ativo.bmp" 
    botaoO <- loadBMP "src/Botoes/BotaoOpcoes.bmp" 
    botaoRA <- loadBMP "src/Botoes/BotaoReiniciar_Ativo.bmp" 
    botaoR <- loadBMP "src/Botoes/BotaoReiniciar.bmp" 
    botaoSA <- loadBMP "src/Botoes/BotaoSkins_Ativo.bmp" 
    botaoS <- loadBMP "src/Botoes/BotaoSkins.bmp" 
    botaoVA <- loadBMP "src/Botoes/BotaoVoltar_Ativo.bmp" 
    botaoV <- loadBMP "src/Botoes/BotaoVoltar.bmp"
    setaDour <- loadBMP "src/Botoes/SetaDourada.bmp"
    setaVerm <- loadBMP "src/Botoes/SetaVermelha.bmp"
    botaoEasy <- loadBMP "src/Botoes/BotaoFacil.bmp"
    botaoMedium <- loadBMP "src/Botoes/BotaoMedio.bmp"
    botaoHard <- loadBMP "src/Botoes/BotaoDificil.bmp"
    botaoSun <- loadBMP "src/Botoes/Sol.bmp"
    botaoMoon <- loadBMP "src/Botoes/Lua.bmp"

    let botaoPausa = scale (0.5) (0.5) botaoP
    let botaoJogarAtivo = scale (0.5) (0.5) botaoJA
    let botaoJogar = scale (0.5) (0.5) botaoJ
    let botaoMenuAtivo = scale (0.5) (0.5) botaoMA
    let botaoMenu = scale (0.5) (0.5) botaoM
    let botaoOpcoesAtivo = scale (0.5) (0.5) botaoOA
    let botaoOpcoes = scale (0.5) (0.5) botaoO
    let botaoReiniciarAtivo = scale (0.5) (0.5) botaoRA
    let botaoReiniciar = scale (0.5) (0.5) botaoR
    let botaoSkinsAtivo = scale (0.5) (0.5) botaoSA
    let botaoSkins = scale (0.5) (0.5) botaoS
    let botaoVoltarAtivo = scale (0.5) (0.5) botaoVA
    let botaoVoltar = scale (0.5) (0.5) botaoV
    let setaDourada = scale (0.30) (0.30) setaDour
    let setaVermelha = scale (0.25) (0.25) setaVerm
    let botaoFacil = scale (0.3) (0.3) botaoEasy
    let botaoMedio = scale (0.3) (0.3) botaoMedium
    let botaoDificil = scale (0.3) (0.3) botaoHard
    let botaoSol = scale (0.3) (0.3) botaoSun
    let botaoLua = scale (0.3) (0.3) botaoMoon


    fundoL <- loadBMP "src/Fundos/Fundo_LightMode.bmp" 
    fundoD <- loadBMP "src/Fundos/Fundo_DarkMode.bmp" 
    logotipo <- loadBMP "src/Fundos/Logo_CrossyRoad.bmp" 
    gameOv <- loadBMP "src/Fundos/GameOver.bmp"
    highSc <- loadBMP "src/Fundos/HighScore.bmp"
    sc <- loadBMP "src/Fundos/Score.bmp"


    let fundoLight = scale (0.5) (0.5) fundoL
    let fundoDark = scale (0.5) (0.5) fundoD
    let logo = scale (0.8) (0.8) logotipo
    let gameOver = scale (0.9) (0.9) gameOv
    let highScore = scale (0.3) (0.3) highSc
    let score = scale (0.3) (0.3) sc

    fotoMapaClassico <- loadBMP "src/FotosMapas/FotoMapaClassico.bmp" 
    fotoMapaNatal <- loadBMP "src/FotosMapas/FotoMapaNatal.bmp" 
    fotoMapaDetalhado <- loadBMP "src/FotosMapas/FotoMapaDetalhado.bmp" 
    fotoMapaMinecraft <- loadBMP "src/FotosMapas/FotoMapaMinecraft.bmp" 

    rioClassico <- loadBMP "src/Mapas/MapaClassico/MapaClassico_Rio.bmp"
    estradaClassico <- loadBMP "src/Mapas/MapaClassico/MapaClassico_Estrada.bmp"
    relvaClassico <- loadBMP "src/Mapas/MapaClassico/MapaClassico_Relva.bmp"
    troncoClassico <- loadBMP "src/Mapas/MapaClassico/MapaClassico_Tronco.bmp"
    carroDireitaClassico <- loadBMP "src/Mapas/MapaClassico/MapaClassico_Carro_Direita.bmp"
    carroEsquerdaClassico <- loadBMP "src/Mapas/MapaClassico/MapaClassico_Carro_Esquerda.bmp"
    arvoreClas <- loadBMP "src/Mapas/MapaClassico/MapaClassico_Arvore.bmp"

    let arvoreClassico = Scale (1.2) (1.1) arvoreClas
    
    rioDetalhado <- loadBMP "src/Mapas/MapaDetalhado/MapaDetalhado_Rio.bmp"
    estradaDetalhado <- loadBMP "src/Mapas/MapaDetalhado/MapaDetalhado_Estrada.bmp"
    relvaDetalhado <- loadBMP "src/Mapas/MapaDetalhado/MapaDetalhado_Relva.bmp"
    troncoDetalhado <- loadBMP "src/Mapas/MapaDetalhado/MapaDetalhado_Tronco.bmp"
    carroDireitaDetalhado <- loadBMP "src/Mapas/MapaDetalhado/MapaDetalhado_Carro_Direita.bmp"
    carroEsquerdaDetalhado <- loadBMP "src/Mapas/MapaDetalhado/MapaDetalhado_Carro_Esquerda.bmp"
    arvoreDetalhado <- loadBMP "src/Mapas/MapaDetalhado/MapaDetalhado_Arvore.bmp"    

    rioMine <- loadBMP "src/Mapas/MapaMinecraft/MapaMinecraft_Rio.bmp"
    estradaMine <- loadBMP "src/Mapas/MapaMinecraft/MapaMinecraft_Estrada.bmp"
    relvaMine <- loadBMP "src/Mapas/MapaMinecraft/MapaMinecraft_Relva.bmp"
    troncoMine <- loadBMP "src/Mapas/MapaMinecraft/MapaMinecraft_Tronco.bmp"
    carroDireitaMinecraft <- loadBMP "src/Mapas/MapaMinecraft/MapaMinecraft_Carro_Direita.bmp"
    carroEsquerdaMinecraft <- loadBMP "src/Mapas/MapaMinecraft/MapaMinecraft_Carro_Esquerda.bmp"
    arvoreMine <- loadBMP "src/Mapas/MapaMinecraft/MapaMinecraft_Arvore.bmp"

    let rioMinecraft = scale (0.9375) (0.9375) rioMine
    let estradaMinecraft = scale (0.9375) (0.9375) estradaMine
    let relvaMinecraft = scale (0.9375) (0.9375) relvaMine
    let troncoMinecraft = Scale (0.9375) (0.8) troncoMine
    let arvoreMinecraft = Translate (0) (0.5*l) (Scale (1.1) (1.1) arvoreMine)

    rioNatal <- loadBMP "src/Mapas/MapaNatal/MapaNatal_Rio.bmp"
    estradaNatal <- loadBMP "src/Mapas/MapaNatal/MapaNatal_Estrada.bmp"
    relvaNatal <- loadBMP "src/Mapas/MapaNatal/MapaNatal_Relva.bmp"
    troncoNatal <- loadBMP "src/Mapas/MapaNatal/MapaNatal_Tronco.bmp"
    carroDireitaNatal <- loadBMP "src/Mapas/MapaNatal/MapaNatal_Carro_Direita.bmp"
    carroEsquerdaNatal <- loadBMP "src/Mapas/MapaNatal/MapaNatal_Carro_Esquerda.bmp"
    arvoreNatal <- loadBMP "src/Mapas/MapaNatal/MapaNatal_Arvore.bmp"

    numeroZ <- loadBMP "src/Numeros/Zero.bmp"
    numeroU <- loadBMP "src/Numeros/Um.bmp"
    numeroD <- loadBMP "src/Numeros/Dois.bmp"
    numeroT <- loadBMP "src/Numeros/Tres.bmp"
    numeroQ <- loadBMP "src/Numeros/Quatro.bmp"
    numeroC <- loadBMP "src/Numeros/Cinco.bmp"
    numeroSei <- loadBMP "src/Numeros/Seis.bmp"
    numeroSet <- loadBMP "src/Numeros/Sete.bmp"
    numeroO <- loadBMP "src/Numeros/Oito.bmp"
    numeroN <- loadBMP "src/Numeros/Nove.bmp"

    let numeroZero = scale (0.035) (0.035) numeroZ
    let numeroUm = scale (0.035) (0.035) numeroU
    let numeroDois = scale (0.035) (0.035) numeroD
    let numeroTres = scale (0.035) (0.035) numeroT
    let numeroQuatro = scale (0.035) (0.035) numeroQ
    let numeroCinco = scale (0.035) (0.035) numeroC
    let numeroSeis = scale (0.035) (0.035) numeroSei
    let numeroSete = scale (0.035) (0.035) numeroSet
    let numeroOito = scale (0.035) (0.035) numeroO
    let numeroNove = scale (0.035) (0.035) numeroN


    skinAmongUsDi <- loadBMP "src/Skins/SkinAmongUs/AmongUsDireita.bmp"
    skinAmongUsEs <- loadBMP "src/Skins/SkinAmongUs/AmongUsEsquerda.bmp"

    let skinAmongUsDireita = Translate 0 (0.3*l) (Scale (1.3) (1.3) skinAmongUsDi)
    let skinAmongUsEsquerda = Translate 0 (0.3*l) (Scale (1.3) (1.3) skinAmongUsEs)

    skinGalinhaCima <- loadBMP "src/Skins/SkinGalinha/GalinhaCima.bmp"
    skinGalinhaBaixo <- loadBMP "src/Skins/SkinGalinha/GalinhaBaixo.bmp"
    skinGalinhaEsquerda <- loadBMP "src/Skins/SkinGalinha/GalinhaEsquerda.bmp"
    skinGalinhaDireita <- loadBMP "src/Skins/SkinGalinha/GalinhaDireita.bmp"
    
    skinGalinhaNatalCima <- loadBMP "src/Skins/SkinGalinhaNatal/GalinhaCimaNatal.bmp"
    skinGalinhaNatalBaixo <- loadBMP "src/Skins/SkinGalinhaNatal/GalinhaBaixoNatal.bmp"
    skinGalinhaNatalEsquerda <- loadBMP "src/Skins/SkinGalinhaNatal/GalinhaEsquerdaNatal.bmp"
    skinGalinhaNatalDireita <- loadBMP "src/Skins/SkinGalinhaNatal/GalinhaDireitaNatal.bmp"
    
    skinMarioDi <- loadBMP "src/Skins/SkinMario/MarioDireita.bmp"
    skinMarioEs <- loadBMP "src/Skins/SkinMario/MarioEsquerda.bmp"

    let skinMarioDireita = Translate 0 (0.2*l) (Scale (1.5) (1.5) skinMarioDi)
    let skinMarioEsquerda = Translate 0 (0.2*l) (Scale (1.5) (1.5) skinMarioEs)
    
    skinPacManCima <- loadBMP "src/Skins/SkinPacMan/PacManCima.bmp"
    skinPacManBaixo <- loadBMP "src/Skins/SkinPacMan/PacManBaixo.bmp"
    skinPacManEsquerda <- loadBMP "src/Skins/SkinPacMan/PacManEsquerda.bmp"
    skinPacManDireita <- loadBMP "src/Skins/SkinPacMan/PacManDireita.bmp"
    
    skinSteveCi <- loadBMP "src/Skins/SkinSteve/SteveCima.bmp"
    skinSteveBa <- loadBMP "src/Skins/SkinSteve/SteveBaixo.bmp"
    skinSteveEs <- loadBMP "src/Skins/SkinSteve/SteveEsquerda.bmp"
    skinSteveDi <- loadBMP "src/Skins/SkinSteve/SteveDireita.bmp"

    let skinSteveCima = Translate 0 (0.4*l) (Scale (1.4) (1.4) skinSteveCi)
    let skinSteveBaixo = Translate 0 (0.4*l) (Scale (1.4) (1.4) skinSteveBa)
    let skinSteveEsquerda = Translate 0 (0.4*l) (Scale (1.4) (1.4) skinSteveEs)
    let skinSteveDireita = Translate 0 (0.4*l) (Scale (1.4) (1.4) skinSteveDi)

    



    g <- newStdGen

    let random = randoms g :: [Int]
    let botoes = [botaoPausa,botaoJogarAtivo,botaoJogar,botaoSkinsAtivo,botaoSkins,botaoOpcoesAtivo,botaoOpcoes,botaoMenuAtivo,botaoMenu,botaoReiniciarAtivo,botaoReiniciar,botaoVoltarAtivo,botaoVoltar,setaVermelha,setaDourada,botaoFacil,botaoMedio,botaoDificil,botaoSol,botaoLua] :: Botoes
    let fundos = [fundoLight,fundoDark,logo,fotoMapaClassico,fotoMapaNatal,fotoMapaDetalhado,fotoMapaMinecraft,gameOver,highScore,score] :: Fundo

    let mapaClassico = [rioClassico,estradaClassico,relvaClassico,troncoClassico,carroDireitaClassico,carroEsquerdaClassico,arvoreClassico] :: SkinMapa
    let mapaDetalhado = [rioDetalhado,estradaDetalhado,relvaDetalhado,troncoDetalhado,carroDireitaDetalhado,carroEsquerdaDetalhado,arvoreDetalhado] :: SkinMapa
    let mapaMinecraft = [rioMinecraft,estradaMinecraft,relvaMinecraft,troncoMinecraft,carroDireitaMinecraft,carroEsquerdaMinecraft,arvoreMinecraft] :: SkinMapa
    let mapaNatal = [rioNatal,estradaNatal,relvaNatal,troncoNatal,carroDireitaNatal,carroEsquerdaNatal,arvoreNatal] :: SkinMapa
    let skinsMapa = [mapaClassico,mapaNatal,mapaDetalhado,mapaMinecraft] :: SkinsMapa

    let numeros = [numeroZero,numeroUm,numeroDois,numeroTres,numeroQuatro,numeroCinco,numeroSeis,numeroSete,numeroOito,numeroNove] :: Numeros

    let skinGalinha = [skinGalinhaCima,skinGalinhaBaixo,skinGalinhaEsquerda,skinGalinhaDireita] :: SkinJogador
    let skinGalinhaNatal = [skinGalinhaNatalCima,skinGalinhaNatalBaixo,skinGalinhaNatalEsquerda,skinGalinhaNatalDireita] :: SkinJogador
    let skinSteve = [skinSteveCima,skinSteveBaixo,skinSteveEsquerda,skinSteveDireita] :: SkinJogador
    let skinPacMan = [skinPacManCima,skinPacManBaixo,skinPacManEsquerda,skinPacManDireita] :: SkinJogador
    let skinAmongUs = [skinAmongUsDireita,skinAmongUsEsquerda] :: SkinJogador
    let skinMario = [skinMarioDireita,skinMarioEsquerda] :: SkinJogador
    let skinsJogador = [skinGalinha,skinGalinhaNatal,skinSteve,skinPacMan,skinAmongUs,skinMario] :: SkinsJogador


    let p = Player (Jogo (Jogador (5,7)) (estendeMapaRecursiva mapa random 6)) 0 0 random 0 0.25 0 :: Player  -- Estado inical do jogo
    
    play
            win
            bkg
            120
            (Menu (Jogar botoes numeros p fundos fundoLight skinsMapa mapaClassico skinsJogador skinGalinha) (Skins botoes numeros p fundos fundoLight skinsMapa mapaClassico skinsJogador Skin1 skinGalinha) (Opçoes botoes numeros p fundos fundoLight skinsMapa Mapa1 mapaClassico skinsJogador skinGalinha))
            drawWorld
            handleEvent
            step