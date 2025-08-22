{- |
Module      : Tarefa3_2022li1g018
Description : Movimentação do personagem e obstáculos
Copyright   : Eduardo de Oliveira Sousa Faria <a104353@alunos.uminho.pt>
              Nuno Miguel Ribeiro da Silva <a104089@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g018 where

import LI12223


{- | Esta função recebe um Jogo, isto é, a posição de um jogador e um mapa válido, e uma jogada.
De acordo com a jogada, irá ocorrer uma atualização do mapa bem como da posição do jogador, isto
inclui o movimento de obstaculos e do jogador de acordo com o seu ambiente -}
animaJogo :: Jogo -> Jogada -> Jogo

animaJogo (Jogo j (Mapa l to)) Parado = Jogo (movimentoEnquantoParado j to) (Mapa l (movimentoObstaculos j to 0 0))

animaJogo (Jogo j (Mapa l to)) d
    | barreiraChecker (Mapa l to) j d = Jogo j (Mapa l (movimentoObstaculos j to 0 0))
    | otherwise = Jogo (moveJogadorTronco j d to) (Mapa l (movimentoObstaculos (moveJogador j d) to 0 0))










-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{- | Esta função, dada uma lista de Obstáculos e um número inteiro n (associado à velocidade),
devolve a lista deslocada para a esquerda n vezes -}
rotateL :: [Obstaculo] -> Int -> [Obstaculo]

rotateL t 0 = t  
rotateL (h:t) n = rotateL (t ++ [h]) (abs n-1) 



{- | Esta função, dada uma lista de Obstáculos e um número inteiro n (associado à velocidade),
devolve a lista deslocada para a direita n vezes -}
rotateR :: [Obstaculo] -> Int -> [Obstaculo]

rotateR t 0 = t
rotateR (h:t) n = rotateR (last t:h:init t) (abs n-1)




{- | Esta função, dada uma lista de Obstáculos, devolve a lista deslocada para a esquerda uma vez -}
rotateLestrada :: [Obstaculo] -> [Obstaculo]
rotateLestrada (h:t) = t ++ [h] 



{- | Esta função, dada uma lista de Obstáculos, devolve a lista deslocada para a direita uma vez -}
rotateRestrada :: [Obstaculo] -> [Obstaculo]
rotateRestrada (h:t) = last t:h:init t





{- | Esta função recebe uma lista de terrenos e os seus obstáculos associados e devolve uma lista
com todas as listas de obstáculos 
    ex.: groupObstaculos [(Rio 2,[Nenhum,Tronco]),(Relva,[Nenhum,Nenhum])] -> [[Nenhum,Tronco],[Nenhum,Nenhum]] -}
groupObstaculos :: [(Terreno,[Obstaculo])] -> [[Obstaculo]]

groupObstaculos [] = []
groupObstaculos ((t,o):xs) = o : groupObstaculos xs 



{- | Esta função recebe uma lista de terrenos e os seus obstáculos associados e devolve uma lista
com todas as velocidades de cada terreno 
    ex.: groupVelocidades [(Rio 2,[Nenhum,Tronco]),(Relva,[Nenhum,Nenhum])] -> [2,0] -}
groupVelocidades :: [(Terreno,[Obstaculo])] -> [Int]

groupVelocidades [] = []
groupVelocidades ((Estrada v,o):xs) = v : groupVelocidades xs
groupVelocidades ((Rio v,o):xs) = v : groupVelocidades xs
groupVelocidades ((Relva,o):xs) = 0 : groupVelocidades xs 
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------







{- | Esta função, dado um jogador, uma lista de terrenos e os seus obstáculos associados, e dois acumuladores inteiros,
move os obstáculos para a esquerda ou para a direita, dependendo da sua velocidade, servindo os acumuladores para determinar
a posição do jogador (n para a abcissa e k para a ordenada) -}
movimentoObstaculos :: Jogador -> [(Terreno, [Obstaculo])] -> Int -> Int -> [(Terreno, [Obstaculo])]

movimentoObstaculos _ [] _ _ = []
movimentoObstaculos j ((Relva,o):t) _ k = (Relva,o):movimentoObstaculos j t 0 (k+1) -- como a relva não tem obstáculos que se movem, não é necessário aplicar as função na mesma. O n permanece 0 uma vez que o terreno não é estrada, sendo que este serviria para verificar, instante a instante, se o jogador foi atropelado ou não. Soma-se ainda uma unidade ao k uma vez que se muda para a próxima linha

movimentoObstaculos j ((Rio v,o):t) _ k
    | v < 0 = (Rio v,rotateL o v):movimentoObstaculos j t 0 (k+1)  -- caso a velocidade do rio seja negativa, então os troncos vão "rodar" para a esquerda de acordo com a velocidade
    | v > 0 = (Rio v,rotateR o v):movimentoObstaculos j t 0 (k+1) -- caso a velocidade do rio seja positiva, então os troncos vão "rodar" para a direita de acordo com a velocidade

movimentoObstaculos j@(Jogador (x,y)) ((Estrada v,o):t) n k
    | (y == k && (!!) o x == Carro) || n == abs v = (Estrada v,o):movimentoObstaculos j t 0 (k+1)  -- neste caso, quando a ordenada do jogador corresponde ao valor k (o jogador encontra-se na linha que está a ser alterada pela função) e a sua abcissa coincide com a posição de um carro ou o acumulador n atinge o valor da velocidade (a função rotateLestrada ou a função rotateRestrada realizou todas as rotações) a linha para de ser rodada e segue-se a rotação das restantes
    | v < 0 = movimentoObstaculos j ((Estrada v,rotateLestrada o):t) (n+1) k
    | v > 0 = movimentoObstaculos j ((Estrada v,rotateRestrada o):t) (n+1) k
    

{- | Esta função recebe a posição de um jogador e uma jogada, e, dependendo da jogada, move o 
jogador para uma nova posição -}
moveJogador :: Jogador -> Jogada -> Jogador

moveJogador (Jogador (x,y)) (Move d)  
    | d == Cima = Jogador (x,y-1)    
    | d == Baixo = Jogador (x,y+1)
    | d == Esquerda = Jogador (x-1,y)
    | d == Direita = Jogador (x+1,y)





{- | Esta função recebe a posição de um jogador, uma jogada e uma lista de terrenos e os seus 
obstáculos associados e, caso o jogador se encontre em cima de um tronco e faça uma jogada, 
verifica se a jogada ou é para a esquerda ou para a direita, e move o jogador tendo em conta
a velocidade com que o tronco se move 
    ex.: (!!) ((!!) (groupObstaculos [(Rio 2, [Nenhum,Tronco]),(Relva, [Nenhum,Nenhum])]) 1) 1 ->
    -> (!!) ((!!) [[Nenhum,Tronco],[Nenhum,Nenhum]] 1) 1 -> (!!) [Nenhum,Nenhum] 1 -> Nenhum -}
moveJogadorTronco :: Jogador -> Jogada -> [(Terreno,[Obstaculo])] -> Jogador

moveJogadorTronco (Jogador (x,y)) (Move d) to
    | (!!) ((!!) (groupObstaculos to) y) x == Tronco && d == Esquerda = Jogador (x + (!!) (groupVelocidades to) y - 1,y)  -- ((!!) ((!!) (groupObstaculos to) y) x) devolve o obstáculo correspondente à posição x do terreno que está na pocição y. Para a nova posição do jogador, é necessário adicionar a velocidade do Tronco em que se encontra para além do valor (-1) (sendo que este se move para a Esquerda)
    | (!!) ((!!) (groupObstaculos to) y) x == Tronco && d == Direita = Jogador (x + (!!) (groupVelocidades to) y + 1,y)  -- ((!!) ((!!) (groupObstaculos to) y) x) devolve o obstáculo correspondente à posição x do terreno que está na pocição y. Para a nova posição do jogador, é necessário adicionar a velocidade do Tronco em que se encontra para além do valor 1 (sendo que este se move para a Direita)
    | otherwise = moveJogador (Jogador (x,y)) (Move d)





{- | Esta função recebe uma posição de um jogador e uma lista de terrenos e os seus obstáculos
associados e, caso o jogador se encontre na mesma posição que um tronco, este acompanha o movimento
do mesmo -}
movimentoEnquantoParado :: Jogador -> [(Terreno,[Obstaculo])] -> Jogador

movimentoEnquantoParado (Jogador (x,y)) to
    | (!!) ((!!) (groupObstaculos to) y) x == Tronco = Jogador (x + (!!) (groupVelocidades to) y,y)  -- apenas quando o jogador se encontre parado em cima de um Tronco é necessário movê-lo de acordo com a velocidade do mesmo
    | otherwise = Jogador (x,y)  -- caso contrário, o jogador permanece no mesmo lugar





{- | Esta função, dado um estado de um mapa, a posição de um jogador, e uma jogada, determina se,
para aquele estado do mapa e para a nova posição desejada pelo jogador, se trata de uma barreira,
i.é., um local inacessível. São consideradas barreiras o topo do mapa, os lados esquerdo e direito
do mapa e um árvore -}
barreiraChecker :: Mapa -> Jogador -> Jogada -> Bool

barreiraChecker (Mapa l to) (Jogador (x,y)) (Move d)
    | (!!) ((!!) (groupObstaculos to) y) x == Tronco && (d == Esquerda || d == Direita) = False  -- neste caso, quando o jogador se encontra num tronco e move-se para a esquerda ou para a direita, nunca é impedido pelas barreiras laterais 
    | x1 == (-1) || x1 == l = True  -- barreiras laterais
    | y1 == 1 = True  -- barreira superior
    | (!!) ((!!) (groupObstaculos to) y1) x1 == Arvore = True  -- uma Árvore é considerada uma barreira quando a nova posição de um jogador seria a mesma posição que essa Árvore 
    | otherwise = False
    where Jogador (x1,y1) = moveJogador (Jogador (x,y)) (Move d)  -- x1 e y1 correspondem às cordenadas da nova posição do jogador de acordo com a jogada feita