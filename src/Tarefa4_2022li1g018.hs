{- |
Module      : Tarefa4_2022li1g018
Description : Determinar se o jogo terminou
Copyright   : Eduardo de Oliveira Sousa Faria <a104353@alunos.uminho.pt>
              Nuno Miguel Ribeiro da Silva <a104089@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g018 where

import LI12223
import Tarefa1_2022li1g018
import Tarefa3_2022li1g018


{- | Esta função recebe um Jogo, isto é, a posição de um jogador e um mapa válido e, dependendo
da posição onde se encontra assim como o tipo de terreno, determina se perdeu o jogo-}
jogoTerminou :: Jogo -> Bool

jogoTerminou (Jogo (Jogador (x,y)) (Mapa l to))
    | x < 0 || x > l-1 || y > length to - 1 = True  -- este caso verifica se um jogador perdeu o jogo saindo do mapa, tanto pela direita ou esquerda, como por baixo
    | (!!) ((!!) (groupObstaculos to) y) x == Nenhum && (!!) (firsts to) y == "Rio" = True  -- este caso verifica se um jogador está num Rio mas não num tronco, i.é., caiu ao Rio 
    | (!!) ((!!) (groupObstaculos to) y) x == Carro && (!!) (firsts to) y == "Estrada" = True  -- este caso verifica se um jogador está numa Estrada e na mesma posição que um carro, i.é., foi atropelado
    | otherwise = False  -- caso nenhuma das condições anteriores se verifique, o jogo não terminou, devolvendo False
