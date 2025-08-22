{- |
Module      : Tarefa5_2022li1g018
Description : Movimento do jogo e do jogador
Copyright   : Eduardo de Oliveira Sousa Faria <a104353@alunos.uminho.pt>
              Nuno Miguel Ribeiro da Silva <a104089@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g018 where

import LI12223
import Tarefa2_2022li1g018


{- | Esta função recebe uma lista de inteiros e um mapa e, atendendo à função
estendeMapa, gera e adiciona uma nova linha válida ao topo. Para além disso,
elimina também a última linha e move o jogador uma posição para baixo -}
deslizeMapa :: [Int] -> Jogo -> Jogo
deslizeMapa a (Jogo (Jogador (x,y)) (Mapa l lo)) = Jogo (Jogador (x,y+1)) (estendeMapa (Mapa l (init lo)) a) 