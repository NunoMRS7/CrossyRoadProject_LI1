module Tarefa3_2022li1g018_Spec where

import LI12223
import Tarefa3_2022li1g018
import Test.HUnit

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test [testeanimaJogo1,
                                              testeanimaJogo2,
                                              testeanimaJogo3,
                                              testeanimaJogo4,
                                              testeanimaJogo5,
                                              testeanimaJogo6,
                                              testeanimaJogo7,
                                              testeanimaJogo8,
                                              testeanimaJogo9]


testeanimaJogo1 = "Jogador permanece parado e atualiza-se apenas o mapa" ~: Jogo (Jogador (1,0)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                         ,(Estrada (-1), [Nenhum, Nenhum, Carro, Carro, Nenhum, Carro, Carro])
                                                                                                         ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                         ,(Rio (-2),     [Tronco, Nenhum, Nenhum, Tronco, Nenhum, Tronco, Tronco])
                                                                                                         ,(Rio 1,        [Tronco, Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco])]) ~=? 
                                                
                                                                 animaJogo (Jogo (Jogador (1,0)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                         ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                         ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                         ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum])
                                                                                                         ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])])) Parado

testeanimaJogo2 = "Jogador permanece parado em cima de um tronco e atualiza-se o mapa" ~: Jogo (Jogador (3,4)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                                       ,(Estrada (-1), [Nenhum, Nenhum, Carro, Carro, Nenhum, Carro, Carro])
                                                                                                                       ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                                       ,(Rio (-2),     [Tronco, Nenhum, Nenhum, Tronco, Nenhum, Tronco, Tronco])
                                                                                                                       ,(Rio 1,        [Tronco, Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco])]) ~=? 
                                                       
                                                                               animaJogo (Jogo (Jogador (2,4)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                                       ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                                       ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                                       ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum])
                                                                                                                       ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])])) Parado

testeanimaJogo3 = "Jogador faz uma jogada mas é impedido por uma barreira (Limite direito)" ~: Jogo (Jogador (6,0)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                                            ,(Estrada (-1), [Nenhum, Nenhum, Carro, Carro, Nenhum, Carro, Carro])
                                                                                                                            ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                                            ,(Rio (-2),     [Tronco, Nenhum, Nenhum, Tronco, Nenhum, Tronco, Tronco])
                                                                                                                            ,(Rio 1,        [Tronco, Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco])]) ~=? 
                                                         
                                                                                    animaJogo (Jogo (Jogador (6,0)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                                            ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                                            ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                                            ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum])
                                                                                                                            ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])])) (Move Direita)

testeanimaJogo4 = "Jogador faz uma jogada mas é impedido por uma barreira (Árvore)" ~: Jogo (Jogador (6,2)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                                    ,(Estrada (-1), [Nenhum, Nenhum, Carro, Carro, Nenhum, Carro, Carro])
                                                                                                                    ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                                    ,(Rio (-2),     [Tronco, Nenhum, Nenhum, Tronco, Nenhum, Tronco, Tronco])
                                                                                                                    ,(Rio 1,        [Tronco, Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco])]) ~=? 

                                                                            animaJogo (Jogo (Jogador (6,2)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                                    ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                                    ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                                    ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum])
                                                                                                                    ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])])) (Move Esquerda)

testeanimaJogo5 = "Jogador faz uma jogada livre" ~: Jogo (Jogador (1,0)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                 ,(Estrada (-1), [Nenhum, Nenhum, Carro, Carro, Nenhum, Carro, Carro])
                                                                                 ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                 ,(Rio (-2),     [Tronco, Nenhum, Nenhum, Tronco, Nenhum, Tronco, Tronco])
                                                                                 ,(Rio 1,        [Tronco, Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco])]) ~=? 

                                         animaJogo (Jogo (Jogador (1,1)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                 ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                 ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                 ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum])
                                                                                 ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])])) (Move Cima)

testeanimaJogo6 = "Jogador faz uma jogada para a esquerda (Em cima de um tronco)" ~: Jogo (Jogador (4,4)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                                  ,(Estrada (-1), [Nenhum, Nenhum, Carro, Carro, Nenhum, Carro, Carro])
                                                                                                                  ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                                  ,(Rio (-2),     [Tronco, Nenhum, Nenhum, Tronco, Nenhum, Tronco, Tronco])
                                                                                                                  ,(Rio 1,        [Tronco, Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco])]) ~=? 
                                                     
                                                                          animaJogo (Jogo (Jogador (2,4)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                                  ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                                  ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                                  ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum])
                                                                                                                  ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])])) (Move Direita)

testeanimaJogo7 = "Exemplo de uma jogada" ~: Jogo (Jogador (2,3)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                          ,(Estrada (-1), [Nenhum, Nenhum, Carro, Carro, Nenhum, Carro, Carro])
                                                                          ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                          ,(Rio (-2),     [Tronco, Nenhum, Nenhum, Tronco, Nenhum, Tronco, Tronco])
                                                                          ,(Rio 1,        [Tronco, Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco])]) ~=? 
                                                     
                                  animaJogo (Jogo (Jogador (2,4)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                          ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                          ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                          ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum])
                                                                          ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])])) (Move Cima)

testeanimaJogo8 = "Exemplo de uma jogada" ~: Jogo (Jogador (2,3)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                          ,(Estrada (-1), [Nenhum, Nenhum, Carro, Carro, Nenhum, Carro, Carro])
                                                                          ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                          ,(Rio (-2),     [Tronco, Nenhum, Nenhum, Tronco, Nenhum, Tronco, Tronco])
                                                                          ,(Rio 1,        [Tronco, Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco])]) ~=? 
                                                     
                                  animaJogo (Jogo (Jogador (2,4)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                          ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                          ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                          ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum])
                                                                          ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])])) (Move Cima)

testeanimaJogo9 = "Exemplo de uma jogada" ~: Jogo (Jogador (0,3)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                          ,(Estrada (-1), [Nenhum, Nenhum, Carro, Carro, Nenhum, Carro, Carro])
                                                                          ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                          ,(Rio (-2),     [Tronco, Nenhum, Nenhum, Tronco, Nenhum, Tronco, Tronco])
                                                                          ,(Rio 1,        [Tronco, Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco])]) ~=? 
                                                     
                                  animaJogo (Jogo (Jogador (0,3)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                          ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                          ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                          ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum])
                                                                          ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])])) (Move Cima)