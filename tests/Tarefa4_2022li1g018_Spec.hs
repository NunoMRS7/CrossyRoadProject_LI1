module Tarefa4_2022li1g018_Spec where

import LI12223
import Tarefa4_2022li1g018
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test [testejogoTerminou1,
                                              testejogoTerminou2,
                                              testejogoTerminou3,
                                              testejogoTerminou4,
                                              testejogoTerminou5,
                                              testejogoTerminou6]

testejogoTerminou1 = "Jogador encontra-se numa Estrada" ~: False ~=? jogoTerminou (Jogo (Jogador (1,1)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                                ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                                ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                                ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum,Tronco,Nenhum])
                                                                                                                ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])]))  

testejogoTerminou2 = "Jogador encontra-se num Tronco" ~: False ~=? jogoTerminou (Jogo (Jogador (1,3)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                              ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                              ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                              ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum,Tronco,Nenhum])
                                                                                                              ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])]))  

testejogoTerminou3 = "Jogador encontra-se debaixo de um Carro" ~: True ~=? jogoTerminou (Jogo (Jogador (0,1)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                                      ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                                      ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                                      ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum,Tronco,Nenhum])
                                                                                                                      ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])]))  

testejogoTerminou4 = "Jogador encontra-se na água" ~: True ~=? jogoTerminou (Jogo (Jogador (3,3)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                          ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                          ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                          ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum,Tronco,Nenhum])
                                                                                                          ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])]))  

testejogoTerminou5 = "Jogador encontra-se fora do mapa pela esquerda (Levado por um tronco)" ~: True ~=? jogoTerminou (Jogo (Jogador (-1,3)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                                                                     ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                                                                     ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                                                                     ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum,Tronco,Nenhum])
                                                                                                                                                     ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])]))  

testejogoTerminou6 = "Jogador encontra-se fora do mapa por baixo" ~: True ~=? jogoTerminou (Jogo (Jogador (2,5)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                                         ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                                         ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                                         ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum,Tronco,Nenhum])
                                                                                                                         ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])]))  