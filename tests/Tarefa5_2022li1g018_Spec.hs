module Tarefa5_2022li1g018_Spec where

import LI12223
import Tarefa5_2022li1g018
import Test.HUnit

testsT5 :: Test
testsT5 = TestLabel "Testes Tarefa 5" $ test [testedeslizaMapa1,
                                              testedeslizaMapa2,
                                              testedeslizaMapa3]

testedeslizaMapa1 = "Exemplo de um deslize do mapa" ~: Jogo (Jogador (1,3)) (Mapa 7 [(Relva,        [Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Arvore])
                                                                                    ,(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                    ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                    ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                    ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum])])            ~=? 
                                                
                                    deslizeMapa [1892389,284924,-1893829,12849248,2839283,-239010239,198392183,23828] (Jogo (Jogador (1,2)) (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                                                                    ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                                                                    ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                                                                    ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum])
                                                                                                                                                    ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])]))


testedeslizaMapa2 = "Exemplo de um deslize do mapa" ~: Jogo (Jogador (6,3)) (Mapa 7 [(Rio (-2),     [Tronco, Tronco, Tronco, Tronco,Nenhum,Nenhum, Tronco])
                                                                                    ,(Rio 1,        [Tronco, Tronco, Tronco,Nenhum, Tronco,Nenhum,Nenhum])
                                                                                    ,(Estrada (-1), [Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Carro])
                                                                                    ,(Relva,        [Arvore,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum])
                                                                                    ,(Estrada (-2), [Carro,Carro,Carro,Nenhum,Nenhum,Carro,Nenhum])])           ~=? 
                                                
                                    deslizeMapa [-2389223,24928492,90149012,-8694863,138492894,938198391,-18394128,-12941294] (Jogo (Jogador (6,2)) (Mapa 7 [(Rio 1,        [Tronco, Tronco, Tronco, Nenhum, Tronco, Nenhum, Nenhum])
                                                                                                                                                            ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                                                                            ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                                                                            ,(Estrada (-2), [Carro, Carro, Carro, Nenhum, Nenhum, Carro, Nenhum])
                                                                                                                                                            ,(Estrada 1,    [Nenhum, Carro, Carro, Carro, Nenhum, Carro, Carro])]))

testedeslizaMapa3 = "Exemplo de um deslize do mapa" ~: Jogo (Jogador (1,2)) (Mapa 7 [(Estrada (-1), [Nenhum, Carro, Nenhum, Nenhum, Nenhum, Nenhum, Carro])
                                                                                    ,(Estrada 1,    [Carro, Carro, Carro, Nenhum, Carro, Nenhum, Nenhum])
                                                                                    ,(Estrada 2,    [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                    ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                    ,(Relva,        [Arvore, Arvore, Arvore, Nenhum, Nenhum, Arvore, Nenhum])])          ~=? 
                                                
                                    deslizeMapa [217482842,2564928492,325205,-86923234863,-1285492894,-198391,424394128,-56941294] (Jogo (Jogador (1,1)) (Mapa 7 [(Estrada 1, [Carro, Carro, Carro, Nenhum, Carro, Nenhum, Nenhum])
                                                                                                                                                            ,(Estrada 2, [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                                                                            ,(Relva,     [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                                                                            ,(Relva,     [Arvore, Arvore, Arvore, Nenhum, Nenhum, Arvore, Nenhum])
                                                                                                                                                            ,(Estrada 1, [Nenhum, Carro, Carro, Carro, Nenhum, Carro, Carro])]))
