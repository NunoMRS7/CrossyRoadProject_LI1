module Tarefa1_2022li1g018_Spec where

import LI12223
import Tarefa1_2022li1g018
import Test.HUnit

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test [testeMapaValido1,
                                              testeMapaValido2,
                                              testeMapaValido3,
                                              testeMapaValido4,
                                              testeMapaValido5,
                                              testeMapaValido6,
                                              testeMapaValido7,
                                              testeMapaValido8,
                                              testeMapaValido9,
                                              testeMapaValido10,
                                              testeMapaValido11,
                                              testeMapaValido12,
                                              testeMapaValido13,
                                              testeMapaValido14,
                                              testeMapaValido15,
                                              testeMapaValido16]

        


testeMapaValido1 = "Largura inferior ou igual 0" ~: False ~=? mapaValido (Mapa (-5) [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore])
                                                                                    ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro])
                                                                                    ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore])]) 
                                                                          
testeMapaValido2 = "Obstáculos pertecem ao tipo de terreno" ~: False ~=? mapaValido (Mapa 5 [(Relva,        [Arvore, Nenhum, Arvore, Tronco, Arvore])
                                                                                            ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro])
                                                                                            ,(Relva,        [Carro, Nenhum, Nenhum, Arvore, Arvore])]) 
                                                                          
testeMapaValido3 = "Rios seguidos têm direções opostas" ~: False ~=? mapaValido (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore])
                                                                                        ,(Rio 3, [Nenhum, Nenhum, Nenhum, Tronco, Tronco])
                                                                                        ,(Rio 4, [Tronco, Nenhum, Tronco, Tronco, Nenhum])]) 
                                                                          
testeMapaValido4 = "Troncos têm no máximo 5 unidades (Meio da lista)" ~: False ~=? mapaValido (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Arvore, Nenhum])
                                                                                                      ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro, Carro, Nenhum])
                                                                                                      ,(Rio 3,        [Nenhum, Tronco, Tronco, Tronco, Tronco, Tronco, Tronco])]) 
                                                                          
testeMapaValido5 = "Troncos têm no máximo 5 unidades (Extremidades da lista)" ~: False ~=? mapaValido (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Arvore, Nenhum])
                                                                                                              ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro, Carro, Nenhum])
                                                                                                              ,(Rio 3,        [Tronco, Nenhum, Tronco, Tronco, Tronco, Tronco, Tronco])]) 
                                                                          
testeMapaValido6 = "Existe pelo menos um tronco num Rio" ~: False ~=? mapaValido (Mapa 5 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore])
                                                                                         ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro])
                                                                                         ,(Rio 3,        [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])]) 
                                                                          
testeMapaValido7 = "Carros têm no máximo 3 unidades (Meio da lista)" ~: False ~=? mapaValido (Mapa 5 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore])
                                                                                                     ,(Estrada (-1), [Nenhum, Carro, Carro, Carro, Carro])
                                                                                                     ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore])]) 
                                                                          
testeMapaValido8 = "Carros têm no máximo 3 unidades (Extremidades da lista)" ~: False ~=? mapaValido (Mapa 5 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore])
                                                                                                             ,(Estrada (-1), [Carro, Nenhum, Carro, Carro, Carro])
                                                                                                             ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore])]) 
                                                                          
testeMapaValido9 = "Existe pelo menos um Nenhum em cada terreno" ~: False ~=? mapaValido (Mapa 5 [(Relva,        [Arvore, Arvore, Arvore, Arvore, Arvore])
                                                                                                 ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro])
                                                                                                 ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore])]) 
                                                                          
testeMapaValido10 = "Rios e Estradas possuem velocidades não nulas" ~: False ~=? mapaValido (Mapa 5 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore])
                                                                                                   ,(Estrada 0,     [Nenhum, Nenhum, Nenhum, Carro, Carro])
                                                                                                   ,(Relva,         [Arvore, Nenhum, Nenhum, Arvore, Arvore])]) 
                                                                          
testeMapaValido11 = "Largura do mapa diferente do número de obstáuculos" ~: False ~=? mapaValido (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore])
                                                                                                         ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro])
                                                                                                         ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore])]) 
                                                                          
testeMapaValido12 = "Verificação de terrenos contiguos" ~: False ~=? mapaValido (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore])
                                                                                        ,(Relva, [Nenhum, Nenhum, Nenhum, Arvore, Arvore])
                                                                                        ,(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore])
                                                                                        ,(Relva, [Nenhum, Arvore, Arvore, Nenhum, Nenhum])
                                                                                        ,(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum])
                                                                                        ,(Relva, [Nenhum, Nenhum, Nenhum, Nenhum, Arvore])]) 
                                                                          
testeMapaValido13 = "Verificação de terrenos contiguos" ~: False ~=? mapaValido (Mapa 5 [(Estrada 1,    [Carro, Nenhum, Carro, Nenhum, Carro])
                                                                                        ,(Estrada (-2), [Nenhum, Nenhum, Nenhum, Carro, Carro])
                                                                                        ,(Estrada 2,    [Carro, Nenhum, Nenhum, Nenhum, Carro])
                                                                                        ,(Estrada (-1), [Nenhum, Carro, Carro, Nenhum, Nenhum])
                                                                                        ,(Estrada 4,    [Carro, Nenhum, Nenhum, Nenhum, Nenhum])
                                                                                        ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Nenhum, Carro])]) 
                                                                          
testeMapaValido14 = "Verificação de terrenos contiguos" ~: False ~=? mapaValido (Mapa 5 [(Rio 1,    [Tronco, Nenhum, Tronco, Nenhum, Tronco])
                                                                                        ,(Rio (-2), [Nenhum, Nenhum, Nenhum, Tronco, Tronco])
                                                                                        ,(Rio 2,    [Tronco, Nenhum, Nenhum, Nenhum, Tronco])
                                                                                        ,(Rio (-1), [Nenhum, Tronco, Tronco, Nenhum, Nenhum])
                                                                                        ,(Rio 4,    [Tronco, Nenhum, Nenhum, Nenhum, Nenhum])])

testeMapaValido15 = "Exemplo de um mapa válido" ~: True ~=? mapaValido (Mapa 5 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore])
                                                                               ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro])
                                                                               ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore])]) 
                                                                          
testeMapaValido16 = "Exemplo de um mapa válido" ~: True ~=? mapaValido (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                               ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                               ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                               ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum,Tronco,Nenhum])
                                                                               ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])]) 