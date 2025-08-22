module Tarefa2_2022li1g018_Spec where

import LI12223
import Tarefa2_2022li1g018
import Test.HUnit

testsT2 :: Test 
testsT2 = TestLabel "Testes Tarefa 2" $ test [testeestendeMapa,
                                             testeproximosTerrenosValidos1,
                                             testeproximosTerrenosValidos2,
                                             testerandomTerreno,
                                             testeproximosObstaculosValidos1,
                                             testeproximosObstaculosValidos2,
                                             testeproximosObstaculosValidos3,
                                             testeproximosObstaculosValidos4,
                                             testeproximosObstaculosValidos5,
                                             testerandomObstaculo1,
                                             testerandomObstaculo2]


testeestendeMapa = "Exemplo de uma linha gerada através de uma lista de números aleatórios" ~: Mapa 7 [(Rio 1,        [Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum])
                                                                                                      ,(Estrada 2,    [Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])
                                                                                                      ,(Relva,        [Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Nenhum])
                                                                                                      ,(Estrada (-1), [Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Carro])
                                                                                                      ,(Relva,        [Arvore,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum])
                                                                                                      ,(Rio (-2),     [Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum])
                                                                                                      ,(Rio 1,        [Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco,Tronco])] ~=? 

                                                                                  estendeMapa (Mapa 7 [(Estrada 2,    [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])
                                                                                                      ,(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                      ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                      ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                      ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum])
                                                                                                      ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])]) [18490,239329,-192399,27837327,183984,829492,18748278,-2482942]


testeproximosTerrenosValidos1 = "Exemplo de uma lista de terrenos válidos" ~: [Rio (-1), Estrada (-2), Relva] ~=? proximosTerrenosValidos (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                                                                                  ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                                                                  ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                                                                                  ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum])
                                                                                                                                                  ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])]) 78
                                            

testeproximosTerrenosValidos2 = "Exemplo de uma lista de Terrenos válidos" ~: [Rio (-1), Relva] ~=? proximosTerrenosValidos (Mapa 7 [(Estrada (-3), [Carro, Nenhum, Carro, Nenhum, Carro, Nenhum, Nenhum])
                                                                                                                                    ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                                                                    ,(Estrada 4,    [Carro, Nenhum, Nenhum, Carro, Carro, Carro, Nenhum])
                                                                                                                                    ,(Estrada (-2), [Carro, Carro, Carro, Nenhum, Nenhum, Carro, Nenhum])
                                                                                                                                    ,(Estrada 1,    [Nenhum, Carro, Carro, Carro, Nenhum, Carro, Carro])]) 58


testerandomTerreno = "Exemplo de um Terreno gerado" ~: Rio 1 ~=? randomTerreno (Mapa 7 [(Relva,        [Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum])
                                                                                       ,(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Carro])
                                                                                       ,(Relva,        [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore, Nenhum])
                                                                                       ,(Rio (-2),     [Tronco, Tronco, Tronco, Nenhum, Nenhum, Tronco, Nenhum])
                                                                                       ,(Rio 1,        [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])]) [18490,239329]                                   


testeproximosObstaculosValidos1 = "Exemplo de uma lista de obstáculos válidos" ~: [Nenhum] ~=? proximosObstaculosValidos 7 (Estrada 2, [Carro,Carro,Carro,Nenhum,Nenhum,Carro])
testeproximosObstaculosValidos2 = "Exemplo de uma lista de obstáculos válidos" ~: [Nenhum] ~=? proximosObstaculosValidos 7 (Estrada 2, [Carro,Carro,Carro])
testeproximosObstaculosValidos3 = "Exemplo de uma lista de obstáculos válidos" ~: [Nenhum] ~=? proximosObstaculosValidos 7 (Rio 2,     [Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco])
testeproximosObstaculosValidos4 = "Exemplo de uma lista de obstáculos válidos" ~: [Nenhum] ~=? proximosObstaculosValidos 7 (Rio 2,     [Tronco,Tronco,Tronco,Tronco,Tronco])
testeproximosObstaculosValidos5 = "Exemplo de uma lista de obstáculos válidos" ~: []       ~=? proximosObstaculosValidos 7 (Relva ,    [Arvore,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum])


testerandomObstaculo1 = "Exemplo de uma lista de obstáculos gerados" ~: [Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum] ~=? randomObstaculo 7 (Rio 2, []) [182948,2409102,-2389238,284822442,-2324294,12909204,284294892]
testerandomObstaculo2 = "Exemplo de uma lista de obstáculos gerados" ~: [Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum] ~=? randomObstaculo 7 (Estrada 2, []) [182948,2409102,-2389238,284822442,-2324294,12909204,123912893]