{- |
Module      : Tarefa2_2022li1g018
Description : Geração contínua de um mapa
Copyright   : Eduardo de Oliveira Sousa Faria <a104353@alunos.uminho.pt>
              Nuno Miguel Ribeiro da Silva <a104089@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}

module Tarefa2_2022li1g018 where

import LI12223 (Mapa(..), Obstaculo(..), Terreno(..))




{- | Esta função recebe um mapa e uma lista de inteiros e, atendendo às funções randomObstaculo
e RandomTerreno, gera e adiciona uma nova linha válida ao topo, ou seja, gera um novo mapa -}
estendeMapa :: Mapa -> [Int] -> Mapa

estendeMapa (Mapa l to) (h:t) = Mapa l ((terreno, randomObstaculo l (terreno,[]) t): to)  -- a função devolve um novo mapa, sendo este a junção do mapa anterior com a nova linha gerada pelas funções auxiliares
    where terreno = randomTerreno (Mapa l to) (h:t)










-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{- | A função seguinte é uma função auxiliar que ajuda na definição de outras funções.
A função isSuffixOf, dada duas listas, determina se a primeira lista é suffixo da segunda
    ex.: isSuffixOf [20,30] [10,20,30] -> True  e  isSuffixOf [10,30] [10,20,30] -> False -}
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf l l'@(_:t) = l == l' || isSuffixOf l t



{- | A função seguinte é uma função auxiliar que ajuda na definição de outras funções.
A função isPrefixOf, dada duas listas, determina se a primeira lista é prefixo da segunda
    ex.: isPrefixOf [10,20] [10,20,30] -> True  e  isPrefixOf [10,30] [10,20,30] -> False -}
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h:t) (h':t') = h == h' && isPrefixOf t t'
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


{- | A função recebe uma lista de terrenos e os seus obstáculos e, atendendo à velocidade do
terreno anterior, devolve uma lista com as velocidades passíveis de serem utilizadas no próximo
terreno (rios contíguos não podem ter a mesma direção) -}
proximasVelociadesValidas :: [(Terreno,[Obstaculo])] -> [Int]

proximasVelociadesValidas ((Rio v,_):t)
    | v < 0 = [1,2]
    | v > 0 = [-1,-2]
proximasVelociadesValidas _ = [1,-1,2,-2]



{- | Esta função recebe um Mapa e um número inteiro (que assume o valor de chance) e, consoante
a disposição dos tipos de terreno do mapa dado, apresenta uma lista de terrenos, acompanhados pela
sua velocidade, que são passíveis de serem procedentes do mapa -}
proximosTerrenosValidos :: Mapa -> Int -> [Terreno]

proximosTerrenosValidos (Mapa _ ((Rio _,_):(Rio _,_):(Rio _,_):(Rio _,_):_)) n  -- neste caso, como o mapa apresenta 4 Ríos contíguos, seguindo as regras da tarefa 1, os únicos terrenos válidos são a Estrada/Relva
    | n >= 0  && n <= 25  = [Estrada 1, Relva]
    | n >= 26 && n <= 50  = [Estrada 2, Relva]  
    | n >= 51 && n <= 75  = [Estrada (-1), Relva]  
    | n >= 76 && n <= 100 = [Estrada (-2), Relva]  
proximosTerrenosValidos (Mapa _ ((Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):_)) n  -- neste caso, como o mapa apresenta 5 Estradas contíguas, seguindo as regras da tarefa 1, os únicos terrenos válidos são o Rio/Relva
    | n >= 0  && n <= 25  = [Rio 1, Relva]
    | n >= 26 && n <= 50  = [Rio 2, Relva]  
    | n >= 51 && n <= 75  = [Rio (-1), Relva]  
    | n >= 76 && n <= 100 = [Rio (-2), Relva]  
proximosTerrenosValidos (Mapa _ ((Relva ,_):(Relva ,_):(Relva ,_):(Relva ,_):(Relva ,_):_)) n  -- neste caso, como o mapa apresenta 5 Estradas contíguas, seguindo as regras da tarefa 1, os únicos terrenos válidos são o Rio/Estrada
    | n >= 0  && n <= 25  = [Estrada 1, Rio 1]
    | n >= 26 && n <= 50  = [Estrada 2, Rio 2]  
    | n >= 51 && n <= 75  = [Estrada (-1), Rio (-1)]  
    | n >= 76 && n <= 100 = [Estrada (-2), Rio (-2)] 
proximosTerrenosValidos (Mapa _ to) n  -- neste caso entra qualquer tipo de mapa, sendo apenas requesitado quando o mapa em questão não entra em nenhum dos casos anteriores, podendo então a nova linha ser formada por qualquer tipo de terreno
    | n >= 0  && n <= 25  = [Rio v1, Estrada 1, Relva]
    | n >= 26 && n <= 50  = [Rio v2, Estrada 2, Relva]
    | n >= 51 && n <= 75  = [Rio v1, Estrada (-1), Relva]
    | n >= 76 && n <= 100 = [Rio v2, Estrada (-2), Relva]
    where v1 = (!!) (proximasVelociadesValidas to) 0  -- a função proximasVelocidadeValidas dispõe as velocidades passíveis de serem utilizadas no próximo terreno
          v2 = (!!) (proximasVelociadesValidas to) 1




{- | Esta função recebe um Mapa e uma lista de números inteiros e, dependendo do valor dos primeiros dois
inteiros da lista e do número de possibilidadesde terrenos válidos, devolve um desses terrenos possíveis -}
randomTerreno :: Mapa -> [Int] -> Terreno

randomTerreno l (x:xs:t) 
    | ran >= 0  && ran <= 40  && length ptv == 2 = (!!) ptv 0  -- sendo ran uma variável de valor compreendido entre 0 e 100, assumindo que a length da lista que passa pela função proxímosTerrenosValidos é 2, i.é., apenas possui dois terrenos válidos, se o número estiver entre 0 e 40 então será escolhido o primeiro elemento, e se estiver entre 41 e 100 será escolhido o segundo elemento
    | ran >= 41 && ran <= 100 && length ptv == 2 = (!!) ptv 1
    where ran = mod x 101  -- a variável ran devolve um número compreendido entre 0 e 100, atendendo ao valor de x
          ptv = proximosTerrenosValidos l (mod xs 101)  -- a variável ptv devolve os próximos terrenos possíveis, usando o valor xs como inteiro aleatório


randomTerreno l@(Mapa _ ((Relva,_):to)) (x:xs:t)
    | ran >= 0  && ran <= 25  && length ptv == 3 = (!!) ptv 0  -- este caso é semelhante ao anterior, onde a única diferença é a length da lista que passa pela função proxímosTerrenosValidos, onde caso seja 3, o valor ran terá de ser dividido em três partes
    | ran >= 26 && ran <= 50  && length ptv == 3 = (!!) ptv 1
    | ran >= 51 && ran <= 100 && length ptv == 3 = (!!) ptv 2
    where ran = mod x 101
          ptv = proximosTerrenosValidos l (mod xs 101)


randomTerreno l@(Mapa _ ((Rio _,_):to)) (x:xs:t)
    | ran >= 0  && ran <= 50  && length ptv == 3 = (!!) ptv 0
    | ran >= 51 && ran <= 75  && length ptv == 3 = (!!) ptv 1
    | ran >= 76 && ran <= 100 && length ptv == 3 = (!!) ptv 2
    where ran = mod x 101
          ptv = proximosTerrenosValidos l (mod xs 101)


randomTerreno l@(Mapa _ ((Estrada _,_):to)) (x:xs:t)
    | ran >= 0  && ran <= 25  && length ptv == 3 = (!!) ptv 0
    | ran >= 26 && ran <= 75  && length ptv == 3 = (!!) ptv 1
    | ran >= 76 && ran <= 100 && length ptv == 3 = (!!) ptv 2
    where ran = mod x 101
          ptv = proximosTerrenosValidos l (mod xs 101)





{- | Esta função recebe uma largura e um terreno e a lista dos seus obstáculos e, atendendo às
condições da tarefa 1, devolve uma lista de obstáculos que são passíveis de serem procedentes à
lista de obstáculos dada -}
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]

proximosObstaculosValidos l (Rio _, []) = [Tronco,Nenhum]  -- a ordem dos obstáculos apresentada é relevante nos três tipos de terrenos para a função randomObstaculo
proximosObstaculosValidos l (Rio v, o)
    | l == length o = []  -- neste caso, como a largura possui o mesmo valor do número de elementos da lista de obstáculos, não são válidos quaisquer obstáculos
    | l - 1 == length o && Nenhum `notElem` o = [Nenhum]  -- neste caso, se apenas houver mais um elemento na lista de obstáculos a ser criada possível de se adicionar, e não houver um Nenhum, então o único obstáculo que é possível é esse mesmo
    | l - 1 == length o && Tronco `notElem` o = [Tronco]  -- a mesma idea neste caso, mas apenas como se trada de um terreno do tipo Rio, é necessário que haja pelo menos um Tronco
    | [Tronco,Tronco,Tronco,Tronco,Tronco] `isSuffixOf` o = [Nenhum]  -- seguindo as regras da tarefa 1, um tronco nunca poderá exceder o tamanho de 5 únidades. Usando a função auxiliar isSuffixOf vemos que, quando esse é o caso, o único obstáculo possível é o Nenhum 
    | [Tronco] `isPrefixOf` o && [Tronco,Tronco,Tronco,Tronco] `isSuffixOf` o && l - 1 == length o = [Nenhum]
    | [Tronco,Tronco] `isPrefixOf` o && [Tronco,Tronco,Tronco] `isSuffixOf` o && l - 1 == length o = [Nenhum]
    | [Tronco,Tronco,Tronco] `isPrefixOf` o && [Tronco,Tronco] `isSuffixOf` o && l - 1 == length o = [Nenhum]
    | [Tronco,Tronco,Tronco,Tronco] `isPrefixOf` o && [Tronco] `isSuffixOf` o && l - 1 == length o = [Nenhum]
    | [Tronco,Tronco,Tronco,Tronco,Tronco] `isPrefixOf` o && l - 1 == length o = [Nenhum]
    | otherwise = [Tronco,Nenhum]  -- quando nenhum dos casos anteriores é verificado, então qualquer obstáculo é válido

proximosObstaculosValidos l (Estrada _, []) = [Nenhum,Carro]
proximosObstaculosValidos l (Estrada v, o)
    | l == length o = []
    | l - 1 == length o && Nenhum `notElem` o = [Nenhum]
    | [Carro,Carro,Carro] `isSuffixOf` o = [Nenhum]  -- atentando às regras da tarefa 1, quando um carro possui 3 unidades de comprimento, então apenas o Nenhum é possível
    | [Carro] `isPrefixOf` o && [Carro,Carro] `isSuffixOf` o && l - 1 == length o = [Nenhum]
    | [Carro,Carro] `isPrefixOf` o && [Carro] `isSuffixOf` o && l - 1 == length o = [Nenhum]
    | [Carro,Carro,Carro] `isPrefixOf` o && l - 1 == length o = [Nenhum]
    | otherwise = [Nenhum,Carro]

proximosObstaculosValidos l (Relva, []) = [Nenhum,Arvore]
proximosObstaculosValidos l (Relva, o)
    | l == length o = []
    | l - 1 == length o && Nenhum `notElem` o = [Nenhum]
    | otherwise = [Nenhum,Arvore]





{- | Esta função recebe uma largura e um terreno e a lista dos seus obstáculos e uma lista
de inteiros e, dependendo do valor resultante dos inteiros e do número de possibilidades
de obstáculos válidos, devolve uma lista de obstáculos possível para um dado terreno -}
randomObstaculo :: Int -> (Terreno,[Obstaculo]) -> [Int] -> [Obstaculo]

randomObstaculo _ _ [] = []
randomObstaculo l (t,[]) (x:xs)
    | ran >= 0 && ran <= 80 && length pov == 2 = (!!) pov 0 : randomObstaculo l (t,[(!!) pov 0]) xs  -- quando a variável ran, compreendida entre 0 e 100, se encontra entre 0 e 80 e a length pov é 2, i.é., são possíveis dois tipos de obstáculo para o terreno e a lista de obstáculos dada, então será escolhido o primeiro obstáculo da função pov e adicionado à função inicial randomObstaculo com o novo obstáculo introduzido na lista
    | ran >= 81 && ran <= 100 && length pov == 2 = (!!) pov 1 : randomObstaculo l (t,[(!!) pov 1]) xs  -- as únicas coisas que diferem da linha anterior para esta é que se o valor estiver compreendido entre 81 e 100, então é escolhido o segundo obstáculo da função pov. Esta descrepância de probabilidades foi feita de maneira a melhorar a experiência do jogador, uma vez que é necessário que haja mais Troncos do que Nenhuns no tipo de terreno Rio e mais Nenhums do que Carros/Árvores nos outros tipos de terreno, sendo por isso planeado a ordem dos obstáculos da função proximosObstaculosValidos
    | length pov == 1 = pov ++ randomObstaculo l (t,pov) xs  -- no caso em que a função pov apenas devolve um obstáculo, então apenas adiciona-se esse mesmo obstáculo à lista
    where ran = mod x 101
          pov = proximosObstaculosValidos l (t,[])  -- a função proximosObstaculosValidos dispõe os obstaculos passíveis de serem utilizados no mesmo terreno

randomObstaculo l (t,o) (x:xs)
    | l == length o = []  -- este caso foi feito de maneira a, quando a lista de obstáculos equivale à largura do mapa, então a função para
    | ran >= 0 && ran <= 80 && length pov == 2 = (!!) pov 0 : randomObstaculo l (t, o ++ [(!!) pov 0]) xs
    | ran >= 81 && ran <= 100 && length pov == 2 = (!!) pov 1 : randomObstaculo l (t, o ++ [(!!) pov 1]) xs
    | length pov == 1 = pov ++ randomObstaculo l (t,o ++ pov) xs
    where ran = mod x 101
          pov = proximosObstaculosValidos l (t,o)