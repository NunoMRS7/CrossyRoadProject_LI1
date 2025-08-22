{- |
Module      : Tarefa1_2022li1g018
Description : Validação de um mapa
Copyright   : Eduardo de Oliveira Sousa Faria <a104353@alunos.uminho.pt>
              Nuno Miguel Ribeiro da Silva <a104089@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}


module Tarefa1_2022li1g018 where
import LI12223




{- | Esta função determina se um dado mapa é válido ou não através de várias condições -}
mapaValido :: Mapa -> Bool

mapaValido (Mapa l []) = False  -- Caso o mapa introduzido não apresente terrenos e obstáculos, o mapa é inválido

mapaValido (Mapa l [(t,o)])  -- A função com apenas um elemento tem de verificar a maior parte das condições da função mapaValido para mais do que um elemento, com exceção das funções que comparam dois elemtentos da lista
    | l <= 0 = False
    | not (terrenoChecker t o) = False
    | not (troncoChecker l o) || not (troncoCheckerPontas o) = False
    | not (elemTronco (t,o)) = False
    | not (carroChecker l o) || not (carroCheckerPontas o) = False
    | Nenhum `notElem` o = False
    | not (velocidadeNulaChecker t) = False
    | l /= length o = False
    | otherwise = True  -- Como, neste caso, o mapa apresenta apenas um elemento, não necessitamos de ver se as condições são verificadas pelo elemento seguinte, o que significa que, caso um mapa com um elemento chegue até esta guarda, então o mapa é válido e a função retorna o valor lógico True 

mapaValido (Mapa l n@((t,o):xs))  -- "l" equivale à largura (Int), "t" equivale ao terreno (Rio, Estrada, Relva) e "o" equivale à lista de obstáculos (Nenhum, Tronco, Carro, Árvore)
    | l <= 0 = False  -- largura inferior ou igual a 0 não faz sentido, logo, o mapa é automáticamente inválido
    | not (terrenoChecker t o) = False  -- quando a função deteta se um tipo de terreno não corresponde aos obstáculos a si atribuidos o mapa é inválido
    | not (rioChecker n) = False  -- quando a função deteta se dois Rios seguidos não possuem velocidades opostas, i.é., não possuem sentidos de movimento opostos, o mapa é inválido
    | not (troncoChecker l o) || not (troncoCheckerPontas o) = False  -- quando a função deteta se, apenas no caso em que o terreno se trata de um Rio, na lista de objetos existe um Tronco maior do que 5 unidades de comprimento (tanto no meio da lista como nas pontas, dividido pelos limites), o mapa é inválido
    | not (elemTronco (t,o)) = False  -- quando a função deteta se, apenas no caso em que o terreno se trata de um Rio, não há a presença de um Tronco (no caso do Rio, se não houver Troncos, o jogador automaticamente cairá), o mapa é inválido
    | not (carroChecker l o) || not (carroCheckerPontas o) = False  -- quando a função deteta se, semelhantemente à função troncoChekcer, apenas no caso em que o terreno se trada de uma Estrada, na lista de objetos existe um Carro maior do que 3 unidades de comprimento (tanto no meio da lista como nas pontas, dividido pelos limites), o mapa é inválido
    | Nenhum `notElem` o = False  -- quando, na lista de obstáculos a ser avaliada, não há a presença de um Nenhum, o mapa é inválido
    | not (velocidadeNulaChecker t) = False  -- quando a função deteta se, quando o tipo de terreno possui uma velocidade associada a ele, esta é igual a 0, sendo o mapa é inválido
    | l /= length o = False  -- neste caso, se o número de obstáculos numa lista não coincidir com a largura do mapa, então o mapa é inválido
    | not (terrenoContiguo (group (firsts n))) = False  -- quando a função deteta se, dada uma lista agrupada apenas pelos tipos de terrenos, houver mais do que 4 Rios contíguos e 5 Estradas/Relvas contíguas, então o mapa é inválido
    | otherwise = mapaValido (Mapa l xs)  -- caso um mapa passe os testes anteriores, é avaliada a segunda linha do mapa e assim sucessivamente










----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{- | As funções seguintes são algumas funções auxiliares que ajudam na defenição de outras funções.
A função group, dada uma lista, agroupa os elementos iguais, 
    ex.: group [1,2,2,3,4,4,4,5,4] -> [[1],[2,2],[3],[4,4,4],[5],[4]] -}
group :: Eq a => [a] -> [[a]]

group [] = []
group [x] = [[x]]
group (h:t)
    | h `elem` head r = (h : head r) : tail r
    | otherwise = [h] : r
    where r = group t



{- | A função firsts, dada uma lista de tuplos, devolve o primeiro elemento de cada tuplo. Esta função
teve de ser mudada de forma a ignorar a velocidade associada aos terrenos Rio e Estrada, devolvendo uma
lista de Strings que correspondem ao tipo de terreno ("Rio"/"Estrada"/"Relva") 
    ex.: firsts [(Rio 3, [Tronco,Tronco,Nenhum]),(Relva, [Arvore,Arvore,Nenhum])] -> ["Rio","Relva"] -}
firsts :: [(Terreno,[Obstaculo])] -> [String]

firsts [] = []
firsts ((Rio a,b):t) = "Rio":firsts t
firsts ((Estrada a,b):t) = "Estrada":firsts t
firsts ((Relva,b):t) = "Relva":firsts t



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
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------







{- | Esta função determina se, num determinado terreno, certos elementos podem ou não existir.
É necessário fazer três casos diferentes uma vez que é a única maneira que temos de distinguir
os diferentes tipos de terreno -}
terrenoChecker :: Terreno -> [Obstaculo] -> Bool


terrenoChecker (Rio v) [] = True
terrenoChecker (Rio v) (h:t)
    | h == Nenhum || h == Tronco = terrenoChecker (Rio v) t
    | otherwise = False

terrenoChecker Relva [] = True
terrenoChecker Relva (h:t)
    | h == Nenhum || h == Arvore = terrenoChecker Relva t
    | otherwise = False

terrenoChecker (Estrada v) [] = True
terrenoChecker (Estrada v) (h:t)
    | h == Nenhum || h == Carro = terrenoChecker (Estrada v) t
    | otherwise = False





{- | Esta função, dada uma lista de terrenos e os seus obstáculos, de um mapa, verifica se os rios 
contíguos têm direções opostas. Semenlhantemente à função terrenoChecker, é necessário fazer vários
casos dependendo de como se apresentam os terrenos -}
rioChecker :: [(Terreno, [Obstaculo])] -> Bool

rioChecker [] = True
rioChecker [(Rio v, o)] = True  -- quando apenas é apresentado um só Rio e os seus obstáculos, então a função devolve automaticamente True
rioChecker ((Rio v1,o1):(Rio v2,o2):xs)  -- apenas neste caso é que a função realmente compara as direções de dois Rios
    | (v1 < 0 && v2 > 0) || (v1 > 0 && v2 < 0) = rioChecker ((Rio v2,o2):xs)  -- caso as velocidades sejam de sinais opostos, então são vistos os proximós rios recursivamente
    | otherwise = False

rioChecker ((Rio _,_):xs) = rioChecker xs  -- caso o primeiro elemento de uma lista de terrenos e os seus obstáculos seja do tipo Rio mas o segundo não, não é necessária qualquer verificação

rioChecker (x:xs) = rioChecker xs  -- o mesmo se aplica quando o primeiro elemento de uma lista de terrenos e os seus obstáculos não seja do tipo Rio





{- | Esta função, dado um valor de largura e uma lista de obstáculos, verifica se, caso a lista de obstáculos
possua troncos, estes não ultrapassam as 5 unidades -}  
troncoChecker :: Largura -> [Obstaculo] -> Bool

troncoChecker l [] = True
troncoChecker l (h:t)
    | l <= 6 = True  -- quaiquer que sejam os obstáculos dados, se a largura associada ao mapa for inferior, não é necessário verificar se o tronco pode ter no máximo 5 unidades pois primeiro passa na função que verifica se o número de obstáculos corresponde com a largura do mapa, ou seja, neste parâmetro, o único "errado" seria: para uma largura de 6 a lista ["Tronco","Tronco","Tronco","Tronco","Tronco","Tronco"]. Consideramos isto verdadeiro pois, mais à frenta verificamos se existe algum Nenhum na lista, onde tal não se verificaria
    | length (head (group (h:t))) >= 6 && elem Tronco (head (group (h:t))) = False  -- quando o primeiro elemento da lista agrupada de uma lista dada for composto por 6 ou mais obstáculos do tipo Tronco, então a função retorna False
    | otherwise = troncoChecker l t  -- caso não sejam verificadas as condições anteriores, a função testa os próximos obstáculos da lista recursivamente





{- | Esta função, recebe uma lista de obstáculos e verifica se, juntando os troncos das extremidades das listas,
se forma um tronco com comprimento superior a 5 unidades -}
troncoCheckerPontas :: [Obstaculo] -> Bool

troncoCheckerPontas o  -- os casos que se seguem são todas as variações necessárias de casos em que a junção de troncos nas extremidades resultaria num tronco com mais de 5 unidades
    | [Tronco] `isPrefixOf` o && [Tronco,Tronco,Tronco,Tronco,Tronco] `isSuffixOf` o = False
    | [Tronco,Tronco] `isPrefixOf` o && [Tronco,Tronco,Tronco,Tronco] `isSuffixOf` o = False
    | [Tronco,Tronco,Tronco] `isPrefixOf` o && [Tronco,Tronco,Tronco] `isSuffixOf` o = False
    | [Tronco,Tronco,Tronco,Tronco] `isPrefixOf` o && [Tronco,Tronco] `isSuffixOf` o = False
    | [Tronco,Tronco,Tronco,Tronco,Tronco] `isPrefixOf` o && [Tronco] `isSuffixOf` o = False
    | otherwise = True





{- | Esta função, dado um tipo de terreno e uma lista de obstáculos, verifica se, apenas quando o terreno em causa 
é o rio, poussui na sua lista de obstáculos pelo menos um tronco -}
elemTronco :: (Terreno,[Obstaculo]) -> Bool

elemTronco (Rio _,o) = Tronco `elem` o  -- apenas quando o tipo de terreno é Rio é necessária a verificação da existência de pelo menos um tronco, sendo por isso que para os outros tipos de terreno, a função devolve automaticamente True
elemTronco (Estrada _,o) = True
elemTronco (Relva,o) = True





{- | Esta função, dado um valor de largura e uma lista de obstáculos, verifica se, caso a lista de obstáculos
possua carros, estes não ultrapassam as 3 unidades -}
carroChecker :: Largura -> [Obstaculo] -> Bool

carroChecker l [] = True
carroChecker l (h:t)
    | l <= 4 = True -- esta guarda é idêntica à da função troncoCheker, mundando apenas a largura necessária para ser automaticamente considerada verdadeira
    | length (head (group (h:t))) >= 4 && elem Carro (head (group (h:t))) = False  -- semelhantemente à função troncoChecker, quando, numa lista agrupada, houver uma lista com, ou mais do que quatro elementos do tipo Carro, a função retorna False
    | otherwise = carroChecker l t





{- | Esta função recebe uma lista de obstáculos e verifica se, juntando os Carros das extremidades das listas,
se forma um carro com comprimento superior a 3 unidades -}
carroCheckerPontas :: [Obstaculo] -> Bool

carroCheckerPontas o  -- os casos que se seguem são todas as variações necessárias de casos em que a junção de carros nas extremidades resultaria num carro com mais de 3 unidades
    | [Carro] `isPrefixOf` o && [Carro,Carro,Carro] `isSuffixOf` o = False
    | [Carro,Carro] `isPrefixOf` o && [Carro,Carro] `isSuffixOf` o = False
    | [Carro,Carro,Carro] `isPrefixOf` o && [Carro] `isSuffixOf` o = False
    | otherwise = True





{- | Esta função, dado um tipo de terreno, verifica se, caso este seja rio ou estrada, possui velocidade não nula associada -}
velocidadeNulaChecker :: Terreno -> Bool

velocidadeNulaChecker Relva = True -- como a relva não possui uma velocidade associada, este caso é automaticamente verdadeiro
velocidadeNulaChecker (Rio v) = v/= 0
velocidadeNulaChecker (Estrada v) = v/=0





{- | Esta função, dada uma lista agrupada (pelo mesmo tipo) de strings (que representam os terrenos), verifica se, atendendo
ao tipo de terreno, o número máximo de terrenos contiguos é 4 para os Rios e 5 para as Estradas e Relvas -}
terrenoContiguo :: [[String]] -> Bool

terrenoContiguo [] = True
terrenoContiguo h
    | elem "Rio" (head h) && length (head h) > 4 = False  -- devido à natureza da função firsts (devolve uma lista de Strings), caso a palavra "Rio" se encontre presente na lista agrupada apenas pelos tipos de terrenos (usando a função group) e o tamanho do primeiro elemento da mesma lista for superior a 4, significa que há 5 ou mais Rios seguidos, sendo por isso que a função devolve False
    | elem "Estrada" (head h) && length (head h) > 5 = False  -- quando o tipo de terreno é Estrada ou Relva e, semelhantemente ao caso anterior, o primeiro elemento da lista agrupada pelos tipos de terrenos é superior a 5, então há 6 ou mais terrenos do tipo Estrada/Relva, sendo por isso que a função devolve False
    | elem "Relva" (head h) && length (head h) > 5 = False
    | otherwise = terrenoContiguo (tail h)