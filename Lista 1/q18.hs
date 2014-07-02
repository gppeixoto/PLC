deleteFromList :: Int -> [Int] -> [Int]
{-Deleta um elemento da lista-}
deleteFromList _ [] = []
deleteFromList a (x:xs) = if (x==a) then xs else x:(deleteFromList a xs)

allPerm :: [Int] -> [[Int]]
{-Gera todas as permutações possíveis de uma lista-}
allPerm [] = [[]]
allPerm xs = [x:ys | x <- xs , ys <- allPerm (deleteFromList x xs)]

getIndex :: Int -> [Int] -> Int
{-Retorna o índice de um elemento na lista-}
getIndex a [] = -1
getIndex a (x:xs) = if (x==a) then 1 else ((getIndex a xs) + 1)

montaPar :: [Int] -> [(Int, Int)]
{-Retorna uma lista com cada elemento e o seu índice-}
montaPar [] = []
montaPar xs = [(x, (getIndex x xs)) | x <- xs ]

modulo :: Int -> Int
{-Calcula modulo de um número-}
modulo x = if (x<0) then (x * (-1)) else x

mesmaDP :: (Int, Int) -> (Int, Int) -> Bool
{-Confere se duas "rainhas" não estão na mesma diagonal principal-}
mesmaDP (x,y) (u, v) = (modulo (u-x)) == (modulo (v-y))

mesmaDS :: (Int, Int) -> (Int, Int) -> Bool
{-Confere se duas "rainhas" não estão na mesma diagonal secundária-}
mesmaDS (x,y) (u, v) = ((x+y) == (u+v))

confere :: (Int, Int) -> (Int, Int) -> Bool
{-Confere se duas rainhas estão se atacando de alguma forma-}
confere (x,y) (u,v) = if (not ( (mesmaDS (x,y) (u,v)) || (mesmaDP (x,y) (u,v)) )) then True else False

confereUm :: (Int, Int) -> [(Int, Int)] -> Bool
{-Confere se uma rainha está atacando a todas às outras (compara uma rainha com todas outras)-}
confereUm _ [] = True
confereUm (x,y) ((u,v):xs) = if (confere (x,y) (u,v)) then confereUm (x,y) xs else False

confereLista :: [(Int, Int)] -> Bool
{-Confere se existe alguma rainha atacando alguma outra rainha (compara todos com todos numa lista)-}
confereLista [] = True
confereLista ((x,y):xs) = if (confereUm (x,y) xs) then confereLista xs else False

take_n :: [t] -> Int -> t
{-Remove o n-ésimo item da lista-}
take_n (x:xs) n = if (n==0) then x else take_n xs (n-1)

arrumarMesas :: Int -> [Int]
{-função final-}
arrumarMesas n = map fst (take_n (filter confereLista (map montaPar (allPerm [1..8]))) n)