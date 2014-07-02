{-Para representar o sudoku, escolhemos representar como uma matriz.
No caso, uma lista de lista de int, onde cada lista de int representa cada linha do sudoku-}

{-A entrada abaixo serve para teste-}

{- sudoku [[8,6,7,4,2,5,3,9,1], [2,1,9,7,6,3,4,5,8] , [4,5,3,1,8,9,7,6,2], [9,3,6,5,1,2,8,4,7], [1,2,8,6,4,7,5,3,9] , [7,4,5,9,3,8,2,1,6] , [5,9,4,2,7,1,6,8,3], [6,8,2,3,9,4,1,7,5], [3,7,1,8,5,6,9,2,4]] -}


sudoku :: [[Int]] -> Bool
sudoku entrada 
	| testarConjuntos entrada && testarConjuntos (gerarColunas entrada) && testarConjuntos (gerarQuadrados entrada) = True
	| otherwise = False

testarConjuntos :: [[Int]] -> Bool
testarConjuntos [] = True
testarConjuntos (a:as)
	| (testarNumeros (qs a) 1) = testarConjuntos as
	| otherwise = False

qs :: [Int] -> [Int]
qs [] = []
qs (a:as) = qs [x | x <- as, x <= a] ++ [a] ++ qs [y | y <- as, y > a]

testarNumeros :: [Int] -> Int -> Bool
testarNumeros (a:as) count
	| (count == 9) && (a == count) = True
	| (a == count) = testarNumeros as (count+1)
	| otherwise = False

gerarColunas :: [[Int]] -> [[Int]]
gerarColunas ((a:as):(b:bs):(c:cs):(d:ds):(e:es):(f:fs):(g:gs):(h:hs):(i:is):js) 
	| (as /= []) = [[a,b,c,d,e,f,g,h,i]] ++ gerarColunas [as,bs,cs,ds,es,fs,gs,hs,is] 
	| otherwise = [[a,b,c,d,e,f,g,h,i]]

gerarQuadrados :: [[Int]] -> [[Int]]
gerarQuadrados (a:b:c:ds) 
	| (ds /= []) = quadAux (gerarColQuad [a,b,c]) ++ gerarQuadrados ds
	| otherwise = quadAux (gerarColQuad [a,b,c])

gerarColQuad :: [[Int]] -> [[Int]]
gerarColQuad ((a:as):(b:bs):(c:cs):ds) 
	| (as /= []) = [[a,b,c]] ++ gerarColQuad [as,bs,cs] 
	| otherwise = [[a,b,c]]


quadAux :: [[Int]] -> [[Int]]
quadAux (a:b:c:ds) 
	| (ds /= []) = [a++b++c] ++ quadAux ds 
	| otherwise = [a++b++c]