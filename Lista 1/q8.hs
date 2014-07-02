primosEntreSi :: [Int] -> [Int]
primosEntreSi [] = []
primosEntreSi lista = primos as [a]
	where (a:as) = qs lista

qs :: [Int] -> [Int]
qs [] = []
qs (a:as) = qs [x | x <- as, x <= a] ++ [a] ++ qs [y | y <- as, y > a]

primos :: [Int] -> [Int] -> [Int]
primos [] retorno = retorno
primos (a:as) retorno 
	| (verificar a retorno) = primos as (retorno ++ [a])
	| otherwise = primos as retorno

verificar :: Int -> [Int] -> Bool
verificar n [] = True
verificar n (a:as)
	| (verificarDois n a) = verificar n as 
	| otherwise = False

verificarDois :: Int -> Int -> Bool
verificarDois a b 
	| ((mai `mod` men) == 1) || (men == 1) = True
	| ((mai `mod` men) == 0) = False
	| otherwise = verificarDois	men (mai `mod` men)
	where 
		mai = max a b
		men = min a b