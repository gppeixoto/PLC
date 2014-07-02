class PrimeirosPrimos n where
    primeFib :: n -> [n]
    primeFat :: n -> [n]

instance PrimeirosPrimos Integer where
    primeFib n = filtrarTamanho n (filter ehPrimo (map fib [1..]))
    primeFat n = filtrarTamanho n (filter ehPrimo (map fat [1..(2*n)]))

filtrarTamanho :: Integer -> [Integer] -> [Integer]
filtrarTamanho 0 lista = []
filtrarTamanho n [] = []
filtrarTamanho n (a:as) = a: filtrarTamanho (n-1) as

ehPrimo :: Integer -> Bool
ehPrimo 1 = False
ehPrimo n = verificar n [1..(n-1)]

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

fat :: Integer -> Integer
fat 0 = 1
fat n = n*fat (n-1)

verificar :: Integer -> [Integer] -> Bool
verificar n [] = True
verificar n (a:as)
	| (verificarDois n a) = verificar n as 
	| otherwise = False

verificarDois :: Integer -> Integer -> Bool
verificarDois a b 
	| ((mai `mod` men) == 1) || (men == 1) = True
	| ((mai `mod` men) == 0) = False
	| otherwise = verificarDois	men (mai `mod` men)
	where 
		mai = max a b
		men = min a b