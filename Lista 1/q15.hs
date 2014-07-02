import Data.Char

cpfFilter :: [String] -> [String]
cpfFilter xs = [x | x <- xs, verificar x]

verificar :: String -> Bool
verificar s
	| (digito (noveDigitos s []) 0 1 == a && digito ((noveDigitos s []) ++ [a]) 0 0 == b) = True
	| otherwise = False
		where (a,b) = digitosFinais s

digito :: [Int] -> Int -> Int -> Int
digito [] result count = result `mod` 11
digito (a:as) result count = digito as (result + (a*count)) (count+1)

noveDigitos :: String -> [Int]-> [Int]
noveDigitos (a:as) ret
	| (a == '-') = ret
	| (a >= '0'  && a <= '9') = noveDigitos as (ret ++ [(ord a) - 48])
	| otherwise = noveDigitos as ret

digitosFinais :: String -> (Int, Int)
digitosFinais (a:as) 
	| (a == '-') = ((ord b) -48, (ord c) - 48)
	| otherwise = digitosFinais as
		where 
			(b:bs) = as
			(c:cs) = bs
