testeListas :: [Int] -> [Int]
testeListas (a:as) = (a:[b]) ++ funcAux (mudarEntrada (a:as) 1) (a:as)
	where (b:bs) = as

funcAux :: [(Int,Int)] -> [Int] -> [Int]
funcAux [] [] = []
funcAux lista orig = [x | (x,pos) <- lista, pos > 2 && (maiores x orig pos 1)]

mudarEntrada :: [Int] -> Int -> [(Int,Int)]
mudarEntrada [] pos = []
mudarEntrada (a:as) pos = (a,pos):mudarEntrada as (pos+1)

maiores :: Int -> [Int] -> Int -> Int -> Bool
maiores x (a:as) pos count
	| (count /= (pos-2)) = maiores x as pos (count+1)
	| (count == (pos-2)) && (x>a) = True
	| otherwise = False