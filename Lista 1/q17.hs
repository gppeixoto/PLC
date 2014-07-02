ordenar :: [Int] -> [Int] -> [Int]
ordenar a [] =  a
ordenar [] b = b
ordenar (a:as) (b:bs) 
	| (a <= b) = a: ordenar as (b:bs)
	| otherwise = b:ordenar (a:as) bs