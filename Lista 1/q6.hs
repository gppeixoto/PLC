lookAndSay :: Int -> String
lookAndSay 1 = "1"
lookAndSay n = lookAux "1" 1 (n-1)

lookAux :: String -> Int -> Int -> String
lookAux (a:as) inicio n 
	| (inicio == n) = func a 1 as
	| otherwise = lookAux (func a 1 as) (inicio+1) n

func :: Char -> Int -> String -> String
func c i [] = show i ++ [c]
func c i (a:as) 
	| (a == c) = func c (i+1) as
	| otherwise = (show i) ++ [c] ++ func a 1 as