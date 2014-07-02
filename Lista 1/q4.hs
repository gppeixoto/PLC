import Data.Char

shift :: Char -> Int -> Char
shift c n = chr ((ord c) + n)

take_n :: [t] -> Int -> t
take_n (x:xs) n = if (n==0) then x else take_n xs (n-1)

shift_list :: [Int] -> [Int]
shift_list (x:xs) = xs ++ [x]

cifra :: String -> [Int] -> String
cifra [] _ = []
cifra (x:xs) chaves = if (x /= ' ') then ([shift x (take_n chaves 0)] ++ (cifra xs (shift_list chaves))) else ([' '] ++ (cifra xs chaves))