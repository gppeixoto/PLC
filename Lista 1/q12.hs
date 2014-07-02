getLast :: String -> Char
{-retorna o último caractere de uma string-}
getLast [] = error "empty string"
getLast [x] = x
getLast (x:xs)
 | (xs == []) = x
 | otherwise = getLast xs

cutFirst :: String -> String
{-retorna uma string sem o primeiro char -}
cutFirst [x] = []
cutFirst (x:xs) = xs

reverseStr :: String -> String
{-retorna o inverso de uma string-}
reverseStr [] = []
reverseStr (x:xs) = (reverseStr xs) ++ [x]

cutLast :: String -> String
{-retorna uma string sem o último char-}
cutLast xs = reverseStr (cutFirst (reverseStr xs))

isPalindromo :: String -> Bool
{-confere se uma string é palíndroma-}
isPalindromo [] = True
isPalindromo [x] = True
isPalindromo (x:xs)
 | (getLast xs == x) = isPalindromo (cutLast xs)
 | otherwise = False

substr_size_n :: String -> Int -> [String]
{-retorna todas as substrings de tamanho N de uma string-}
substr_size_n [] _ = []
substr_size_n xs n
 | (length xs == n) = [xs]
 | otherwise = (substr_size_n (cutFirst xs) n) ++ (substr_size_n (cutLast xs) n)

filtra_palindromos :: String -> Int -> [String]
{-retorna todas as substrings palindromas de mesmo tamanho N (ou menor) -}
filtra_palindromos [] _ = []
filtra_palindromos xs n
 | ((filter isPalindromo (substr_size_n xs n)) /= []) = filter isPalindromo (substr_size_n xs n)
 | otherwise = filtra_palindromos xs (n-1)

quicksort :: Ord t => [t] -> [t]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y | y <- xs, y >= x]

tira_iguais :: [String] -> [String]
{-remove elementos repetidos-}
tira_iguais [] = []
tira_iguais [x] = [x]
tira_iguais [x,y] = if (x==y) then [x] else [x,y]
tira_iguais (x:y:xs) = if (x==y) then tira_iguais (x:xs) else x:(tira_iguais (y:xs))

maioresPalindromos :: String -> [String]
maioresPalindromos xs = tira_iguais (quicksort (filtra_palindromos xs (length xs)))