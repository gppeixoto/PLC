map_letra :: Char -> Int {-Faz o mapeamento de acordo com a tabela pitagória-}
map_letra n
 | (n=='A' || n=='J' || n=='S' || n=='a' || n=='j' || n=='s') = 1
 | (n=='B' || n=='K' || n=='T' || n=='b' || n=='k' || n=='t') = 2
 | (n=='C' || n=='L' || n=='U' || n=='c' || n=='l' || n=='u') = 3
 | (n=='D' || n=='M' || n=='V' || n=='d' || n=='m' || n=='v') = 4
 | (n=='E' || n=='N' || n=='W' || n=='e' || n=='n' || n=='w') = 5
 | (n=='F' || n=='O' || n=='X' || n=='f' || n=='o' || n=='x') = 6
 | (n=='G' || n=='P' || n=='Y' || n=='g' || n=='p' || n=='y') = 7
 | (n=='H' || n=='Q' || n=='Z' || n=='h' || n=='q' || n=='z') = 8
 | (n=='I' || n=='R' || n=='i' || n=='r') = 9

less10 :: Int -> Bool {-Confere se um número é menor que 10-}
less10 n = (n<10)

sumAll :: [Int] -> Int {-Soma todos os itens de uma lista-}
sumAll [] = 0
sumAll (x:xs) = x + sumAll xs

get_digits :: Int -> [Int] {-Retorna uma lista com os dígitos daquele número-}
get_digits n
 | (less10 n) =  [n]
 | otherwise = (get_digits (div n 10))++[mod n 10]

list_to_digit :: [Int] -> Int {-Soma os dígitos de um número até que a soma seja inferior a 10-}
list_to_digit xs
   | (less10 (sumAll xs)) = answer
   | otherwise = list_to_digit (get_digits (sumAll xs))
   where answer = sumAll xs

nameToNumber :: String -> Int {-Função principal que faz o mapeamento de um nome pra um número-}
nameToNumber xs = list_to_digit (map letra xs)