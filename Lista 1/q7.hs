import Data.Char

mapping :: [Char] -> Char
mapping x
 | x == ".-" = 'A'
 | x == "-..." = 'B'
 | x == "-.-." = 'C'
 | x == "-.." = 'D'
 | x == "." = 'E'
 | x == "..-." = 'F'
 | x == ['-','-','.'] = 'G'
 | x == "...." = 'H'
 | x == ".." = 'I'
 | x == ['.','-','-','-'] = 'J'
 | x == "-.-" = 'K'
 | x == ".-.." = 'L'
 | x == ['-','-'] = 'M'
 | x == "-." = 'N'
 | x == ['-','-','-'] = 'O'
 | x == ['.','-','-','.'] = 'P'
 | x == ['-','-','.','-'] = 'Q'
 | x == ".-." = 'R'
 | x == "..." = 'S'
 | x == "-" = 'T'
 | x == "..-" = 'U'
 | x == "...-" = 'V'
 | x == ['.','-','-'] = 'W'
 | x == "-..-" = 'X'
 | x == ['-','.','-','-'] = 'Y'
 | x == ['-','-','.','.'] = 'Z'
 | (x == "/") = ' '
 | x == "-----" = '0'
 | x == ".----" = '1'
 | x == "..---" = '2'
 | x == "...--" = '3'
 | x == "....-" = '4'
 | x == "....." = '5'
 | x == "-...." = '6'
 | x == "--..." = '7'
 | x == "---.." = '8'
 | x == "----." = '9'
 | x == ".-.-.-" = '.'
 | x == "--..--" = ','
 | x == "..--.." = '?'
 | x == ".----." = '\''
 | x == "-.-.--" = '!'
 | x == "-..-." = '/'
 | x == "-.--." = '('
 | x == "-.--.-" = ')'
 | x == ".-..." = '&'
 | x == "---..." = ':'
 | x == "-.-.-." = ';'
 | x == "-...-" = '='
 | x == ".-.-." = '+'
 | x == "-....-" = '-'
 | x == "..--.-" = '_'
 | x == ".-..-." = '"'
 | x == "...-..-" = '$'
 | x == ".--.-." = '@'

getWord :: [Char] -> [Char]
getWord [] = []
getWord (x:xs)
 | (x /= ' ') = x:(getWord xs)
 | otherwise = []

dropWord :: [Char] -> [Char]
dropWord [] = []
dropWord (x:xs)
 | (x /= ' ') = dropWord xs
 | otherwise = x:xs

dropSpace :: [Char] -> [Char]
dropSpace [] = []
dropSpace (x:xs)
 | (x == ' ') = dropSpace xs
 | otherwise = x:xs

splitWords :: [Char] -> [[Char]]
splitWords [] = []
splitWords (x:xs) = getWord(dropSpace(x:xs)):splitWords(dropWord(dropSpace(x:xs)))

getTogether :: [[Char]] -> [[Char]]
getTogether [] = []
getTogether (x:xs)
 | (x /= "/") = x:xs
 | otherwise = getTogether xs

morseTranslator :: [Char] -> [Char]
morseTranslator xs = map mapping (getTogether (splitWords xs))