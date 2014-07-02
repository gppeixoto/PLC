import Data.Char
data Stack t = Nil | Cons t (Stack t) deriving (Show, Eq)
type Queue = String

push :: t -> Stack t -> Stack t
push t Nil = Cons t Nil
push x (Cons t stack) = Cons x (Cons t stack)

pop :: Stack t -> (t, Stack t)
pop (Cons t stack) = (t, stack)

dropspace :: String -> String
dropspace [] = []
dropspace (x:xs) = if (x == ' ') then dropspace xs else x:(dropspace xs)

is_num :: Char -> Bool
is_num c = ((ord c) >= 48) && ((ord c) <= 57)

is_op :: Char -> Bool
is_op c = (c == '-') || (c == '+') || (c == '*') || (c == '^') || (c == '/')

makealgo :: String -> Stack Char -> Queue
makealgo [] Nil = []
makealgo [] stack
 | (is_op $ fst $ pop stack) = (makealgo [] $ snd $ pop stack) ++ [fst $ pop stack]
 | otherwise = (makealgo [] $ snd $ pop stack)
makealgo (x:xs) stack
 | (is_num x) = (makealgo xs stack) ++ [x]
 | (x == '(') = (makealgo xs (push x stack))
 | (is_op x) = if (stack == Nil) then makealgo xs (push x stack) else (if (is_op $ fst $ pop stack) then (makealgo (x:xs) (snd $ pop stack)) ++ [(fst $ pop stack)] else makealgo xs (push x stack))
 | (x == ')') = if (not ( (fst $ pop stack) =='(')) then (makealgo (x:xs) (snd $ pop stack)) ++ [fst $ pop stack] else (makealgo xs (snd $ pop stack))

printo :: String -> String
printo [] = []
printo (x:xs) = [x] ++ "\n" ++ (printo xs)

makeStack :: String -> IO()
makeStack xs = putStr $ printo $ makealgo (dropspace xs) Nil