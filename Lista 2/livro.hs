type Stack a = [a]

pop :: Stack a -> (a, Stack a)
pop (x:xs) = (x, xs)

push :: a -> Stack a -> ((), Stack a)
push a xs = ((), a:xs)