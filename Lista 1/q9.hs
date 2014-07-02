comprime :: (t -> t -> t) -> [t] -> t
comprime f l = foldr1 f l

aux :: [[t]] -> (t -> t -> t) -> [t]
aux [] _ = []
aux (x:xs) f = [comprime f x] ++ (aux xs f)

foldMatrix :: (t -> t -> t) -> (t -> t -> t) -> [[t]] -> t
foldMatrix f g l = comprime g (aux l f)