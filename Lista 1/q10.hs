myMap :: (t -> u) -> [t] -> [u]
myMap _ [] = []
myMap f l = [f x | x <- l]

myFilter :: (t -> Bool) -> [t] -> [t]
myFilter _ [] = []
myFilter f l = [x | x <- l, f x]

myFoldr :: (t -> u -> u) -> u -> [t] -> u
myFoldr _ b [] = b
myFoldr f b (x:xs) = f x (myFoldr f b xs)