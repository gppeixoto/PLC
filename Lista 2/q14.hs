import System.Random

rng :: (Int, Int) -> IO(Int)
rng (i, j) = do {
r <- randomRIO(i,j);
return r
}

type Rank = Int -- 1 a 25
type Espera = Int -- 0 a 100
type Id = Int
data User = Usuario Id Rank Espera deriving (Show, Eq)

generate :: [Id] -> [User]
generate [] = []
generate (x:xs) = [Usuario x (randomRIO(1,25)) (randomRIO(0,100))] ++ generate xs 

sort :: [User] -> [User]
sort [] = []
