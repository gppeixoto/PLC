import System.Random

type Limite = Int
type No = Int
type Aresta = (Int, Int)
data Grafo = Nil | G [No] [Aresta] deriving Show
type Chance = Float

qsort :: (Ord t, Ord u) => [(t,u)] -> [(t,u)] --pra ordenar as probabilidades
qsort [] = []
qsort (x:xs) = qsort [y | y<-xs, (snd y < snd x)] ++ [x] ++ qsort [y | y<-xs, (snd y >= snd x)]

conectaRegular :: No -> Grafo -> IO(Grafo) --conecta um no a um grafo pelo modelo regular
conectaRegular n Nil = return $ G [n] []
conectaRegular 2 (G [1] []) = return $ G [1,2] [(1,2)]
conectaRegular 3 (G [1,2] [(1,2)]) = return $ G [1,2,3] [(1,2), (1,3), (2,3)]
conectaRegular n (G nos arestas) = return $ G (nos ++ [n]) ([(1, n)] ++ [(2, n)] ++ arestas ++ [(n-1, n)])

countConexoes :: Int -> [Aresta] -> Int --conta o numero de conexoes de um nó
countConexoes _ [] = 0
countConexoes n (x:xs) = if (n == fst x || n == snd x) then (+) 1 $ countConexoes n xs else countConexoes n xs

countProbabilidades :: Grafo -> [(No, Chance)] --conta a probabilidade de ser conectado pra usar no modelo livre de escala
countProbabilidades Nil = []
countProbabilidades (G [] arestas) = []
countProbabilidades (G (x:xs) arestas) = [(x , chance)] ++ countProbabilidades (G (xs) arestas) where
    chance = (fromIntegral $ countConexoes x arestas) / (fromIntegral $ length arestas)

getRandom :: IO Float
getRandom = randomRIO ((fromIntegral 0), (fromIntegral 1))

tudoRegular :: Int -> Int -> IO(Grafo) -> IO(Grafo) --cria um grafo inteiramente pelo modelo regular
tudoRegular n limite g = do {
    if (n > limite) then g
        else
            do {
                grafo <- g;
                tudoRegular (n+1) limite (conectaRegular n grafo)
            }
}

conectaLivre :: No -> Grafo -> [(No, Chance)] -> IO(Grafo) --conecta um nó a um grafo pelo modelo livre de escala
conectaLivre 1 Nil _ = return $ G [1] []
conectaLivre 2 (G [1] []) _ = return $ G [1,2] [(1,2)]
conectaLivre n (G nos arestas) ((i,c):xs) = do {
    prob <- getRandom;
    if (prob <= c) then return $ G (nos ++ [n]) (arestas ++ [(i,n)]) else
        conectaLivre n (G nos arestas) xs
}
conectaLivre n (G nos arestas) [] = return $ G (nos ++ [n]) arestas

tudoLivre :: Int -> Int -> IO(Grafo) -> IO(Grafo) --cria um grafo inteiramente pelo modelo livre de escala
tudoLivre n limite g = do {
    if (n > limite) then g
        else
            do {
                grafo <- g;
                tudoLivre (n+1) limite (conectaLivre n grafo (qsort $ countProbabilidades $ grafo))
        }
}

begin :: Int -> Int -> Float -> IO(Grafo) -> IO(Grafo) --loop para adicionar nós a um grafo alternando os modelos
begin n limite p_modelo g = do {
    rnd <- getRandom;
    if (n > limite) then g
        else
            do {
                grafo <- g;
                if (rnd > p_modelo) then do { --conecta como regular
                    begin (n+1) limite p_modelo (conectaRegular n grafo)
                } else do { --conecta como livre de escala
                    begin (n+1) limite p_modelo (conectaLivre n grafo $ qsort $ countProbabilidades $ grafo)
            }
        }
}

printAux ::  Grafo -> String
printAux Nil = ""
printAux (G nos []) = ""
printAux (G nos ((x,y):xs)) = show x ++ "," ++ show y ++ "\n" ++ show y ++ "," ++ show x ++ "\n" ++ (printAux (G nos xs))

printGrafo :: FilePath -> IO(Grafo) -> IO()
printGrafo fp g = do {
    grafo <- g;
    writeFile fp (printAux grafo) 
}

modeloMisto :: Int -> Float -> FilePath -> IO()
modeloMisto 0 _ fp = writeFile fp ("Empty graph")
modeloMisto n p_modelo fp
 | (p_modelo <= 0) = printGrafo fp $ tudoRegular 1 n (return $ Nil)
 | (p_modelo >= 1) = printGrafo fp $ tudoLivre 1 n (return $ Nil)
 | otherwise = printGrafo fp $ begin 1 n p_modelo (return $ Nil)