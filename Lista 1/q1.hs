pascal :: Int -> Int -> Int
pascal n m
 | m==0 = 1
 | (m > n) = 0
 | otherwise = (pascal (n-1) (m)) + (pascal (n-1) (m-1))