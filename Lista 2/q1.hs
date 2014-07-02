bin2double :: String -> Double
bin2double [] = 0.0
bin2double (x:xs)
 | (x=='1') = 2^size + bin2double xs
 | otherwise = bin2double xs
    where size = length (x:xs) - 1


get_cores_str :: String -> (String, String, String)
get_cores_str (a:as) = ([a,b1,c1,d1,e1,f1,g1,h1],[a2,b2,c2,d2,e2,f2,g2,h2],[a3,b3,c3,d3,e3,f3,g3,h3]) 
    where   
        (b1:c1:d1:e1:f1:h1:g1:bs) = as 
        (a2:b2:c2:d2:e2:f2:g2:h2:cs) = bs 
        (a3:b3:c3:d3:e3:f3:g3:h3:[]) = cs

get_rgb_values :: (String, String, String) -> (Double, Double, Double)
get_rgb_values (x,y,z) = (bin2double x, bin2double y, bin2double z)

get_gs_value :: (Double, Double, Double) -> Int
get_gs_value (x,y,z) = round (0.299*x + 0.587*y + 0.114*z)

rgbToGs :: [[String]] -> [[Int]]
rgbToGs [] = []
rgbToGs (x:xs) = [(map (get_gs_value . get_rgb_values . get_cores_str) x)] ++ rgbToGs xs

media :: [[Int]] -> [Int]
media [] = []
media (x:xs) = [div (foldr1 (+) x) (length x)] ++ media xs

mediaF :: [Int] -> Int
mediaF xs = div (foldr1 (+) xs) (length xs)

getMedia :: [[Int]] -> Int
getMedia xs = mediaF . media $ xs


-- x é uma lista

--[["000000001111111100000000", "000010101111111101010011"], ["000000000000000000000000", "101010100101010111111111"]]