    
import Control.Parallel
import Control.Concurrent
import Control.Concurrent.MVar

-------------------------------------------------
------------Testes no final do código------------
-------------------------------------------------
------------------ SEQUENTIAL -------------------
-------------------------------------------------

merge :: (Ord t) => [t] -> [t] -> [t]
merge [] [] = []
merge [] l = l
merge l [] = l
merge (x:xs) (y:ys) = if (x <= y) then (++) [x] $ merge xs (y:ys) else (++) [y] $ merge (x:xs) ys

mergeSort :: (Ord t) => [t] -> [t]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l =   let (left, right) = (splitAt (div (length l) 2) l)
                in merge (mergeSort left) (mergeSort right)

-------------------------------------------------
------------------- PARALLEL --------------------
-------------------------------------------------

{-
Since the parallel version is hardcoded to do the MergeSort with
four threads, here we do some auxiliary functions to split a list
in a 4-tuple containing four lists.-}
getTwoLists :: (Ord t) => [t] -> ([t], [t])
getTwoLists l = splitAt (div (length l) 2) l

splitInFour :: (Ord t) => ([t], [t]) -> (([t], [t]), ([t], [t]))
splitInFour (x,y) = (getTwoLists x, getTwoLists y)

{-
Given a list L containing x sorted lists, returns a sorted list
containing all elements of each list x in L-}
merge_n :: (Ord t) => [[t]] -> [t]
merge_n [] = []
merge_n [l] = l
merge_n (x:y:xs) = merge_n $ (merge x y):xs

{-
Thread is going to get the list passed to it and apply the
mergeSort to the argument.-}
msThread :: (Ord t) => [t] -> MVar [t] -> IO()
msThread arg mvar = pseq f (putMVar mvar f)
                    where f = mergeSort arg

{-
The thread does the work it is supposed to do in the mergeSort plus
it increments the "counter" in the toWait MVar. The toWait param is
further used to prevent the main thread from finishing before its 
children finishes.-}
work :: (Ord t) => [t] -> MVar [t] -> MVar Int -> IO()
work arg mvar toWait = do {
        cont <- takeMVar toWait;
        putMVar toWait (cont+1);
        msThread arg mvar
    }

type NumThreads = Int

{-
Auxiliar fuction. Forces the main thread to wait until the
children threads are done with their work, by keeping the main 
thread in busy wait.-}
waitThreads :: MVar Int -> NumThreads -> IO()
waitThreads mv nt = do {
    f <- takeMVar mv;
    if (f < nt) then
        do {
            putMVar mv f;
            waitThreads mv nt;
        }
    else
        return ()
}

{-
Auxiliar function to help parsing from IO String to a list of Int.-}
readAux x = x >>= (\k -> read k :: [Int])

main :: IO()
main = do {
    putStrLn "Input the list to be mergeSorted by four threads: ";
    input <- getLine;
    list <- return $ readAux (return input);
    toWait <- newMVar 0;
    mvar1 <- newEmptyMVar;
    mvar2 <- newEmptyMVar;
    mvar3 <- newEmptyMVar;
    mvar4 <- newEmptyMVar;
    newList <- return $ splitInFour.getTwoLists $ list;
    --newList <- return $ splitInFour.getTwoLists $ [1..1000000];
    forkIO $ work (fst.fst $ newList) mvar1 toWait;
    forkIO $ work (fst.snd $ newList) mvar2 toWait;
    forkIO $ work (snd.fst $ newList) mvar3 toWait;
    forkIO $ work (snd.snd $ newList) mvar4 toWait;
    waitThreads toWait 4; --mvar1..4 now should all have sorted lists at this point.
    l1 <- takeMVar mvar1; --here we extract the lists from the MVars...
    l2 <- takeMVar mvar2;
    l3 <- takeMVar mvar3;
    l4 <- takeMVar mvar4;
    finalList <- return $ merge_n [l1,l2,l3,l4]; --here we merge them.
    putStrLn $ "Your sorted list: " ++ (show $ finalList)
    --putStrLn $ show $ length finalList
}

{-
Versão sequencial:
*Main> length $ mergeSort [1..500000]
500000
(7.72 secs, 1696971288 bytes)

*Main> length $ mergeSort [1..700000]
700000
(10.86 secs, 2401833552 bytes)

*Main> length $ mergeSort [1..1000000]
1000000
(16.03 secs, 3552689096 bytes)

Versão paralela:
> C:\Users\Guilherme\...\Lista 4>q1.exe +RTS -N4 -s
500000
  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    0.66s  (  0.41s elapsed)
  GC      time    1.67s  (  0.48s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    2.33s  (  0.90s elapsed)

> C:\Users\...\Lista 4>q1.exe +RTS -N4 -s
700000
  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    1.28s  (  0.60s elapsed)
  GC      time    2.36s  (  0.75s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    3.64s  (  1.36s elapsed)

C:\Users\...\Lista 4>q1.exe +RTS -N4 -s
1000000
  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    1.55s  (  0.86s elapsed)
  GC      time    3.39s  (  1.04s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    4.94s  (  1.90s elapsed)
-}