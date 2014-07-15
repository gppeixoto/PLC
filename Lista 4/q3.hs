import Control.Parallel
import Control.Concurrent
import Control.Concurrent.MVar
import System.Random as Random

{-
Movement rules: a random number will determinate (as it follows) which shells the octopus is going to manipulate:
    1: LeftShell swaps with the MiddleShell
    2: MiddleShell swaps with the RightShell
    3: RightShell swaps with the LeftShell.-}
type Id = Int
octopus :: Id -> MVar Int -> MVar Int -> MVar Int -> MVar Int -> IO()
octopus id fimVar lShell mShell rShell = do {
    fim <- takeMVar fimVar;
    if (fim <= 0)
        then
            putMVar fimVar (-1);
    else do {
        move <- randomRIO(1::Int , 3::Int);
        putStrLn $ "The pair of tentacles (" ++ (show id) ++ ") is going to make a move!";
        case move of {
            1 -> do {
                putStrLn $ "Swapped the left and middle shells";
                left <- takeMVar lShell;
                middle <- takeMVar mShell;
                putMVar lShell middle;
                putMVar mShell left;
            };
            2 -> do {
                putStrLn $ "Swapped the middle and right shells";
                middle <- takeMVar mShell;
                right <- takeMVar rShell;
                putMVar mShell right;
                putMVar rShell middle;
            };
            3 -> do {
                putStrLn $ "Swapped the left and right shells";
                left <- takeMVar lShell;
                right <- takeMVar rShell;
                putMVar lShell right;
                putMVar rShell left;
            };
        };
    putMVar fimVar (fim-1);
    octopus id fimVar lShell mShell rShell;
    };
}

{-
The user inputs an integer, representing one of the three shells. It returns
true if the selected shell contains the stone, otherwise, returns false.-}
makeGuess :: Int -> MVar Int -> MVar Int -> MVar Int -> IO Bool
makeGuess n lShell mShell rShell = do {
    left <- takeMVar lShell;
    middle <- takeMVar mShell;
    right <- takeMVar rShell;
    if (n==1) then 
        if (left == 1) then return True else return False
    else
        if (n==2) then
            if (middle == 1) then return True else return False
        else
            if (right == 1) then return True else return False;
}

{-
This auxiliar function makes the main thread wait in busy wait
until its children threads conclude their work.-}
waitThreads :: MVar Int -> IO()
waitThreads mv = do {
    f <- takeMVar mv;
    if (f > 0) then
        do {
            putMVar mv f;
            waitThreads mv;
        }
    else
        return ()
}

main :: IO()
main = do {
    putStrLn $ "Input the number of movements: ";
    input <- getLine;
    fimVar <- newMVar $ (read input :: Int);
    lShell <- newMVar 0;
    mShell <- newMVar 1;
    rShell <- newMVar 0;
    forkIO $ octopus 1 fimVar lShell mShell rShell;
    forkIO $ octopus 2 fimVar lShell mShell rShell;
    forkIO $ octopus 3 fimVar lShell mShell rShell;
    forkIO $ octopus 4 fimVar lShell mShell rShell;
    waitThreads fimVar;
    putStrLn $ "Make your guess (1 - Left, 2 - Middle, 3 - Right): ";
    guess <- getLine;
    win <- makeGuess (read input :: Int) lShell mShell rShell;
    if (win == True) then putStrLn "You won the game! :-) " else putStrLn "You lost the game! :-(";
    {-The Game. You just lost it.-}
}