import Control.Concurrent
import Control.Concurrent.STM as Concurrent

{-
Tem que usar 3 TVars
uma pra acordar o barbeiro
uma pra cadeira de espera(s)
uma pra cadeira do barbeiro
-}

waitCut = 2000000

type BarberTVar = TVar Int      --working:1 / sleeping:0
type ChairTVar = TVar Int       --busy/free
type FreeSeats = MVar Int       --how many free seats are left
type ID = Int

customer :: ID -> BarberTVar -> ChairTVar -> MVar Int -> IO()
customer id barb cadeiras mv = do {
        v <- atomically (readTVar cadeiras);
        if (v == 0) 
        then (do 
                putStrLn $ "Thread " ++ (show id) ++ " leaving, no seats left.";
                x <- takeMVar mv
                putMVar mv (x-1)
                ) 
        else do {
            atomically (writeTVar cadeiras (v-1));
            atomically (do { 
                            busy <- readTVar barb;
                            if (busy == 1) then retry else return ();
                            }
                );
            putStrLn $ "Thread " ++ (show id) ++ " getting a haircut.";
            threadDelay $ waitCut
        } 
    }

barber :: ID -> BarberTVar -> ChairTVar -> MVar Int -> IO()
barber id barb cadeiras mv = do {
    atomically (    
            do
            --monad STM
                v <- readTVar cadeiras
                if (v == numeroCadeiras)
                then retry
                else do {
                        writeTVar barb 1;
                        writeTVar cadeiras (v+1);
                    }
        );
    --monad IO
    threadDelay waitCut;
    atomically (writeTVar barb 0);
    x <- takeMVar mv;
    if (x == 0)
    then return ()
    else barber id barb cadeiras mv
}

{--------DEBUG-------}
numeroCadeiras = 5
numeroClientes = 15

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


incomingCustomers :: ID -> BarberTVar -> ChairTVar -> MVar Int -> IO()
incomingCustomers 0 _ _ _ = return ()
incomingCustomers n tvarBarbeiro tvarCadeiras mvarClientes = do {
    forkIO $ customer n tvarBarbeiro tvarCadeiras mvarClientes;
    putStrLn $ "Thread " ++ (show n) ++ " walking into the barbershop.";
    threadDelay 3000000;
    incomingCustomers (n-1) tvarBarbeiro tvarCadeiras mvarClientes
}

main :: IO()
main = do {
    mvarClientes <- newMVar numeroClientes;
    tvarCadeiras <- atomically (newTVar numeroCadeiras);
    tvarBarbeiro <- atomically (newTVar 0);
    incomingCustomers numeroClientes tvarBarbeiro tvarCadeiras mvarClientes;
    forkIO $ barber 0 tvarBarbeiro tvarCadeiras mvarClientes;
    waitThreads mvarClientes;
}