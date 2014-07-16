import Control.Concurrent
import Control.Concurrent.STM as Concurrent
import System.Random as Random

type BarberTVar = TVar Int      --working:1 / sleeping:0
type ChairTVar = TVar Int       --busy/free
type FreeSeats = MVar Int       --how many free seats are left
type ID = Int

customer :: ID -> BarberTVar -> ChairTVar -> MVar Int -> IO()
customer id barb cadeiras mv = do { 
        v <- atomically (readTVar cadeiras); --v contem o numero de cadeiras livres, o numero total de cadeiras é 5
        if (v <= 0) --se n tem mais nenhuma cadeira vazia
        then putStrLn $ "Customer " ++ (show id) ++ " is leaving! No seats left :-(\n" --sai do barbershop
        else do { --aqui tem alguma cadeira livre
            --x <- takeMVar mv; --dimnui em 1 o numero de clientes atendidos
            --putMVar mv (x-1);
            atomically (writeTVar cadeiras (v-1)); --senta em uma cadeira, uma cadeira livre a menos
            atomically (do { 
                            busy <- readTVar barb; --ve se o barbeiro ta dormindo ou acordado
                            if (busy == 1) then retry else return (); --se tiver ocupado fica em busy wait
                            } --o return () serve p nao fazer nada e ir pra threadDelay
                );
            threadDelay $ 2000000; --espera o tempo de corte
            putStrLn $ "Customer " ++ (show id) ++ " got a haircut! :-D\n"; --cortando o cabelo...
        };
        --x <- takeMVar mv; --dimnui em 1 o numero de clientes atendidos
        --putMVar mv (x-1)
    }

barber :: ID -> BarberTVar -> ChairTVar -> MVar Int -> Int -> IO()
barber id barb cadeiras mv numeroCadeiras = do {
    atomically (   --monad STM
            do {
                v <- readTVar cadeiras; --ve o numero de cadeiras vazias
                if (v >= numeroCadeiras) --se todas as cadeiras estao vazias
                then retry --bota que ta desocupado, tenta de novo
                else writeTVar barb 1;
                });
    --monad IO
    threadDelay 2000000; --cortando o cabelo...
    --putStrLn $ "Acabei um corte.";
    x <- takeMVar mv; --diminui em 1 o número de clientes atendidos
    putMVar mv (x-1);
    atomically (do { --nesse ponto ele ja cortou o cabelo
        v <- readTVar cadeiras; --um cliente foi atendido
        writeTVar cadeiras (v+1); --entao tem uma cadeira vazia
        });
    if ((x-1) < 1) --se chegou em 0, acabou o servico
    then return ()
    else do {atomically (writeTVar barb 0); barber id barb cadeiras mv numeroCadeiras} --senao, espera ate que complete o numero de clientes atendidos
}

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
incomingCustomers n tvarBarbeiro tvarCadeiras mvarClientes = do {
    remaining <- takeMVar mvarClientes;
    putMVar mvarClientes remaining;
    if (remaining > 0)
    then do {
        delay <- Random.randomRIO(1000000, 3000000);
        threadDelay delay;
        forkIO $ customer n tvarBarbeiro tvarCadeiras mvarClientes;
        incomingCustomers (n+1) tvarBarbeiro tvarCadeiras mvarClientes
    }
    else return ();
}

main :: IO()
main = do {
    putStrLn "Input the number of customers: ";
    nc <- getLine;
    numeroClientes <- return (read nc :: Int);
    mvarClientes <- newMVar numeroClientes;
    putStrLn "Input the number of chairs: ";
    nchairs <- getLine;
    numeroCadeiras <- return (read nchairs :: Int);
    tvarCadeiras <- atomically (newTVar numeroCadeiras);
    tvarBarbeiro <- atomically (newTVar 0);
    forkIO $ barber 40 tvarBarbeiro tvarCadeiras mvarClientes numeroCadeiras;
    incomingCustomers 1 tvarBarbeiro tvarCadeiras mvarClientes;
    waitThreads mvarClientes;
    putStrLn $ "I have cut today " ++ (show numeroClientes) ++ " customers! I'm done! zzZZz"
}