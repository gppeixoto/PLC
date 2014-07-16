import Control.Concurrent
import Control.Concurrent.STM as Concurrent
import System.Random as Random

{-
Tem que usar 3 TVars
uma pra acordar o barbeiro
uma pra cadeira de espera(s)
uma pra cadeira do barbeiro
-}

waitCut = 5000000

type BarberTVar = TVar Int      --working:1 / sleeping:0
type ChairTVar = TVar Int       --busy/free
type FreeSeats = MVar Int       --how many free seats are left
type ID = Int

customer :: ID -> BarberTVar -> ChairTVar -> MVar Int -> IO()
customer id barb cadeiras mv = do { 
        v <- atomically (readTVar cadeiras); --v contem o numero de cadeiras livres, o numero total de cadeiras é 5
        if (v == 0) --se n tem mais nenhuma cadeira vazia
        then putStrLn $ "Thread " ++ (show id) ++ " leaving, no seats left." --sai do barbershop
        else do { --aqui tem alguma cadeira livre
            atomically (writeTVar cadeiras (v-1)); --senta em uma cadeira, uma cadeira livre a menos
            atomically (do { 
                            busy <- readTVar barb; --ve se o barbeiro ta dormindo ou acordado
                            if (busy == 1) then retry else return (); --se tiver ocupado fica em busy wait
                            } --o return () serve p nao fazer nada e ir pra threadDelay
                );
            putStrLn $ "Thread " ++ (show id) ++ " getting a haircut."; --cortando o cabelo...
            threadDelay $ waitCut --espera o tempo de corte
        };
        --x <- takeMVar mv; --dimnui em 1 o numero de clientes atendidos
        --putMVar mv (x-1)
    }

barber :: ID -> BarberTVar -> ChairTVar -> MVar Int -> IO()
barber id barb cadeiras mv = do {
    atomically (   --monad STM
            do
                v <- readTVar cadeiras --ve o numero de cadeiras vazias
                if (v == numeroCadeiras) --se todas as cadeiras estao vazias
                then do {writeTVar barb 0; retry} --bota que ta desocupado, tenta de novo
                else do { --tem uma cadeira nao vazia --> tem um cliente esperando
                        writeTVar barb 1; --fica ocupado pra cortar o cabelo
                        --writeTVar cadeiras (v+1);
                    }
        );
    --monad IO
    threadDelay waitCut; --cortando o cabelo...
    atomically (do { --nesse ponto ele ja cortou o cabelo
        v <- readTVar cadeiras; --um cliente foi atendido
        writeTVar cadeiras (v+1); --entao tem uma cadeira vazia
        writeTVar barb 0; --e ele tá desocupado
        });
    --debug--------------------------------
    --num <- atomically (readTVar cadeiras);
    --num2 <- atomically (readTVar barb);
    --putStrLn $ "barber cadeiras: " ++ (show num) ++ " barb: " ++ (show num2);
    putStrLn $ "Acabei um corte.";
    --debug--------------------------------
    x <- takeMVar mv; --diminui em 1 o número de clientes atendidos
    putMVar mv (x-1);
    if (x == 1) --se chegou em 0, acabou o servico
    then putStrLn $ "Atendi todos os meus clientes hoje!" --return () --e faz nada
    else barber id barb cadeiras mv --senao, espera ate que complete o numero de clientes atendidos
}

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
    continue <- takeMVar mvarClientes;
    putMVar mvarClientes continue;
    if (continue > 0) then
        do {
            forkIO $ customer n tvarBarbeiro tvarCadeiras mvarClientes;
            putStrLn $ "Thread " ++ (show n) ++ " walking into the barbershop.";
            delay <- Random.randomRIO(1000000, 3000000);
            threadDelay delay;
            incomingCustomers (n-1) tvarBarbeiro tvarCadeiras mvarClientes
        }
    else return ();
}

main :: IO()
main = do {
    putStrLn "How many customers will the barber cut today: ";
    num <- getLine;
    n <- return (read num :: Int); 
    mvarClientes <- newMVar n;
    tvarCadeiras <- atomically (newTVar numeroCadeiras);
    tvarBarbeiro <- atomically (newTVar 0);
    forkIO $ barber 40 tvarBarbeiro tvarCadeiras mvarClientes;
    incomingCustomers 1000000    tvarBarbeiro tvarCadeiras mvarClientes;
}