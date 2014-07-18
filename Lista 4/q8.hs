module Main where
import Control.Concurrent
import Control.Concurrent.STM
import Control.Parallel
import System.Random

type Client = Int
type Buffer = TVar [Client]
type Limit = Int
type Current = TVar Int
type Messages = TVar [String]
type Id = Int

attend :: IO Int
attend = randomRIO (25000, 100000) :: IO Int

arrive :: IO Int
arrive = randomRIO (50000, 500000) :: IO Int

attendant :: Id -> Buffer -> Limit -> Current -> TVar [String] -> IO()
attendant id buf lim curr msgs = do {
    atomically(
        do {
            soFar <- readTVar curr;
            if (soFar >= lim) --the number of customers attended reached the limit
            then
                return ()
            else --still have to attend some number of customers
                do {
                    list <- readTVar buf;   --checks if there is any customer waiting in the buffer
                    if (list == []) then retry
                        else do {  --attending a customer
                            val <- readTVar curr;
                            writeTVar curr (val+1); --increments the number of customers attended
                            client <- return $ head list; --removes the first customer of the queue
                            newBuffer <- return $ tail list;
                            writeTVar buf newBuffer; --updates the buffer
                            messages <- readTVar msgs;
                            novo <- return $ (messages ++ [("Aluno #" ++ (show client) ++ " foi atendido por atendente #" ++ (show id))]);
                            writeTVar msgs novo;
                        };
                }
            }
        );
    time <- attend;
    threadDelay time; --the attendant takes 25ms to 100ms (randomly determined) to attend a costumer
    attendant id buf lim curr msgs
}

putClients :: Int -> Id -> Buffer -> Limit -> Current -> TVar [String] -> IO()
putClients bufferLimit id buf lim curr msgs = do {
    soFar <- atomically $ readTVar curr;
    if (soFar >= lim) --the number of customers attended reached the limit,
    then              --so there will be no more customers "produced"
        return ()
    else
        do {
        atomically ( do{
            list <- readTVar buf;
            if ((length list) == bufferLimit) then --the buffer is full, the customers
                do                                 --does not enter the queue
                    messages <- readTVar msgs;
                    novo <- return $ (messages ++ [("The customer #" ++ (show id) ++ " left!")]);
                    writeTVar msgs novo;
            else --the costumer enters at the end of the queue
                do {
                    writeTVar buf (list ++ [id]);
                    messages <- readTVar msgs;
                    novo <- return $ (messages ++ [("The customer #" ++ (show id) ++ " entered the queue!")]);
                    writeTVar msgs novo;
            };
        });
        delay <- arrive; --a new customer is produced in every 50ms to 500ms (randomly determined)
        threadDelay delay;
        putClients bufferLimit (id + 1) buf lim curr msgs
    };
}

waitThreads :: TVar Int -> Int -> IO()
waitThreads curr lim = atomically(do{
    done <- readTVar curr;
    if (done < lim) then retry else return ()
    })

prettyPrint :: [String] -> IO()
prettyPrint [] = return ()
prettyPrint (x:xs) = do{
    putStrLn x;
    prettyPrint xs
}

main :: IO()
main = do {
    putStrLn $ "Input number of clients to be attended: ";
    numberClients <- getLine >>= (\x -> return (read x :: Int));
    putStrLn $ "Input the max length of the queue: ";
    bufferLimit <- getLine >>= (\x -> return (read x :: Int));
    buf <- atomically $ newTVar [];
    msgs <- atomically $ newTVar [];
    curr <- atomically $ newTVar 0;
    forkIO $ attendant 101 buf numberClients curr msgs;
    forkIO $ attendant 202 buf numberClients curr msgs;
    forkIO $ attendant 303 buf numberClients curr msgs;
    putClients bufferLimit 1 buf numberClients curr msgs;
    waitThreads curr numberClients;
    list <- atomically $ readTVar msgs;
    prettyPrint list;
}

{-
O código acima satisfaz as seguintes propriedades:

- Ausência de deadlocks (liveness)
Para haver deadlocks, é necessário que haja o cumprimento das quatro condições -necessárias-
para que ocorra. No código acima, garantimos a ausência de deadlocks porque não há espera circular,
a quebra dessa condição é suficiente para que argumentemos que não há deadlocks.

- Exclusão mútua (safety)
O fato de usar TVars garante que só uma thread execute por vez (e atomicamente) dentro de blocos
atomically. Dessa forma, temos a garantia de exclusão mútua, uma vez que nunca teremos duas threads
tentando acessar a mesma região crítica simultaneamente.

-Ausência de starvation (liveness)
Uma thread nunca irá entrar em starvation porque ela sempre irá terminar, quando a quantidade de clientes
atendidos chegar ao limite definido pelo usuário. No entanto, não há garantia que todas as três threads atendam
a mesma quantidade de clientes -ou atendam algum-, no entanto, a thread sempre irá terminar a sua tarefa, independente
de quantos alunos ela atendeu. 
-}