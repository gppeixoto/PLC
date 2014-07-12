{-
4) Implemente, usando MVars, um sistema de oferendas para uma pequena
vila. A vila possui n fazendeiros e m deuses os quais devem receber as
oferendas (m e n definidos pelo usuário). Cada fazendeiro produz oferendas e
cada deus consome uma oferenda continuamente. Quando um deus vai
consumir suas oferendas, porém não há nenhuma produzida, este mata um
dos fazendeiros. Quando um fazendeiro vai produzir uma oferenda, mas a
cesta de oferendas está cheia (ao atingir um limite x indicado pelo usuário), o
fazendeiro volta pra casa, fica com sua esposa e produz um filho (este torna-se
imediatamente um fazendeiro). O programa encerra quando os deuses tiverem
consumido p oferendas (dado do usuário), ou quando não houver nenhum
fazendeiro vivo. Em seu programa, deuses e fazendeiros devem ser modelados
como threads.

-}


module Main where
import Control.Parallel as Parallel
import Control.Concurrent as Concurrent
import Control.Concurrent.MVar as MVar

waitThreads :: MVar Int -> IO ()
waitThreads fim =
    do f <- takeMVar fim
       if (f > 0) then
                do putMVar fim f
                   waitThreads fim
       else
            return ()

oper::(Int->Int->Int)->MVar Int->MVar Int->Int->IO()
oper op cont fim 0
    = do v <- takeMVar cont -- remove mas não recoloca!
         putStrLn (show v)
         f <- takeMVar fim
         putMVar fim (f-1)
oper op cont fim num
    = do v <- takeMVar cont
         putMVar cont (op v 1)
         oper op cont fim (num-1)


main :: IO ()
main = do   contador <- newMVar 0
            fim <- newMVar 2
            forkIO (oper (+) contador fim 100)
            forkIO (oper (-) contador fim 100)
            waitThreads fim
            return ()
