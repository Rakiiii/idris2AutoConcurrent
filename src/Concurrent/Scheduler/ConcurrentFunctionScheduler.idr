module Concurrent.Scheduler.ConcurrentFunctionScheduler

import public Concurrent.Utils.IR

import public System
import public System.Concurrency
import public System.Future

-- Все не так, надо стартовать функцию которая возвращает IO ()
runFunctionInFuture : a -> (a -> IO ()) -> IO $ Future $ IO ()
runFunctionInFuture x f = forkIO $ do pure $ f x

-- tmp
public export
concatIO : IO (List (IO ())) -> IO ()
concatIO x = do 
    list <- x
    foldl (<+>) (do pure ()) list

-- Должна быть еще результирующая функция получающая результат из последнего канала
-- сделать функцию которая будет получать данные из последнего канала и ждать???
public export
runConcurrent : a -> List (a -> IO ()) -> IO () -- IO b
runConcurrent _ [] = pure ()
runConcurrent argument functions = do
    trav <- traverse (runFunctionInFuture argument) functions
    foldl (<+>) (do pure ()) $ map await trav

public export
runConcurrentTest : Channel b -> a -> List (a -> IO () ) -> IO b
-- imposible due to design
-- попробовать перейти к List1 чтобы не рассматривать случай пустого списка
runConcurrentTest channel _        []        = do channelGet channel 
runConcurrentTest channel argument functions = do
    trav <- traverse (runFunctionInFuture argument) functions
    foldl (<+>) (do pure ()) $ map await trav
    channelGet channel

public export
runConcurrentFork : Channel b -> a -> List (a -> IO () ) -> IO b
-- imposible due to design
-- попробовать перейти к List1 чтобы не рассматривать случай пустого списка
runConcurrentFork channel _        []        = do channelGet channel 
runConcurrentFork channel argument functions = do
    let ids = map (\func => do Prelude.fork $ func argument) functions
    let unit = map (\id => do threadWait !id) ids
    let res = foldl (<+>) (do pure ()) unit
    channelGet channel


public export
runConcurrentFork' : Channel b -> a -> List (a -> IO () ) -> IO ()
runConcurrentFork' channel _        []        = do pure ()
runConcurrentFork' channel argument functions = do
    let ids = map (\func => do Prelude.fork $ func argument) functions
    let unit = map (\id => do threadWait !id) ids
    foldl (<+>) (do pure ()) unit
