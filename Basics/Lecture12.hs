module Lecture12 where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (replicateM_, forever, void)



main1 = do
  forkIO $ replicateM_ 5 $ putStr "a"
  replicateM_ 5 $ putStr "b"
  getLine
  return ()

setReminder :: String -> IO() 
setReminder s = do
  let t = read s :: Int
  putStrLn $ "Przypomne sie za " ++ show t ++ " sekund."
  threadDelay (10^6 * t)
  putStrLn $ "Minelo " ++ show t ++ " sekund. Juz czas."

main2 = forever $ do
  s <- getLine
  forkIO $ setReminder s



-- data MVar a
-- newEmptyMVar 
-- takeMVar 
-- putMVar MVar a a
--
--
--

main4 = do
  x <- newEmptyMVar
  putMVar x "a"
  value <- takeMVar x
  putStrLn value
  putMVar x "b"
  value <- takeMVar x
  putStrLn value


inc :: MVar Int -> MVar Int -> IO()
inc v1 v2 = replicateM_ 10000 $ do
  x <- takeMVar v1
  putMVar v1 (x+1)
  y <- takeMVar v2
  putMVar v2 (y+1)
  if x == y then return ()  else print (x,y)


main5 threads = do
  v1 <- newMVar 1
  v2 <- newMVar 1
  mapM_ forkIO  [ t v1 v2 | t <- threads ]

inc' :: TMVar Int -> TMVar Int -> IO()
inc' v1 v2 = replicateM_ 10000 $ do
  (x',y') <- atomically $ do
    x <- takeTMVar v1
    putTMVar v1 (x+1)
    y <- takeTMVar v2
    putTMVar v2 (y+1)
    return (x,y)
    
  if x' == y' then return ()  else print (x',y')

main6 threads = do
  v1 <- newTMVarIO 1
  v2 <- newTMVarIO 1
  mapM_ forkIO  [ t v1 v2 | t <- threads ]




