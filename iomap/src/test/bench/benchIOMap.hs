module Main
where

  import Data.IOMap
  import System.Environment
  import Data.Char

  import Data.Time.Clock

  main :: IO ()
  main = do
    os <- getArgs
    let mx = case os of
               []  -> 1000
               [x] -> if all isDigit x then read x 
                        else error $ "Not a number: " ++ x
               _   -> error "Unknown arguments!"
    -- insOneToN $! mx
    insNToM 1000 $! mx
    
  upd :: a -> a -> IO a
  upd _ = return 

  insOneToN :: Int -> IO ()
  insOneToN n = do
    t <- empty
    mapM_ (\x -> insert t upd x ()) $! [1..n]
    ts1 <- getCurrentTime
    insert t upd (n + 1) ()
    h <- height t
    ts2 <- getCurrentTime
    let d = diffUTCTime ts2 ts1
    putStrLn $ "Time needed (with height " ++ show h ++ "): " ++ show d

  insNToM :: Int -> Int -> IO ()
  insNToM n m = do
    t <- empty 
    mapM_ (\x -> insert t upd x ()) $! [1..m]
    ts1 <- getCurrentTime
    ins t n m 
    ts2 <- getCurrentTime
    h <- height t
    let d = diffUTCTime ts2 ts1
    putStrLn $ "Time needed (with height " ++ show h ++ "): " ++ show d

  ins :: IOMap Int () -> Int -> Int -> IO ()
  ins t n m | n <= 0     = return ()
            | otherwise  = insert t upd (m+n) () >> ins t (n-1) m

