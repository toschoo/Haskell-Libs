module Main
where

  import Marriage
  import RandomMarriage

  main :: IO ()
  main = do 
    (ms,ws) <- arbMarriage 1000
    putStrLn $ "Ready: " ++ show (sum $ map prefsL ms) ++ ", " 
                         ++ show (sum $ map prefsL ws)
    print $ match ms ws
    where prefsL = length . pPref 
            
