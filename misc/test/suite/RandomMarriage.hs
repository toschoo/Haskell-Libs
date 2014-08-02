module RandomMarriage
where

  import qualified Data.Vector.Mutable as V
  import           Data.Vector.Mutable (IOVector)
  import           System.Random (randomRIO)
  import           Control.Monad (when)
  import           Control.Applicative ((<$>))

  import           Marriage

  type Permutation = IOVector String

  permute :: Permutation -> Int -> IO ()
  permute v size = go 0
    where go :: Int -> IO ()
          go i | i == size = return ()
               | otherwise = do x <- randomRIO (0,size-1)
                                when (i /= x) $ V.unsafeSwap v i x
                                go (i+1)

  newPerm :: [String] -> IO Permutation
  newPerm s = let l = length s 
               in do v <- V.new l; iniP s 0 v; return v
    where iniP [] _ _     = return ()
          iniP (x:xs) i v = V.unsafeWrite v i x >> iniP xs (i+1) v

  killPerm :: Permutation -> IO ()
  killPerm = V.clear

  printPerm :: Permutation -> Int -> IO ()
  printPerm v size = go 0
    where go :: Int -> IO ()
          go i | i == size = putStrLn ""
               | otherwise = V.unsafeRead v i >>= putStr >> 
                                                  putStr " " >> go (i+1)

  arbMarriage :: Int -> IO ([Partner], [Partner])
  arbMarriage n = let ps = [1..n]
                      ms = makeMen ps
                      ws = makeWom ps
                   in do vm <- newPerm ms
                         vw <- newPerm ws
                         mp <- addPerms vw n ms 
                         wp <- addPerms vm n ws
                         return (mp, wp)

  addPerms :: Permutation -> Int -> [Pid] -> IO [Partner]
  addPerms v size = mapM pid2P 
    where pid2P x = do permute v size
                       prfs <- toList 0
                       return $ Partner x Nothing prfs
          toList i | i == size = return []
                   | otherwise = do x <- V.unsafeRead v i
                                    (x:) <$> toList (i+1)
                         
  makeMen, makeWom :: [Int] -> [Pid]
  makeMen = makeP 'm'
  makeWom = makeP 'w'

  makeP :: Char -> [Int] -> [Pid]
  makeP c = map ((c:) . show) 

  
  
