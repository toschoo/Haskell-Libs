{-# LANGUAGE BangPatterns #-}
module Main (main)
where

  import           Data.Monoid ((<>))
  import           System.IO.Unsafe
  import           Options.Applicative
  import           Control.Concurrent.MVar

  import KnightAndQueen

  main :: IO ()
  main = do
    initOpts
    c <- getGlobalComplete
    if c then mapM_ print knightTour -- complete knight tour
         else do                     -- one path between two squares
           q <- getGlobalQueen
           k <- getGlobalKnight
           t <- getGlobalTarget
           print $ funnyKnight q k t

  initOpts :: IO ()
  initOpts = do
    !o <- parseOpts
    putMVar mopts o

  ---------------------------------------------------------------------------
  -- Commandline Options
  ---------------------------------------------------------------------------
  data Opts = Opts {
    complete :: Bool,         -- complete tour?
    queen    :: Maybe String, -- queen  position
    knight   :: Maybe String, -- knight position
    target   :: Maybe String  -- target position
  } deriving Show

  {-# NOINLINE mopts #-}
  mopts :: MVar Opts
  mopts = unsafePerformIO newEmptyMVar

  ---------------------------------------------------------------------------
  -- Get Global Variable (with default value)
  ---------------------------------------------------------------------------
  getOptionalGlobal :: (Opts -> Maybe a) -> a -> IO a
  getOptionalGlobal acc d = do
    o <- readMVar mopts
    case acc o of
      Nothing -> return d
      Just v  -> return v

  ---------------------------------------------------------------------------
  -- Get Global Queen, Knight and Target
  ---------------------------------------------------------------------------
  getGlobalQueen, getGlobalKnight, getGlobalTarget :: IO String
  getGlobalQueen  = getOptionalGlobal queen  "d5"
  getGlobalKnight = getOptionalGlobal knight "h8"
  getGlobalTarget = getOptionalGlobal target "f8"

  ---------------------------------------------------------------------------
  -- Get Global complete
  ---------------------------------------------------------------------------
  getGlobalComplete :: IO Bool
  getGlobalComplete = complete <$> readMVar mopts

  ---------------------------------------------------------------------------
  -- Commandline Parser
  ---------------------------------------------------------------------------
  optsParser :: Parser Opts
  optsParser = Opts
    <$> switch
        (long "complete" <> short 'c' <> help "complete knight tour with queen on d5")
    <*> optional (strOption
        (long "queen"  <> short 'q' <> showDefault <> value "d5" <> help "queen position"))
    <*> optional (strOption
        (long "knight" <> short 'k' <> showDefault <> value "h8" <> help "initial knight position"))
    <*> optional (strOption
        (long "target" <> short 't' <> showDefault <> value "f8" <> help "target square"))

  parseOpts :: IO Opts
  parseOpts = execParser opts
    where opts = info (helper <*> optsParser) (
                   fullDesc
                   <> progDesc "perform knight tour"
                   <> header "find knight routes with Dijkstra")


