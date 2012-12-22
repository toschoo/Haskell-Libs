module Main
where

  import System.Environment
  import System.FilePath
  import Control.Applicative ((<$>))

  import Text.Pandoc.Readers.LaTeX
  import Text.Pandoc.Writers.LaTeX
  import Text.Pandoc.Parsing
  import Text.Pandoc.Shared

  main :: IO ()
  main = do
    os <- getArgs
    case os of 
      [f] -> if takeExtension f /= ""
               then error "file name without extension, please" 
               else doTest f
      _   -> error "I need a file name"

  doTest :: FilePath -> IO ()
  doTest f = do
    s <- readFile (replaceExtension f "tex") 
    --  putStrLn s
    let p  = readLaTeX defaultParserState s -- {stateParseRaw=True} s
        s' = writeLaTeX defaultWriterOptions p
     in do putStrLn $ show p
           writeFile ("test.tex") s' 

