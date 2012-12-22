module Main
where

  import Text.Pandoc
  import Prelude hiding (getContents, putStrLn)
  import System.IO.UTF8

  doc :: String
  doc = "\\documentclass{scrartcl}\n" ++
        "\\usepackage[utf8]{inputenc}\n" ++
        "\\usepackage[T1]{fontenc}\n" ++
        "\\usepackage[ngerman]{babel}\n\n" ++
        "\\begin{document}\n" ++
        "\\newcommand{\\testcmd}\n" ++
        "{\\bfseries Hello world}\n" ++
        "\\end{document}\n"

  os :: WriterOptions
  os = defaultWriterOptions

  tex2rtf :: String -> String
  tex2rtf = (writeRTF defaultWriterOptions) . 
                readLaTeX defaultParserState

  tex2tex :: String -> String
  tex2tex = (writeLaTeX os) . 
                readLaTeX defaultParserState

  -- main = getContents >>= putStrLn -- . tex2rtf
  -- main = putStrLn $ tex2tex doc
  main = putStrLn $ show (readLaTeX defaultParserState doc)
