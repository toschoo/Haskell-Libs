module Main
where

  import Data.Text
  import Data.Char
  import Text.LaTeX.Base.Parser
  import Text.LaTeX.Base.Render

  doc :: String
  doc = "\\documentclass{scrartcl}\n" ++
        "\\usepackage[utf8]{inputenc}\n" ++
        "\\usepackage[T1]{fontenc}\n" ++
        "\\usepackage[ngerman]{babel}\n\n" ++
        "\\begin{document}\n" ++
        "\\newcommand{\\testcmd}\n" ++
        "{\\bfseries Hello world}\n" ++
        "\\end{document}" 

  -- main = getContents >>= putStrLn -- . tex2rtf
  -- main = putStrLn $ tex2tex doc
  main = do
    putStrLn doc
    putStrLn $ show (parseLaTeX $ pack doc)
