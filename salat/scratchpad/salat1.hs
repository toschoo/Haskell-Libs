module Main
where

  import           Data.Text
  import           Data.Char
  import           Parser -- Text.LaTeX.Base.Parser
  import           Text.LaTeX.Base.Render

  doc :: String
  doc = "\\documentclass{scrartcl}\n" ++
        "\\usepackage[utf8]{inputenc}\n" ++
        "\\usepackage[T1]{fontenc}\n" ++
        "\\usepackage[ngerman]{babel}\n\n" ++
        "% Start document:\n" ++
        "\\begin{document}\n" ++
        "\\newcommand{\\testcmd} % a new command\n" ++
        "{\\bfseries Hello world}\n" ++
        "\\begin{center}\n" ++
        "Headline\\\\*[24.5pt]\n" ++
        "\\begin{flushleft}\n" ++
        "\\begin{math}\n" ++
        "e = mc^2\n" ++
        "\\end{math}\n" ++
        "Text 1\n" ++
        "Text 2\\\\[24pt]\n" ++
        "Text 3: $e = mc^2$\n" ++
        "\\begin{center}\n" ++
        "Footline\n" ++
        "\\end{center}\n" ++
        "\\end{flushleft}\n" ++
        "\\end{center}\n" ++
        "\\end{document}" 

  -- main = getContents >>= putStrLn -- . tex2rtf
  -- main = putStrLn $ tex2tex doc
  main = do
    -- putStrLn doc
    putStrLn $ show (latexAtOnce $ pack doc)
