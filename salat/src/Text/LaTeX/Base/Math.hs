module Text.LaTeX.Base.Math
where

  import           Text.TeXMath.Types
  import           Data.Monoid
  import           Data.Text(Text(..), snoc, cons)
  import qualified Data.Text as T hiding (Text)
  import           Data.List (foldl', lookup)

  printMath :: [Exp] -> Text
  printMath = mconcat . map printExp

  printExp :: Exp -> Text
  printExp (ENumber n) = T.pack n
  printExp (EGrouped xs) = '{'          `cons`
                           printMath xs `snoc` 
                           '}'
  printExp (EDelimited s e xs) = T.pack ("\\left" ++ s ++ " ") <> 
                                 printMath xs                  <> 
                                 T.pack ("\\right" ++ e) 

  printExp exp@(EIdentifier i) = T.pack (sym exp i) `snoc` ' '
  printExp exp@(EMathOperator o) = T.pack $ sym exp o
  printExp exp@(ESymbol t s) = case t of
                                 Accent -> T.pack "accent"
                                 _      -> T.pack (sym exp s) `snoc` ' '
  printExp (ESpace s) = T.pack " "
  printExp (EBinary o x y) = T.pack o   <>
                             printExp x <>
                             printExp y 
  printExp (ESub x y) = printExp x  <>
                        T.pack "_{" <>
                        printExp y  <>
                        T.pack "}"
  printExp (ESuper x y) = printExp x  <>
                          T.pack "^{" <>
                          printExp y  <>
                          T.pack "}"

  printExp (ESubsup x y z) = undefined

  printExp (EOver x y) = T.pack "\\limits^{"   <> 
                         printExp x `snoc` '}' <> 
                         printExp y 
                         
  
  printExp (EUnder x y) = T.pack "\\limits_{"   <> 
                          printExp x `snoc` '}' <> 
                          printExp y 

  printExp (EUnderover x y z) = T.pack "EUnderOver"
  printExp (EUp x y) = T.pack "EUp"
  printExp (EDown x y) = T.pack "EDown"
  printExp (EDownup x y z) = T.pack "EDownup"
  printExp (EUnary o x) = T.pack o <> printExp x
  printExp (EScaled o x) = T.pack "EScaled"
  printExp (EStretchy x) = T.pack "EStretchy"

  printExp (EArray as al) = 
    let a = map align as
        l = map line  al
     in T.pack a <> (mconcat . mconcat) l
    where align x = case x of
                      AlignLeft    -> 'l'
                      AlignRight   -> 'r'
                      AlignCenter  -> 'c'
                      AlignDefault -> ' '
          line  x = map printMath x

  printExp (EText typ t) = case typ of
                             TextNormal -> T.pack t

  sym :: Exp -> String -> String 
  sym x d = case lookup x symbols of
              Just s  -> s
              Nothing -> d

  swap :: [(k,v)] -> [(v,k)]
  swap = foldl' (\n (k,v) -> (v,k) : n) []

  scalers :: [(String, String)]
  scalers = 
    swap  [ ("\\bigg", "2.2")
          , ("\\Bigg", "2.9")
          , ("\\big", "1.2")
          , ("\\Big", "1.6")
          , ("\\biggr", "2.2")
          , ("\\Biggr", "2.9")
          , ("\\bigr", "1.2")
          , ("\\Bigr", "1.6")
          , ("\\biggl", "2.2")
          , ("\\Biggl", "2.9")
          , ("\\bigl", "1.2")
          , ("\\Bigl", "1.6")
          ]

  enclosures :: [(Exp, String)]
  enclosures = swap [ ("(", ESymbol Open "(")
             , (")", ESymbol Close ")")
             , ("[", ESymbol Open "[")
             , ("]", ESymbol Close "]")
             , ("\\{", ESymbol Open "{")
             , ("\\}", ESymbol Close "}")
             , ("\\lbrack", ESymbol Open "[")
             , ("\\lbrace", ESymbol Open "{")
             , ("\\rbrack", ESymbol Close "]")
             , ("\\rbrace", ESymbol Close "}")
             , ("\\llbracket", ESymbol Open "\x27E6")
             , ("\\rrbracket", ESymbol Close "\x27E7")
             , ("\\langle", ESymbol Open "\x27E8")
             , ("\\rangle", ESymbol Close "\x27E9")
             , ("\\lfloor", ESymbol Open "\x230A")
             , ("\\rfloor", ESymbol Close "\x230B")
             , ("\\lceil", ESymbol Open "\x2308")
             , ("\\rceil", ESymbol Close "\x2309")
             , ("|", ESymbol Open "\x2223")
             , ("|", ESymbol Close "\x2223")
             , ("\\|", ESymbol Open "\x2225")
             , ("\\|", ESymbol Close "\x2225")
             , ("\\lvert", ESymbol Open "\x7C")
             , ("\\rvert", ESymbol Close "\x7C")
             , ("\\vert", ESymbol Close "\x7C")
             , ("\\lVert", ESymbol Open "\x2225")
             , ("\\rVert", ESymbol Close "\x2225")
             , ("\\Vert", ESymbol Close "\x2016")
             , ("\\ulcorner", ESymbol Open "\x231C")
             , ("\\urcorner", ESymbol Close "\x231D")
             ]

  symbols :: [(Exp, String)]
  symbols = swap [
             ("+", ESymbol Bin "+")
           , ("-", ESymbol Bin "\x2212")
           , ("*", ESymbol Bin "*")
           , (",", ESymbol Pun ",")
           , (".", ESymbol Pun ".")
           , (";", ESymbol Pun ";")
           , (":", ESymbol Pun ":")
           , ("?", ESymbol Pun "?")
           , (">", ESymbol Rel ">")
           , ("<", ESymbol Rel "<")
           , ("!", ESymbol Ord "!")
           , ("'", ESymbol Ord "\x02B9")
           , ("''", ESymbol Ord "\x02BA")
           , ("'''", ESymbol Ord "\x2034")
           , ("''''", ESymbol Ord "\x2057")
           , ("=", ESymbol Rel "=")
           , (":=", ESymbol Rel ":=")
           , ("\\mid", ESymbol Bin "\x2223")
           , ("\\parallel", ESymbol Rel "\x2225")
           , ("\\backslash", ESymbol Bin "\x2216")
           , ("/", ESymbol Bin "/")
           , ("\\setminus",	ESymbol Bin "\\")
           , ("\\times", ESymbol Bin "\x00D7")
           , ("\\alpha", EIdentifier "\x03B1")
           , ("\\beta", EIdentifier "\x03B2")
           , ("\\chi", EIdentifier "\x03C7")
           , ("\\delta", EIdentifier "\x03B4")
           , ("\\Delta", ESymbol Op "\x0394")
           , ("\\epsilon", EIdentifier "\x03B5")
           , ("\\varepsilon", EIdentifier "\x025B")
           , ("\\eta", EIdentifier "\x03B7")
           , ("\\gamma", EIdentifier "\x03B3")
           , ("\\Gamma", ESymbol Op "\x0393")
           , ("\\iota", EIdentifier "\x03B9")
           , ("\\kappa", EIdentifier "\x03BA")
           , ("\\lambda", EIdentifier "\x03BB")
           , ("\\Lambda", ESymbol Op "\x039B")
           , ("\\mu", EIdentifier "\x03BC")
           , ("\\nu", EIdentifier "\x03BD")
           , ("\\omega", EIdentifier "\x03C9")
           , ("\\Omega", ESymbol Op "\x03A9")
           , ("\\phi", EIdentifier "\x03D5")
           , ("\\varphi", EIdentifier "\x03C6")
           , ("\\Phi", ESymbol Op "\x03A6")
           , ("\\pi", EIdentifier "\x03C0")
           , ("\\Pi", ESymbol Op "\x03A0")
           , ("\\psi", EIdentifier "\x03C8")
           , ("\\Psi", ESymbol Ord "\x03A8")
           , ("\\rho", EIdentifier "\x03C1")
           , ("\\sigma", EIdentifier "\x03C3")
           , ("\\Sigma", ESymbol Op "\x03A3")
           , ("\\tau", EIdentifier "\x03C4")
           , ("\\theta", EIdentifier "\x03B8")
           , ("\\vartheta", EIdentifier "\x03D1")
           , ("\\Theta", ESymbol Op "\x0398")
           , ("\\upsilon", EIdentifier "\x03C5")
           , ("\\xi", EIdentifier "\x03BE")
           , ("\\Xi", ESymbol Op "\x039E")
           , ("\\zeta", EIdentifier "\x03B6")
           , ("\\frac12", ESymbol Ord "\x00BD")
           , ("\\frac14", ESymbol Ord "\x00BC")
           , ("\\frac34", ESymbol Ord "\x00BE")
           , ("\\frac13", ESymbol Ord "\x2153")
           , ("\\frac23", ESymbol Ord "\x2154")
           , ("\\frac15", ESymbol Ord "\x2155")
           , ("\\frac25", ESymbol Ord "\x2156")
           , ("\\frac35", ESymbol Ord "\x2157")
           , ("\\frac45", ESymbol Ord "\x2158")
           , ("\\frac16", ESymbol Ord "\x2159")
           , ("\\frac56", ESymbol Ord "\x215A")
           , ("\\frac18", ESymbol Ord "\x215B")
           , ("\\frac38", ESymbol Ord "\x215C")
           , ("\\frac58", ESymbol Ord "\x215D")
           , ("\\frac78", ESymbol Ord "\x215E")
           , ("\\pm", ESymbol Bin "\x00B1")
           , ("\\mp", ESymbol Bin "\x2213")
           , ("\\triangleleft", ESymbol Bin "\x22B2")
           , ("\\triangleright", ESymbol Bin "\x22B3")
           , ("\\cdot", ESymbol Bin "\x22C5")
           , ("\\star", ESymbol Bin "\x22C6")
           , ("\\ast", ESymbol Bin "\x002A")
           , ("\\times", ESymbol Bin "\x00D7")
           , ("\\div", ESymbol Bin "\x00F7")
           , ("\\circ", ESymbol Bin "\x2218")
           , ("\\bullet", ESymbol Bin "\x2022")
           , ("\\oplus", ESymbol Bin "\x2295")
           , ("\\ominus", ESymbol Bin "\x2296")
           , ("\\otimes", ESymbol Bin "\x2297")
           , ("\\bigcirc", ESymbol Bin "\x25CB")
           , ("\\oslash", ESymbol Bin "\x2298")
           , ("\\odot", ESymbol Bin "\x2299")
           , ("\\land", ESymbol Bin "\x2227")
           , ("\\wedge", ESymbol Bin "\x2227")
           , ("\\lor", ESymbol Bin "\x2228")
           , ("\\vee", ESymbol Bin "\x2228")
           , ("\\cap", ESymbol Bin "\x2229")
           , ("\\cup", ESymbol Bin "\x222A")
           , ("\\sqcap", ESymbol Bin "\x2293")
           , ("\\sqcup", ESymbol Bin "\x2294")
           , ("\\uplus", ESymbol Bin "\x228E")
           , ("\\amalg", ESymbol Bin "\x2210")
           , ("\\bigtriangleup", ESymbol Bin "\x25B3")
           , ("\\bigtriangledown", ESymbol Bin "\x25BD")
           , ("\\dag", ESymbol Bin "\x2020")
           , ("\\dagger", ESymbol Bin "\x2020")
           , ("\\ddag", ESymbol Bin "\x2021")
           , ("\\ddagger", ESymbol Bin "\x2021")
           , ("\\lhd", ESymbol Bin "\x22B2")
           , ("\\rhd", ESymbol Bin "\x22B3")
           , ("\\unlhd", ESymbol Bin "\x22B4")
           , ("\\unrhd", ESymbol Bin "\x22B5")
           , ("\\lt", ESymbol Rel "<")
           , ("\\gt", ESymbol Rel ">")
           , ("\\ne", ESymbol Rel "\x2260")
           , ("\\neq", ESymbol Rel "\x2260")
           , ("\\le", ESymbol Rel "\x2264")
           , ("\\leq", ESymbol Rel "\x2264")
           , ("\\leqslant", ESymbol Rel "\x2264")
           , ("\\ge", ESymbol Rel "\x2265")
           , ("\\geq", ESymbol Rel "\x2265")
           , ("\\geqslant", ESymbol Rel "\x2265")
           , ("\\equiv", ESymbol Rel "\x2261")
           , ("\\ll", ESymbol Rel "\x226A")
           , ("\\gg", ESymbol Rel "\x226B")
           , ("\\doteq", ESymbol Rel "\x2250")
           , ("\\prec", ESymbol Rel "\x227A")
           , ("\\succ", ESymbol Rel "\x227B")
           , ("\\preceq", ESymbol Rel "\x227C")
           , ("\\succeq", ESymbol Rel "\x227D")
           , ("\\subset", ESymbol Rel "\x2282")
           , ("\\supset", ESymbol Rel "\x2283")
           , ("\\subseteq", ESymbol Rel "\x2286")
           , ("\\supseteq", ESymbol Rel "\x2287")
           , ("\\sqsubset", ESymbol Rel "\x228F")
           , ("\\sqsupset", ESymbol Rel "\x2290")
           , ("\\sqsubseteq", ESymbol Rel "\x2291")
           , ("\\sqsupseteq", ESymbol Rel "\x2292")
           , ("\\sim", ESymbol Rel "\x223C")
           , ("\\simeq", ESymbol Rel "\x2243")
           , ("\\approx", ESymbol Rel "\x2248")
           , ("\\cong", ESymbol Rel "\x2245")
           , ("\\Join", ESymbol Rel "\x22C8")
           , ("\\bowtie", ESymbol Rel "\x22C8")
           , ("\\in", ESymbol Rel "\x2208")
           , ("\\ni", ESymbol Rel "\x220B")
           , ("\\owns", ESymbol Rel "\x220B")
           , ("\\propto", ESymbol Rel "\x221D")
           , ("\\vdash", ESymbol Rel "\x22A2")
           , ("\\dashv", ESymbol Rel "\x22A3")
           , ("\\models", ESymbol Rel "\x22A8")
           , ("\\perp", ESymbol Rel "\x22A5")
           , ("\\smile", ESymbol Rel "\x2323")
           , ("\\frown", ESymbol Rel "\x2322")
           , ("\\asymp", ESymbol Rel "\x224D")
           , ("\\notin", ESymbol Rel "\x2209")
           , ("\\gets", ESymbol Rel "\x2190")
           , ("\\leftarrow", ESymbol Rel "\x2190")
           , ("\\to", ESymbol Rel "\x2192")
           , ("\\rightarrow", ESymbol Rel "\x2192")
           , ("\\leftrightarrow", ESymbol Rel "\x2194")
           , ("\\uparrow", ESymbol Rel "\x2191")
           , ("\\downarrow", ESymbol Rel "\x2193")
           , ("\\updownarrow", ESymbol Rel "\x2195")
           , ("\\Leftarrow", ESymbol Rel "\x21D0")
           , ("\\Rightarrow", ESymbol Rel "\x21D2")
           , ("\\Leftrightarrow", ESymbol Rel "\x21D4")
           , ("\\iff", ESymbol Rel "\x21D4")
           , ("\\Uparrow", ESymbol Rel "\x21D1")
           , ("\\Downarrow", ESymbol Rel "\x21D3")
           , ("\\Updownarrow", ESymbol Rel "\x21D5")
           , ("\\mapsto", ESymbol Rel "\x21A6")
           , ("\\longleftarrow", ESymbol Rel "\x2190")
           , ("\\longrightarrow", ESymbol Rel "\x2192")
           , ("\\longleftrightarrow", ESymbol Rel "\x2194")
           , ("\\Longleftarrow", ESymbol Rel "\x21D0")
           , ("\\Longrightarrow", ESymbol Rel "\x21D2")
           , ("\\Longleftrightarrow", ESymbol Rel "\x21D4")
           , ("\\longmapsto", ESymbol Rel "\x21A6")
           , ("\\sum", ESymbol Op "\x2211")
           , ("\\prod", ESymbol Op "\x220F")
           , ("\\bigcap", ESymbol Op "\x22C2")
           , ("\\bigcup", ESymbol Op "\x22C3")
           , ("\\bigwedge", ESymbol Op "\x22C0")
           , ("\\bigvee", ESymbol Op "\x22C1")
           , ("\\bigsqcap", ESymbol Op "\x2A05")
           , ("\\bigsqcup", ESymbol Op "\x2A06")
           , ("\\coprod", ESymbol Op "\x2210")
           , ("\\bigoplus", ESymbol Op "\x2A01")
           , ("\\bigotimes", ESymbol Op "\x2A02")
           , ("\\bigodot", ESymbol Op "\x2A00")
           , ("\\biguplus", ESymbol Op "\x2A04")
           , ("\\int", ESymbol Op "\x222B")
           , ("\\iint", ESymbol Op "\x222C")
           , ("\\iiint", ESymbol Op "\x222D")
           , ("\\oint", ESymbol Op "\x222E")
           , ("\\prime", ESymbol Ord "\x2032")
           , ("\\dots", ESymbol Ord "\x2026")
           , ("\\ldots", ESymbol Ord "\x2026")
           , ("\\cdots", ESymbol Ord "\x22EF")
           , ("\\vdots", ESymbol Ord "\x22EE")
           , ("\\ddots", ESymbol Ord "\x22F1")
           , ("\\forall", ESymbol Op "\x2200")
           , ("\\exists", ESymbol Op "\x2203")
           , ("\\Re", ESymbol Ord "\x211C")
           , ("\\Im", ESymbol Ord "\x2111")
           , ("\\aleph", ESymbol Ord "\x2135")
           , ("\\hbar", ESymbol Ord "\x210F")
           , ("\\ell", ESymbol Ord "\x2113")
           , ("\\wp", ESymbol Ord "\x2118")
           , ("\\emptyset", ESymbol Ord "\x2205")
           , ("\\infty", ESymbol Ord "\x221E")
           , ("\\partial", ESymbol Ord "\x2202")
           , ("\\nabla", ESymbol Ord "\x2207")
           , ("\\triangle", ESymbol Ord "\x25B3")
           , ("\\therefore", ESymbol Pun "\x2234")
           , ("\\angle", ESymbol Ord "\x2220")
           , ("\\diamond", ESymbol Op "\x22C4")
           , ("\\Diamond", ESymbol Op "\x25C7")
           , ("\\lozenge", ESymbol Op "\x25CA")
           , ("\\neg", ESymbol Op "\x00AC")
           , ("\\lnot", ESymbol Ord "\x00AC")
           , ("\\bot", ESymbol Ord "\x22A5")
           , ("\\top", ESymbol Ord "\x22A4")
           , ("\\square", ESymbol Ord "\x25AB")
           , ("\\Box", ESymbol Op "\x25A1")
           , ("\\wr", ESymbol Ord "\x2240")
           , ("\\!", ESpace "-0.167em")
           , ("\\,", ESpace "0.167em")
           , ("\\>", ESpace "0.222em")
           , ("\\:", ESpace "0.222em")
           , ("\\;", ESpace "0.278em")
           , ("~", ESpace "0.333em")
           , ("\\quad", ESpace "1em")
           , ("\\qquad", ESpace "2em")
           , ("\\arccos", EMathOperator "arccos")
           , ("\\arcsin", EMathOperator "arcsin")
           , ("\\arctan", EMathOperator "arctan")
           , ("\\arg", EMathOperator "arg")
           , ("\\cos", EMathOperator "cos")
           , ("\\cosh", EMathOperator "cosh")
           , ("\\cot", EMathOperator "cot")
           , ("\\coth", EMathOperator "coth")
           , ("\\csc", EMathOperator "csc")
           , ("\\deg", EMathOperator "deg")
           , ("\\det", EMathOperator "det")
           , ("\\dim", EMathOperator "dim")
           , ("\\exp", EMathOperator "exp")
           , ("\\gcd", EMathOperator "gcd")
           , ("\\hom", EMathOperator "hom")
           , ("\\inf", EMathOperator "inf")
           , ("\\ker", EMathOperator "ker")
           , ("\\lg", EMathOperator "lg")
           , ("\\lim", EMathOperator "lim")
           , ("\\liminf", EMathOperator "liminf")
           , ("\\limsup", EMathOperator "limsup")
           , ("\\ln", EMathOperator "ln")
           , ("\\log", EMathOperator "log")
           , ("\\max", EMathOperator "max")
           , ("\\min", EMathOperator "min")
           , ("\\Pr", EMathOperator "Pr")
           , ("\\sec", EMathOperator "sec")
           , ("\\sin", EMathOperator "sin")
           , ("\\sinh", EMathOperator "sinh")
           , ("\\sup", EMathOperator "sup")
           , ("\\tan", EMathOperator "tan")
           , ("\\tanh", EMathOperator "tanh")
           ]

