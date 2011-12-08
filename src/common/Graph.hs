module Graph 
where

  type GPoint = (Int, Int)
  type GLine  = (GNode, GNode)
  data GNode  = GNode {
                  gTxt   :: String,
                  gPoint :: GPoint  -- joke repetition penalty!
                }

  startPoint :: GPoint
  startPoint = (600, 30)

  startLength :: Int
  startLength = 300

  descfactor :: Int
  descfactor =  5

  textVerTolerance :: Int
  textVerTolerance = 4

  textHorTolerance :: Int
  textHorTolerance = 10

  nil :: String
  nil = "NIL"

  getX :: GNode -> Int
  getX = fst . gPoint

  getY :: GNode -> Int
  getY = snd . gPoint



