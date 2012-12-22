module Visual
where

  import Graph
  import Graphics.SOE

  drawLines :: [GLine] -> IO()
  drawLines list = runGraphics $
    do w <- openWindow "Tree" (1200, 1200)
       drawLine list w
       _ <- getKey w
       closeWindow w

  drawLine :: [GLine] -> Window -> IO()
  drawLine [] _ = putStr "" 
  drawLine (n:ns) w = do
       let x1 = getX (fst n)
           y1 = getY (fst n)
           x2 = getX (snd n)
           y2 = getY (snd n) 
       drawInWindow w (text ((x1 - textHorTolerance), y1) (gTxt (fst n)))
       drawInWindow w (line (x1, (y1 + 3 * textVerTolerance)) 
                            (x2, (y2 - textVerTolerance)))
       drawInWindow w (text ((x2 - textHorTolerance), y2) (gTxt (snd n)))
       drawLine ns w

  ----------------------------------------------------------------------------
  -- Step by step sort, waiting for user input
  -- supports forward and backward navigation
  ----------------------------------------------------------------------------
{-
  data GContext a = GContext {tree :: Tree a,
                              list :: [a]}

  data GTurn = GTurn {cur    :: Int,
                      target :: Int}

  visualSort :: (Show a, Ord a) => [a] -> IO()
  visualSort [] = return ()
  visualSort l = runGraphics $
    do w <- openWindow "Tree" (1200, 1200)
       gStartSorting w l
       closeWindow w

  gStartSorting :: (Show a, Ord a) => Window -> [a] -> IO ()
  gStartSorting _ [] = return ()
  gStartSorting w (x:xs) = do
    let t = newTree x
    gSort w t xs (GContext t xs) (GTurn 1 1)

  gSort :: (Show a, Ord a) => 
           Window -> Tree a -> [a] -> GContext a -> GTurn -> IO ()
  gSort w t [] ctx turn = do
    drawInWindow w (text (10, 10) (show (cur turn)))
    let l = tree2lines t
    drawLine l w
    gWaitForInput w t [] ctx turn True

  gSort w t (x:xs) ctx turn = do
    let l = tree2lines t
    let t' = insert x t
    if (cur turn) == (target turn)
      then do
        drawInWindow w (text (10, 10) (show (cur turn)))
        drawLine l w
        gWaitForInput w t' xs ctx GTurn{cur    = ((cur turn + 1)),
                                        target = ((target turn) + 1)}
                      False
      else do
        gSort w t' xs ctx GTurn{cur    = ((cur turn) + 1),
                                target = (target turn)} 

  gWaitForInput :: (Show a, Ord a) => 
                   Window -> Tree a -> [a] -> GContext a -> GTurn -> Bool -> IO ()
  gWaitForInput w t l ctx turn end = do
    k <- getKey w
    case k of
      'q' -> return ()
      '\8' -> do
        let minus = if end then 1 else 2
        if (cur turn) > minus -- we are one ahead 
          then do
            clearWindow w
            gSort w (tree ctx) (list ctx) 
                    ctx
                    GTurn{cur    = 1,
                          target = ((cur turn) - minus)} 
          else gWaitForInput w t l ctx turn end
      otherwise -> do
        clearWindow w
        gSort w t l ctx GTurn {cur    = (cur turn),
                               target = (cur turn)}

 -} 
