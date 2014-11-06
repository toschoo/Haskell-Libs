--------------------------------------------------------------------------
-- Gale-Shapley Algorithm
--------------------------------------------------------------------------
module Algorithms.Marriage
where

  import qualified Data.Sequence as S
  import           Data.Sequence ((|>), (<|), Seq, ViewL(..))
  import           Data.List (delete, find, elemIndex)
  import           Data.Maybe (fromJust,fromMaybe)

  type Pid     = String
  data Partner = Partner {
                   pId     :: Pid,
                   pSpouse :: Maybe Pid,
                   pPref   :: [Pid]
                 }
  
  instance Eq Partner where
    a == b = pId a == pId b 

  instance Show Partner where
    show = show . pId

  type Partners = (Seq Partner, [Partner])

  match :: [Partner] -> [Partner] -> [(Pid,Pid)]
  match ms ws = go (S.fromList ms, ws)
    where go ps = case lonely ps of
                   (Nothing,_)   -> couples $ snd ps
                   (Just m, ps') -> go      $ propose m ps'

  couples :: [Partner] -> [(Pid,Pid)]
  couples = map (\w -> (fromJust $ pSpouse w, pId w))

  wanted :: Pid -> Seq Partner -> Maybe Partner
  wanted p ms = case S.viewl ms of
                  EmptyL -> Nothing
                  m :< s -> if p == pId m then Just m else wanted p s

  cherche :: Pid -> [Partner] -> Maybe Partner
  cherche p = find (\x -> p == pId x) 

  rplMan :: Partner -> Seq Partner -> Seq Partner
  rplMan m ms = case S.viewl ms of
                  EmptyL -> ms
                  x :< s -> if x == m then m <| s else x <| rplMan m s 

  rplWom :: Partner -> [Partner] -> [Partner]
  rplWom w ws = w : delete w ws

  lonely :: Partners -> (Maybe Partner, Partners)
  lonely (ms, ws) = let (r, rs) = wolf ms in (r, (rs, ws))
    where wolf xs = case S.viewl xs of
                      EmptyL -> (Nothing, xs)
                      x :< s -> case pSpouse x of
                                  Nothing -> (Just x, s |> x)
                                  _       -> let (r,rs) = wolf s in (r, x <| rs)

  -- pre
  propose :: Partner -> Partners -> Partners
  propose m (ms,ws) = go (pPref m)
    where go        [] = error "no more women!"
          go (wid:wis) = 
            case cherche wid ws of
              Nothing -> error $ "unknown woman: " ++ wid
              Just w  -> 
                case m `ask` w of
                  (False, _)   -> (rplMan m{pPref = wis} ms, ws)
                  (True, mOid) -> engage w wis mOid
          engage w wis mOid = 
            let ms' = rplMan m{pSpouse = Just $ pId w,
                               pPref   = wis} ms
                ws' = rplWom w{pSpouse = Just $ pId m} ws
             in case mOid of
                  Nothing  -> (ms', ws')
                  Just oid -> 
                    case wanted oid ms of
                      Nothing -> error $ "unknown man: " ++ oid
                      Just o  -> (rplMan o{pSpouse = Nothing} ms', ws')

  ask :: Partner -> Partner -> (Bool, Maybe Pid)
  ask m w = case pSpouse w of
              Nothing  -> (True,Nothing)
              Just oid -> if pref w oid > pref w (pId m) 
                            then (False,Nothing)
                            else (True,Just oid)
                        
  pref :: Partner -> Pid -> Int
  pref w oid = length (pPref w) - idx
    where idx = fromMaybe err $ elemIndex oid (pPref w)
          err = error $ "not in preferences of " ++ show w ++ ": " ++ oid
