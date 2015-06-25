{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Canvase.Scenes.Rooms
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Room scenes.
--
-----------------------------------------------------------------------------
module Canvase.Scenes.Rooms where

import Canvase.Types
import Canvase.Color

import qualified Data.Map as M


rooms0 :: Scene
rooms0 = Scene $ M.fromList $
         (map (, moccasin)
          ([ Position x y z | x <- [-5], y <- [-1..1], z <- [1..8] ]) ++
          map (, yellow)
          ([ Position x y z | x <- [5], y <- [-1..1], z <- [1..8] ]) ++
          map (, indianred)
          ([ Position x y z | x <- [-5..5], y <- [-1..1], z <- [9] ])
         )

rooms1 :: Scene
rooms1 = Scene $ M.fromList $
         (map (, moccasin)
          ([ Position x y z | x <- [1..1], y <- [0], z <- [3] ]) ++
          map (, indianred)
          ([ Position x y z | x <- [2..2], y <- [0], z <- [3] ])
         )

rooms2 :: IO Scene
rooms2 = do
  m <- readFile "rooms2"
  return $ Scene $ M.fromList $ fromMap 1 (-4) $ parseMap m

rooms3 :: IO Scene
rooms3 = do
  m <- readFile "rooms3"
  return $ Scene $ M.fromList $ fromMap 8 (-11) $ parseMap m

rooms4 :: IO Scene
rooms4 = do
  m <- readFile "rooms4"
  return $ Scene $ M.fromList $ fromMap 1 (-4) $ parseMap m

data Field = Player
           | Color Color
           | Empty
           deriving (Show)

fromMap :: Integer -> Integer -> [[Field]] -> [(Position Integer, Color)]
fromMap bottomY topY m = withFloor $
            concatMap (\(t, m') ->
                        concatMap (\(u, f) -> case f of
                                      Color color -> [ (Position (u - ul `div` 2) y (tl - t), color)
                                                     | y <- [topY + 1..bottomY - 1] ]
                                      _ -> [])
                        $ zip [0..] m') $ zip [0..] m
  where tl = fromIntegral $ length m
        ul = fromIntegral $ maximum $ map length m
        withFloor ss = [ (Position x bottomY z, royalblue) | (x, z) <- ps ]
                       ++ [ (Position x topY z, springgreen) | (x, z) <- ps ]
                       ++ ss
        ps = concatMap (\(t, x) -> (map (\(u, _) -> (u - ul `div` 2, tl - t)) $ zip [0..] x)) $ zip [0..] m

parseMap :: String -> [[Field]]
parseMap = map (map parseField) . lines

parseField :: Char -> Field
parseField c = case c of
  'x' -> Player
  'r' -> Color red
  'g' -> Color limegreen
  'b' -> Color blue
  'm' -> Color magenta
  'c' -> Color cyan
  'o' -> Color orange
  'p' -> Color pink
  _ -> Empty
