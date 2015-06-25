-----------------------------------------------------------------------------
-- |
-- Module      :  Canvase.Scenes
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- All scenes.
--
-----------------------------------------------------------------------------
module Canvase.Scenes where

import Canvase.Types
import Canvase.Color
import Canvase.Scenes.Rooms

import qualified Data.Map as M
import System.Random
import Control.Monad


canvaseScenes :: [(String, IO Scene)]
canvaseScenes = [ ("rooms0", return rooms0)
                , ("rooms1", return rooms1)
                , ("rooms2", rooms2)
                , ("rooms3", rooms3)
                , ("race0", return race0)
                , ("random0", random0)
                , ("random1", random1)
                ]

race0 :: Scene
race0 = Scene $ M.fromList
        [ (Position x 2 z, if z `mod` 2 == 0 then darkgrey else lightgrey)
        | x <- [-10..10], z <- [0..100] ]

random0 :: IO Scene
random0 = do
  let poss = [ Position x 2 z
             | x <- [-50..50], z <- [-50..50] ]
  s <- mapM gen poss
  return $ Scene $ M.fromList s
  where gen p = do
          r <- randomRIO (0.0, 1.0)
          g <- randomRIO (0.0, 1.0)
          b <- randomRIO (0.0, 1.0)
          return (p, RGBA r g b 1)

random1 :: IO Scene
random1 = do
  s <- replicateM 100000 gen
  return $ Scene $ M.fromList s
  where gen = do
          x <- randomRIO (-50, 50)
          y <- randomRIO (-50, 50)
          z <- randomRIO (-50, 50)
          r <- randomRIO (0.0, 1.0)
          g <- randomRIO (0.0, 1.0)
          b <- randomRIO (0.0, 1.0)
          return (Position x y z, RGBA r g b 1)
