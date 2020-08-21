-----------------------------------------------------------------------------
-- |
-- Module      :  Canvase.Renderer
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- The renderer.
--
-----------------------------------------------------------------------------
module Canvase.Renderer where

import Prelude hiding (Left, Right)
import Canvase.CanvaseM
import Control.Monad.State
import Canvase.Utils
import Canvase.Types
import Canvase.SDL
import Canvase.Color
import Canvase.Math
import Data.Functor
import Data.Word
import qualified Data.List as L
import qualified Data.Map as M
import qualified Graphics.UI.SDL as SDL


perspectiveRender :: CanvaseM ()
perspectiveRender = do
  scene <- getScene
  polys <- scenePolygons scene
  liftIO $ print $ length polys
  mapM_ drawPolygon polys

type Polygon = [(Double, Double)]
data FilledPolygon = FilledPolygon [(Double, Double)] Color
                   deriving (Show)

scenePolygons :: Scene -> CanvaseM [FilledPolygon]
scenePolygons (Scene scene) = do
  screen <- getScreen
  (w, h) <- liftIO $ surfSize screen
  Position x0 y0 z0 <- getPosition
  ya <- getAngle Y
  xa <- getAngle X
  let getBox pos0
        = let pos1 = rotate Y ya $ rotate X xa (fromIntegral <$> pos0)
              pos2 = round <$> (Position (pX pos1 + x0) (pY pos1 + y0) (pZ pos1 + z0))
          in (pos2, M.lookup pos2 scene)

  let zDist = 20
      poss :: [Position Integer]
      poss = [ Position x y z
             | z <- [zDist, zDist-1..0],
               y <- nrange z ++ mnrange z ++ [0],
               x <- nrange z ++ mnrange z ++ [0]
             ]
  let filt ((_, Nothing), _) = []
      filt ((vp, Just c), p) = [(c, p, vp)]
      cubes = map (\(c, p) -> (light (1 / (1 + (fromIntegral (pZ p))**0.5)) c, p))
              $ map (\(c, p, _) -> (c, p))
--              $ nubBy (\(_, _, vp0) (_, _, vp1) -> vp0 == vp1)
              $ concatMap filt $ zip (map getBox poss) poss

  return $ concatMap cubePoly cubes

nrange z = [(-z - 1)..(-1)]
mnrange z = [z + 1, z..1]

data Direction2D = Up | Right | Down | Left
                 deriving (Show, Read, Eq, Ord)

instance Enum Direction2D where
  succ Up    = Right
  succ Right = Down
  succ Down  = Left
  succ Left  = Up

  toEnum 0 = Up
  toEnum 1 = Right
  toEnum 2 = Down
  toEnum 3 = Left
  toEnum n = toEnum $ n `mod` 4

  fromEnum Up    = 0
  fromEnum Right = 1
  fromEnum Down  = 2
  fromEnum Left  = 3

cubePoly :: (Color, Position Integer) -> [FilledPolygon]
cubePoly (c, Position xI yI zI)
  = map (\p -> FilledPolygon p c)
    $ polys x Left ++ polys y Up
    ++ [squarePoly (top z x) (top z y) (size z) (size z)]
  where (x, y, z) = (fromIntegral xI, fromIntegral yI, fromIntegral zI)

        size :: Int -> Double
        size z = 1 / fromIntegral (z * 2 + 1)

        top :: Int -> Int -> Double
        top z rel = fromIntegral (z + rel) * size z

        bottom :: Int -> Int -> Double
        bottom z rel = top z rel + size z

        fitsTop :: Int -> Bool
        fitsTop rel = top z rel > top (z + 1) rel

        fitsBottom :: Int -> Bool
        fitsBottom rel = bottom z rel < bottom (z + 1) rel

        polys :: Int -> Direction2D -> [Polygon]
        polys rel d | fitsTop rel == fitsBottom rel = []
                    | fitsTop rel = [trapezoidShape d]
                    | fitsBottom rel = [trapezoidShape $ succ $ succ d]

        trapezoidShape :: Direction2D -> Polygon
        trapezoidShape dir = shape' $ case dir of
          Up -> (top, top, bottom, top)
          Right -> (bottom, top, bottom, bottom)
          Down -> (top, bottom, bottom, bottom)
          Left -> (top, top, top, bottom)
          where shape' (a, b, c, d) = [ (a z x, b z y)
                                      , (a (z + 1) x, b (z + 1) y)
                                      , (c (z + 1) x, d (z + 1) y)
                                      , (c z x, d z y)
                                      ]

squarePoly :: Double -> Double -> Double -> Double -> Polygon
squarePoly x y w h = [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

drawPolygon :: FilledPolygon -> CanvaseM ()
drawPolygon (FilledPolygon points color) = do
  screen <- getScreen
  (w, h) <- liftIO $ surfSize screen
  liftIO $ filledPolygon (map (stretch w h) points) color screen

stretch :: Int -> Int -> (Double, Double) -> (Int, Int)
stretch w h (x, y) = (round (x * fromIntegral w), round (y * fromIntegral h))
