-----------------------------------------------------------------------------
-- |
-- Module      :  Canvase.SDL
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- SDL wrapper functions.
--
-----------------------------------------------------------------------------
module Canvase.SDL where

import Canvase.Types
import Canvase.Color

import Control.Monad
import Control.Monad.State
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
import qualified Graphics.UI.SDL.Rotozoomer as SDL


surfSize :: SDL.Surface -> IO (Int, Int)
surfSize surf = do
  rect <- SDL.getClipRect surf
  return (SDL.rectW rect, SDL.rectH rect)

i n = fromIntegral n

fillRect r c s = SDL.fillRect s r $ colorToPixel c

pixel x y c s = SDL.pixel s (i x) (i y) $ colorToPixel $ primConvert c
hLine x1 x2 y c s = SDL.hLine s (i x1) (i x2) (i y) $ colorToPixel $ primConvert c
vLine x y1 y2 c s = SDL.vLine s (i x) (i y1) (i y2) $ colorToPixel $ primConvert c
rectangle r c s = SDL.rectangle s r $ colorToPixel $ primConvert c
box r c s = SDL.box s r $ colorToPixel $ primConvert c
line x y x' y' c s = SDL.line s (i x) (i y) (i x') (i y') $ colorToPixel $ primConvert c
aaLine x y x' y' c s = SDL.aaLine s (i x) (i y) (i x') (i y') $ colorToPixel $ primConvert c
circle x y r c s = SDL.circle s (i x) (i y) (i r) $ colorToPixel $ primConvert c
arc x y r s e c surf = SDL.arc surf (i x) (i y) (i r) (i s) (i e) $ colorToPixel $ primConvert c
aaCircle x y r c s = SDL.aaCircle s (i x) (i y) (i r) $ colorToPixel $ primConvert c
filledCircle x y r c s = SDL.filledCircle s (i x) (i y) (i r) $ colorToPixel $ primConvert c
ellipse x y rx ry c s = SDL.ellipse s (i x) (i y) (i rx) (i ry) $ colorToPixel $ primConvert c
aaEllipse x y rx ry c s = SDL.aaEllipse s (i x) (i y) (i rx) (i ry) $ colorToPixel $ primConvert c
filledEllipse x y rx ry c s = SDL.filledEllipse s (i x) (i y) (i rx) (i ry) $ colorToPixel $ primConvert c
pie x y r s e c surf = SDL.pie surf (i x) (i y) (i r) (i s) (i e) $ colorToPixel $ primConvert c
filledPie x y r s e c surf = SDL.filledPie surf (i x) (i y) (i r) (i s) (i e) $ colorToPixel $ primConvert c
trigon x1 y1 x2 y2 x3 y3 c s = SDL.trigon s (i x1) (i y1) (i x2) (i y2) (i x3) (i y3) $ colorToPixel $ primConvert c
aaTrigon x1 y1 x2 y2 x3 y3 c s = SDL.aaTrigon s (i x1) (i y1) (i x2) (i y2) (i x3) (i y3) $ colorToPixel $ primConvert c
filledTrigon x1 y1 x2 y2 x3 y3 c s = SDL.filledTrigon s (i x1) (i y1) (i x2) (i y2) (i x3) (i y3) $ colorToPixel $ primConvert c
polygon points c s = SDL.polygon s (map (\(a, b) -> (i a, i b)) points) $ colorToPixel c
aaPolygon points c s = SDL.aaPolygon s (map (\(a, b) -> (i a, i b)) points) $ colorToPixel $ primConvert c
filledPolygon points c s = void $ SDL.filledPolygon s (map (\(a, b) -> (i a, i b)) points) $ colorToPixel $ primConvert c
texturedPolygon points texture dx dy s = liftIO $ SDL.texturedPolygon s (map (\(a, b) -> (i a, i b)) points) texture (i dx) (i dy)
bezier points steps c s = SDL.bezier s (map (\(a, b) -> (i a, i b)) points) (i steps) $ colorToPixel $ primConvert c

rotozoom src zoomx zoomy smooth = liftIO $ SDL.rotozoom src zoomx zoomy smooth
zoom src zoomx zoomy smooth = liftIO $ SDL.zoom src zoomx zoomy smooth

-- The functions in Graphics.UI.SDL.Primitives act up.
primConvert (RGBA r g b a) = RGBA g b a r
