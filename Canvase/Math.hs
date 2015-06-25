-----------------------------------------------------------------------------
-- |
-- Module      :  Canvase.Math
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- The necessary math stuff.
--
-----------------------------------------------------------------------------
module Canvase.Math where

import Canvase.Types


rotate :: Axis -> Double -> Position Double -> Position Double
rotate axis angle (Position x y z) = case axis of
  X -> Position x (y * c - z * s) (y * s + z * c)
  Y -> Position (z * s + x * c) y (z * c - x * s)
  Z -> Position (x * c - y * s) (x * s + y * c) z
  where (c, s) = (cos angle, sin angle)

posAdd :: Num a => Position a -> Position a -> Position a
posAdd (Position x0 y0 z0) (Position x1 y1 z1)
  = Position (x0 + x1) (y0 + y1) (z0 + z1)