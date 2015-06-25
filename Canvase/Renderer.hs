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

import Canvase.Utils
import Canvase.Types
import Canvase.Color
import Canvase.Math
import Data.Functor
import Data.Word
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Environment as Env


render :: Bool -> Scene -> Int -> Int -> SDL.Surface -> IO SDL.Surface
render doDebug scene width height surf = undefined
