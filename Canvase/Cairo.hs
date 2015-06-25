-----------------------------------------------------------------------------
-- |
-- Module      :  Canvase.Cairo
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Cairo wrapper for use with SDL.
--
-----------------------------------------------------------------------------
module Canvase.Cairo where

import Canvase.Types
import Canvase.Color
import Canvase.SDL

import qualified Graphics.UI.SDL as SDL
import Graphics.Rendering.Cairo
import Foreign.Ptr (castPtr)
import Data.Functor


renderCairo :: Render () -> SDL.Surface -> IO ()
renderCairo m surf = do
  pixels <- castPtr <$> SDL.surfaceGetPixels surf
  (w, h) <- surfSize surf
  canvas <- createImageSurfaceForData pixels FormatARGB32 w h (w * 4)
  renderWith canvas m

setSourceColor :: Color -> Render ()
setSourceColor c = setSourceRGBA (r c) (g c) (b c) (a c)
