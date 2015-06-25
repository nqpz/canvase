-----------------------------------------------------------------------------
-- |
-- Module      :  Canvase.CanvaseM
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- The monad.
--
-----------------------------------------------------------------------------
module Canvase.CanvaseM where

import GHC.Int
import Canvase.Utils
import Canvase.Types
import Canvase.Color
import Canvase.Math
import Canvase.SDL
import Data.Functor
import Data.Word
import qualified Data.Map as M
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Framerate as FPS
import Control.Monad.State
import System.Exit


type CanvaseM a = StateT ExplorerState IO a
data ExplorerState = ES { fpsm :: FPS.FPSManager
                        , screenSurf :: SDL.Surface
                        , position :: Position Double
                        , angles :: (Double, Double, Double)
                        , turnDirection :: TurnDirection
                        , moveDirection :: MoveDirection
                        , scene :: Scene
                        , clock :: Int
                        }
                   deriving (Show)

data TurnDirection = NoTurn
                   | TurnRight
                   | TurnLeft
                   | TurnUp
                   | TurnDown
                   deriving (Eq, Show)

data MoveDirection = NoMove
                   | MoveForward
                   | MoveBackward
                   | MoveRight
                   | MoveLeft
                   deriving (Eq, Show)


getFPSManager :: CanvaseM FPS.FPSManager
getFPSManager = fpsm <$> get

getScreen :: CanvaseM SDL.Surface
getScreen = screenSurf <$> get

getPosition :: CanvaseM (Position Double)
getPosition = position <$> get

setPosition :: Position Double -> CanvaseM ()
setPosition p = modify (\es -> es { position = p })

modifyPosition :: (Position Double -> Position Double) -> CanvaseM ()
modifyPosition f = do
  p <- getPosition
  setPosition $ f p

getAngle :: Axis -> CanvaseM Double
getAngle axis = do
  (x, y, z) <- angles <$> get
  return $ case axis of
    X -> x
    Y -> y
    Z -> z

setAngle :: Axis -> Double -> CanvaseM ()
setAngle axis angle = do
  angles <- (\(xm, ym, zm) -> do
                x <- xm
                y <- ym
                z <- zm
                return (x, y, z)) $ case axis of
    X -> (return angle, getAngle Y, getAngle Z)
    Y -> (getAngle X, return angle, getAngle Z)
    Z -> (getAngle X, getAngle Y, return angle)
  modify (\es -> es { angles = angles })

modifyAngle :: Axis -> (Double -> Double) -> CanvaseM ()
modifyAngle axis angleF = do
  angle <- getAngle axis
  setAngle axis $ angleF angle

getTurnDirection :: CanvaseM TurnDirection
getTurnDirection = turnDirection <$> get

setTurnDirection :: TurnDirection -> CanvaseM ()
setTurnDirection d = modify (\es -> es { turnDirection = d })

getMoveDirection :: CanvaseM MoveDirection
getMoveDirection = moveDirection <$> get

setMoveDirection :: MoveDirection -> CanvaseM ()
setMoveDirection d = modify (\es -> es { moveDirection = d })

getScene :: CanvaseM Scene
getScene = scene <$> get

getClock :: CanvaseM Int
getClock = clock <$> get

incClock :: CanvaseM ()
incClock = modify (\es -> es { clock = clock es + 1 })
