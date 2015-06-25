-----------------------------------------------------------------------------
-- |
-- Module      :  Canvase.Explorer
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- A scene explorer.
--
-----------------------------------------------------------------------------
module Canvase.Explorer where

import Canvase.CanvaseM
import Control.Monad.State
import Canvase.Utils
import Canvase.Types
import Canvase.SDL
import Canvase.Color
import Canvase.Math
import Canvase.Renderer
import Data.Functor
import Data.Word
import System.Exit
import qualified Data.List as L
import qualified Data.Map as M
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Framerate as FPS


explore :: Bool -> Scene -> Int -> Int -> IO ()
explore doDebug scene width height = do
  SDL.init [SDL.InitVideo]
  screen <- SDL.setVideoMode width height 32
            [SDL.HWSurface, SDL.HWAccel, SDL.DoubleBuf]
  fm <- FPS.new
  FPS.init fm
  FPS.set fm 60
  evalStateT run $ ES fm screen (Position 0 0 0) (0, 0, 0) NoTurn NoMove scene 0
  where
    run :: CanvaseM ()
    run = do
      readEvents
      directionsAct
      screen <- getScreen
      liftIO $ fillRect Nothing black screen
      perspectiveRender
      liftIO $ SDL.flip screen
      fm <- getFPSManager
      liftIO $ FPS.delay fm
      incClock
      run
    readEvents :: CanvaseM ()
    readEvents = do
      event <- liftIO SDL.pollEvent
      case event of
        SDL.NoEvent -> return ()
        _ -> do
          eventAction event
          readEvents

    eventAction :: SDL.Event -> CanvaseM ()
    eventAction SDL.Quit = liftIO exitSuccess
    eventAction event@(SDL.KeyDown (SDL.Keysym k mods _))
      | k == SDL.SDLK_UP = if hasCtrl event
                           then setTurnDirection TurnUp
                           else setMoveDirection MoveForward
      | k == SDL.SDLK_DOWN = if hasCtrl event
                             then setTurnDirection TurnDown
                             else setMoveDirection MoveBackward
      | k == SDL.SDLK_LEFT = if hasCtrl event
                             then setMoveDirection MoveLeft
                             else setTurnDirection TurnLeft
      | k == SDL.SDLK_RIGHT = if hasCtrl event
                              then setMoveDirection MoveRight
                              else setTurnDirection TurnRight
    eventAction event@(SDL.KeyUp (SDL.Keysym k mods _)) = do
      t <- getTurnDirection
      m <- getMoveDirection
      case k of
        SDL.SDLK_LEFT -> do
          when (t == TurnLeft) $ setTurnDirection NoTurn
          when (m == MoveLeft) $ setMoveDirection NoMove
        SDL.SDLK_RIGHT -> do
          when (t == TurnRight) $ setTurnDirection NoTurn
          when (m == MoveRight) $ setMoveDirection NoMove
        SDL.SDLK_UP -> do
          when (m == MoveForward) $ setMoveDirection NoMove
          when (t == TurnUp) $ setTurnDirection NoTurn
        SDL.SDLK_DOWN -> do
          when (m == MoveBackward) $ setMoveDirection NoMove
          when (t == TurnDown) $ setTurnDirection NoTurn
        _ -> return ()
    eventAction _ = return ()

    hasCtrl :: SDL.Event -> Bool
    hasCtrl (SDL.KeyDown (SDL.Keysym _ mods _)) = ctrlInMods mods
    hasCtrl (SDL.KeyUp (SDL.Keysym _ mods _)) = ctrlInMods mods
    hasCtrl _ = False

    hasShift :: SDL.Event -> Bool
    hasShift (SDL.KeyDown (SDL.Keysym _ mods _)) = shiftInMods mods
    hasShift (SDL.KeyUp (SDL.Keysym _ mods _)) = shiftInMods mods
    hasShift _ = False

    ctrlInMods mods = any (`elem` mods)
                      [SDL.KeyModCtrl, SDL.KeyModLeftCtrl,
                       SDL.KeyModRightCtrl]

    shiftInMods mods = any (`elem` mods)
                      [SDL.KeyModShift, SDL.KeyModLeftShift,
                       SDL.KeyModRightShift]

directionsAct :: CanvaseM ()
directionsAct = do
  t <- getTurnDirection
  case t of
    TurnRight -> modifyAngle Y (+0.05)
    TurnLeft -> modifyAngle Y (subtract 0.05)
    TurnUp -> modifyAngle X (+0.05)
    TurnDown -> modifyAngle X (subtract 0.05)
    NoTurn -> return ()

  m <- getMoveDirection
  c <- getClock
  ya <- getAngle Y
  xa <- getAngle X

  let d = 0.3
  modifyPosition $ \p -> posAdd p $ rotate Y ya $ rotate X xa $ case m of
    MoveForward -> Position 0 0 d
    MoveBackward -> Position 0 0 (-d)
    MoveRight -> Position d 0 0
    MoveLeft -> Position (-d) 0 0
    NoMove -> Position 0 0 0
