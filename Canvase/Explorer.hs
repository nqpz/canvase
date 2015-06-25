-----------------------------------------------------------------------------
-- |
-- Module      :  Canvase.Explorer
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- An interactive scene explorer.
--
-----------------------------------------------------------------------------
module Canvase.Explorer where

import GHC.Int
import Prelude hiding (Left, Right)
import Canvase.Utils
import Canvase.Types
import Canvase.Color
import Canvase.Math
import Canvase.SDL
import Data.Functor
import Data.Word
import qualified Data.Map as M
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLp
import qualified Graphics.UI.SDL.Framerate as FPS
import Control.Monad.State
import System.Exit


type ExplorerM a = StateT ExplorerState IO a
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


getFPSManager :: ExplorerM FPS.FPSManager
getFPSManager = fpsm <$> get

getScreen :: ExplorerM SDL.Surface
getScreen = screenSurf <$> get

getPosition :: ExplorerM (Position Double)
getPosition = position <$> get

setPosition :: Position Double -> ExplorerM ()
setPosition p = modify (\es -> es { position = p })

modifyPosition :: (Position Double -> Position Double) -> ExplorerM ()
modifyPosition f = do
  p <- getPosition
  setPosition $ f p

getAngle :: Axis -> ExplorerM Double
getAngle axis = do
  (x, y, z) <- angles <$> get
  return $ case axis of
    X -> x
    Y -> y
    Z -> z

setAngle :: Axis -> Double -> ExplorerM ()
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

modifyAngle :: Axis -> (Double -> Double) -> ExplorerM ()
modifyAngle axis angleF = do
  angle <- getAngle axis
  setAngle axis $ angleF angle

getTurnDirection :: ExplorerM TurnDirection
getTurnDirection = turnDirection <$> get

setTurnDirection :: TurnDirection -> ExplorerM ()
setTurnDirection d = modify (\es -> es { turnDirection = d })

getMoveDirection :: ExplorerM MoveDirection
getMoveDirection = moveDirection <$> get

setMoveDirection :: MoveDirection -> ExplorerM ()
setMoveDirection d = modify (\es -> es { moveDirection = d })

getScene :: ExplorerM Scene
getScene = scene <$> get

getClock :: ExplorerM Int
getClock = clock <$> get

incClock :: ExplorerM ()
incClock = modify (\es -> es { clock = clock es + 1 })

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
    run :: ExplorerM ()
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
    readEvents :: ExplorerM ()
    readEvents = do
      event <- liftIO SDL.pollEvent
      case event of
        SDL.NoEvent -> return ()
        _ -> do
          eventAction event
          readEvents

    eventAction :: SDL.Event -> ExplorerM ()
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

directionsAct :: ExplorerM ()
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

perspectiveRender :: ExplorerM ()
perspectiveRender = do
  scene <- getScene
  polys <- scenePolygons scene
  liftIO $ print $ length polys
  mapM_ drawPolygon polys

type Polygon = [(Double, Double)]
data FilledPolygon = FilledPolygon [(Double, Double)] Color
                   deriving (Show)

scenePolygons :: Scene -> ExplorerM [FilledPolygon]
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

-- drawPolygon :: FilledPolygon -> ExplorerM ()
-- drawPolygon (FilledPolygon points color) = do
--   screen <- getScreen
--   (w, h) <- liftIO $ surfSize screen
--   void $ liftIO $ filledPolygon (map (stretch w h) points) color screen 

stretch :: Int -> Int -> (Double, Double) -> (Int16, Int16)
stretch w h (x, y) = (round (x * fromIntegral w), round (y * fromIntegral h))
  
drawPolygon :: FilledPolygon -> ExplorerM ()
drawPolygon (FilledPolygon points color) = do
--  let points = [(10, 100), (100, 100), (100, 10)]
  let gfxtransf (RGBA r g b a) = RGBA g b a r
  let color' = gfxtransf color -- lightskyblue
  screen <- getScreen
  (w, h) <- liftIO $ surfSize screen
  let format = SDL.surfaceGetPixelFormat screen
      cw = colorDoubleToWord8
  p <- liftIO $ SDL.mapRGBA format (cw $ r color') (cw $ g color') (cw $ b color') (cw $ a color')
  void $ liftIO $ SDLp.filledPolygon screen (map (stretch w h) points) $ colorToPixel $ gfxtransf color

--stretch _ _ = id
