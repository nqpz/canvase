-----------------------------------------------------------------------------
-- |
-- Module      :  Canvase.Main
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- The main entry point.
--
-----------------------------------------------------------------------------
module Canvase.Main where

import Canvase.Scenes as Sce
import Canvase.Explorer as Exp
import qualified Canvase.CommandLineParser as Cmd
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (intercalate)


config :: Cmd.Config
config = [ Cmd.Setting "showHelp" "h" "help" 0
           "show this help message and exit" Nothing
         , Cmd.Setting "showVersion" "V" "version" 0
           "show version information and exit" Nothing
         , Cmd.Setting "renderWidth" "w" "render-width" 1
           "the width of the render" (Just ["600"])
         , Cmd.Setting "renderHeight" "h" "render-height" 1
           "the height of the render" (Just ["600"])
         , Cmd.Setting "debug" "d" "debug" 0
           "print debug messages" Nothing
         ]

showHelp = do
  putStrLn "Usage: canvase [OPTION] SCENE"
  putStrLn "A naive 3D renderer"
  putStrLn ""
  putStrLn $ Cmd.formatOptions config
  putStrLn ""
  putStrLn ("Scenes: " ++ intercalate ", " (map fst Sce.canvaseScenes))

showVersion = putStrLn "\
\canvase 0.1.0\n\
\Copyright (C) 2015 Niels G. W. Serup\n\
\This is free software under the terms of the Do What The Fuck You Want To Public\n\
\License (WTFPL); see <http://wtfpl.net/>."

runMain :: [String] -> IO ()
runMain args
  | "showHelp" `M.member` settings = showHelp
  | "showVersion" `M.member` settings = showVersion
  | otherwise = do
    scene' <- scene
    Exp.explore doDebug scene' width height
  where settings :: Cmd.Results
        settings = Cmd.parseCommandLine config args
        doDebug = "debug" `M.member` settings
        scene = fromMaybe (error "no such scene")
                $ lookup sceneS Sce.canvaseScenes
        [sceneS] = Cmd.getRest settings
        width = read $ head (settings M.! "renderWidth")
        height = read $ head (settings M.! "renderHeight")
