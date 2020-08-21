#!/usr/bin/env runghc
module Main (main) where

import System.Environment (getArgs)
import Canvase.Main (runMain)


main :: IO ()
main = getArgs >>= runMain
