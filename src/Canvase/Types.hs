-----------------------------------------------------------------------------
-- |
-- Module      :  Canvase.Types
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- The basic types for the entire program.
--
-----------------------------------------------------------------------------
module Canvase.Types where

import qualified Data.Map as M

data Color = RGBA { r :: Double
                  , g :: Double
                  , b :: Double
                  , a :: Double
                  }
           deriving (Show)

data Position a = Position { pX :: a
                           , pY :: a
                           , pZ :: a
                           }
                deriving (Eq, Ord, Show)

instance Functor Position where
  fmap f (Position x y z) = Position (f x) (f y) (f z)

data Axis = X | Y | Z
          deriving (Show)

data Scene = Scene (M.Map (Position Integer) Color)
           deriving (Show)
