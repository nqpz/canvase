-----------------------------------------------------------------------------
-- |
-- Module      :  Canvase.Utils
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Miscellaneous utilities.
--
-----------------------------------------------------------------------------
module Canvase.Utils where

import Debug.Trace (trace)


debug :: Show a => String -> a -> a
debug s a = trace (s ++ ": " ++ show a) a

debug' :: Show a => String -> a -> a
debug' _ x = x
