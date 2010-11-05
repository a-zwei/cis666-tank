module Util (boolToInt, fromJust) where

boolToInt True = 1
boolToInt False = 0

fromJust (Just a) = a
