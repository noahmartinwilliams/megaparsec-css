module Text.Megaparsec.CSS.Types where

import Data.Int

data ColorVal = ColorHexVal Int8 Int8 Int8 Int8 deriving(Show, Eq)
