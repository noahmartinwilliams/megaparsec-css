module Text.Megaparsec.CSS.Size where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.CSS.Types

cssSizeVal :: CSSParser Size
cssSizeVal = do
    nums <- many digitChar
    void $ hspace
    void $ string "px"
    return (SizePx (read nums :: Int))

cssSizeType :: CSSParser SizeType
cssSizeType = do
    stype <- (string "border-radius")
    case stype of
        "border-radius" -> return BorderRadius
        _ -> error "Internal error in cssSizeType."

cssPercent :: CSSParser Int
cssPercent = do
    n <- some (digitChar)
    void $ single '%'
    return (read n :: Int)
