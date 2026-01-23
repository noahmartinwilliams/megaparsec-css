module Text.Megaparsec.CSS.Colors where

import Control.Monad
import Data.Bits
import Data.Map as Map
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.CSS.AllColors
import Text.Megaparsec.CSS.Combin
import Text.Megaparsec.CSS.Ident
import Text.Megaparsec.CSS.Types

cssColorType :: CSSParser ColorType
cssColorType = do
    ct <- (string "background-color") <|> (string "foreground-color")
    case ct of
        "background-color" -> return BGColor
        "foreground-color" -> return FGColor

colorHex :: String -> (Word8, Word8, Word8)
colorHex ('#' : rest) = do
    let redC = Prelude.take 2 rest
        greenC = Prelude.take 2 (Prelude.drop 2 rest)
        blueC = Prelude.take 2 (Prelude.drop 4 rest)
    (hex2Int8 redC, hex2Int8 greenC, hex2Int8 blueC)

hex2Int8 :: String -> Word8
hex2Int8 [a, b] = (.|.) ((hexNibble a) `shift` 4) (hexNibble b)

hexNibble :: Char -> Word8
hexNibble '0' = 0
hexNibble '1' = 1
hexNibble '2' = 2
hexNibble '3' = 3
hexNibble '4' = 4
hexNibble '5' = 5
hexNibble '6' = 6
hexNibble '7' = 7
hexNibble '8' = 8
hexNibble '9' = 9
hexNibble 'a' = 10
hexNibble 'A' = 10
hexNibble 'b' = 11
hexNibble 'B' = 11
hexNibble 'c' = 12
hexNibble 'C' = 12
hexNibble 'd' = 13
hexNibble 'D' = 13
hexNibble 'e' = 14
hexNibble 'E' = 14
hexNibble 'f' = 15
hexNibble 'F' = 15

cssColorValNamed :: CSSParser ColorVal
cssColorValNamed = do
    let colors = fst (Prelude.unzip allColors)
    cn <- choice (Prelude.map chunk colors)
    let m = Map.fromList allColors
        (Just rgbVal) = Map.lookup cn m
        (r, g, b) = colorHex rgbVal 
    return (ColorName cn r g b)
