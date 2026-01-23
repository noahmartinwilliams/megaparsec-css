module Text.Megaparsec.CSS.Types where

import Data.Void
import Data.Word
import Text.Megaparsec

type CSSParser = Parsec Void String

data ColorVal = ColorName String Word8 Word8 Word8 deriving(Show, Eq)

data Selector = SelectorCombin [(CompoundSelector, Combin)] deriving(Show, Eq)

data TypeSelector = TSName String deriving(Show, Eq)

data IDSelector = IDSel String deriving(Show, Eq)

data ClassSelector = ClassSelSingle String deriving(Show, Eq)

data PseudoElement = PEString String deriving(Show, Eq)

data CompoundSelector = CSTypeSelector TypeSelector | CSClass (Maybe TypeSelector) [IDSelector] [ClassSelector] (Maybe PseudoElement) deriving(Show, Eq)

data Combin = NoCombin | Child | Period deriving(Show, Eq)

data ColorType = BGColor | FGColor deriving(Show, Eq)

data Declaration = ColorDeclaration ColorType ColorVal | SizeDeclaration SizeType Size deriving(Show, Eq)


data Size = SizePx Int deriving(Show, Eq)

data SizeType = BorderRadius deriving(Show, Eq)
