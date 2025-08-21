module Text.Megaparsec.CSS.Types where

import Data.Int
import Data.Text
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text

data ColorVal = ColorName Text | ColorHexVal Int8 Int8 Int8 Double deriving(Show, Eq)

data ColorType = BGColor | FGColor deriving(Show, Eq)

data Declaration = ColorDeclaration ColorType ColorVal | SizeDeclaration SizeType Size deriving(Show, Eq)

data Size = SizePx Int deriving(Show, Eq)

data SizeType = BorderRadius deriving(Show, Eq)

data Selector = UnivSelector | ElemSelectorID Text | ElemSelector Text deriving(Show, Eq)

data Combinator = Child | Column | Descendant | Namespace | NextSibling | SubsquentSibling deriving(Show, Eq)

data Combin = CombinSel Selector | Combin Selector Combinator Selector | CombinSelectorList [Selector] deriving(Show, Eq)

data Block = Block Combin [Declaration] deriving(Show, Eq)

data Doc = Doc [Block] deriving(Show, Eq)
