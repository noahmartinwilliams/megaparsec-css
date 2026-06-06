module Text.Megaparsec.CSS.Types where

import Data.Void
import Data.Word
import Text.Megaparsec

type CSSParser = Parsec Void String


data Selector = SelectorCombin [(CompoundSelector, Combin)] deriving(Show, Eq)

data TypeSelector = TSName String deriving(Show, Eq)

data IDSelector = IDSel String deriving(Show, Eq)

data ClassSelector = ClassSelSingle String | ClassSelPseudo PseudoClass | ClassSelIdentPseudo String PseudoClass deriving(Show, Eq)

data PseudoElement = PEString String deriving(Show, Eq)

data PseudoClass = PCFunctional String [Selector] | PCString String deriving(Show, Eq)

data CompoundSelector = CSTypeSelector TypeSelector | CSClass (Maybe TypeSelector) [IDSelector] [ClassSelector] (Maybe PseudoElement) deriving(Show, Eq)

data Combin = NoCombin | Child | Descendant deriving(Show, Eq)

data DisplayType = DBlock | DInlineBlock | DNone | DFlex | DGrid deriving(Show, Eq)


data ColorType = BGColor | FGColor deriving(Show, Eq)

data ColorVal = ColorName String Word8 Word8 Word8 deriving(Show, Eq)


data RuleSet = RuleSet [Selector] [Declaration] deriving(Show, Eq)

data Visibility = Hidden | Visible deriving(Show, Eq)

data Declaration = VisibilityDeclaration Visibility Bool | ColorDeclaration ColorType ColorVal Bool | SizeDeclaration SizeType Size Bool | DisplayDeclaration DisplayType Bool deriving(Show, Eq)

data CSSDoc = CSSDocRS [RuleSet] | CSSDocC [Container] deriving(Show, Eq)


data Size = SizePx Int deriving(Show, Eq)

data SizeType = BorderRadius deriving(Show, Eq)


data MediaType = MTAll | MTPrint | MTScreen deriving(Show, Eq)

data ContainerInfo = CIMedia MediaType deriving(Show, Eq)

data Container = MediaContainer ContainerInfo [RuleSet] deriving(Show, Eq)
