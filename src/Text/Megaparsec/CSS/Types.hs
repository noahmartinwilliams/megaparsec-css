module Text.Megaparsec.CSS.Types where

import Data.Void
import Text.Megaparsec

type CSSParser = Parsec Void String

data Selector = SelectorCombin [(CompoundSelector, Combin)] deriving(Show, Eq)

data TypeSelector = TSName String deriving(Show, Eq)

data IDSelector = IDSel String deriving(Show, Eq)

data ClassSelector = ClassSelSingle String deriving(Show, Eq)

data PseudoElement = PEString String deriving(Show, Eq)

data CompoundSelector = CSTypeSelector TypeSelector | CSClass (Maybe TypeSelector) [IDSelector] [ClassSelector] (Maybe PseudoElement) deriving(Show, Eq)

data Combin = NoCombin | Child | Period deriving(Show, Eq)
