module Text.Megaparsec.CSS.Types where

import Data.Void
import Text.Megaparsec

type CSSParser = Parsec Void String

data Selector = Selector CompoundSelector Combin CompoundSelector deriving(Show, Eq)

data TypeSelector = TSName String deriving(Show, Eq)

data ClassSelector = ClassSelSingle String deriving(Show, Eq)

data PseudoElement = PEString String deriving(Show, Eq)

data CompoundSelector = CSTypeSelector TypeSelector | CSClass TypeSelector ClassSelector PseudoElement deriving(Show, Eq)

data Combin = Child | Period deriving(Show, Eq)
