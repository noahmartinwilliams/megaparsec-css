{-# LANGUAGE OverloadedStrings #-}
module Text.Megaparsec.CSS.Selector where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.CSS.Types
import Text.Megaparsec.CSS.Space
import Data.Text
import Data.Void
import Control.Monad

cssUnivSelector :: Parser Selector
cssUnivSelector = do
    void $ lexeme (single '*')
    return UnivSelector

cssTypeSelector :: Parser Selector
cssTypeSelector = do
    tag <- lexeme (try (choice (Prelude.map string (Prelude.reverse htmlTags ))))
    return (ElemSelector tag)

cssSelector :: Parser Selector
cssSelector = try (cssTypeSelector <|> cssIDSelector <|> cssUnivSelector )

cssIDSelector :: Parser Selector
cssIDSelector = do
    void $ lexeme (single '#')
    tag <- (try (choice (Prelude.map string (Prelude.reverse htmlTags))))
    return (ElemSelectorID tag)

htmlTags :: [Text]
htmlTags = [ "a", "abbr", "acronym", "address", "area", "article", "aside", "audio", "b", "base", "bdi", "bdo", "big", "blockquote", "body", "br", "button", "canvas", "caption", "center ", "cite", "code", "col", "colgroup", "data", "datalist", "dd", "del", "details", "dfn", "dialog", "dir", "div", "dl", "dt", "em", "embed", "fencedframe", "fieldset", "figcaption", "figure", "font", "footer", "form", "frame", "frameset", "h1", "head", "header", "hgroup", "hr", "html", "i", "iframe", "img", "input", "ins", "kbd", "label", "legend", "li", "link", "main", "map", "mark", "marquee", "menu", "meta", "meter", "nav", "nobr", "noembed", "noframes", "noscript", "object", "ol", "optgroup", "option", "output", "p", "param", "picture", "plaintext", "pre", "progress", "q", "rb", "rp", "rt", "rtc", "ruby", "s", "samp", "script", "search", "section", "select", "selectedcontent", "slot", "small", "source", "span", "strike", "strong", "style", "sub", "summary", "sup", "table", "tbody", "td", "template", "textarea", "tfoot", "th", "thead", "time", "title", "tr", "track", "tt", "u", "ul", "var", "video", "wbr", "xmp" ]
