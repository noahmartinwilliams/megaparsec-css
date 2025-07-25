module Main where

import Text.Megaparsec
import Data.Text as T
import Data.Either
import Text.Megaparsec.CSS.Colors
import Text.Megaparsec.CSS.Declarations
import Text.Megaparsec.CSS.Types
import Text.Megaparsec.CSS.Selector

test01 :: IO ()
test01 = do
    let input = "background-color"
        result = parse cssColorProperty "" (T.pack input)
    if isRight result
    then do
        let (Right result') = result
        if result' == BGColor
        then
            putStrLn "Test 01 succeeded."
        else
            putStrLn "Test 01 failed."
    else
        let (Left err) = result in putStrLn (errorBundlePretty err)

test02 :: IO ()
test02 = do
    let input = "red"
        result = parse cssColorValNamed "" (T.pack input)
    if isRight result
    then do
        let (Right result') = result
        if result' == (ColorName (T.pack "red"))
        then
            putStrLn "Test 02 succeeded."
        else
            putStrLn ("Test 02 failed. Got: \"" ++ (show result') ++ "\".")
    else
        let (Left err) = result in putStrLn (errorBundlePretty err)

test03 :: IO ()
test03 = do
    let input = "background-color : red;"
        result = parse cssColorDeclaration "" (T.pack input)
    if isRight result
    then do
        let (Right result') = result
        if result' == (ColorDeclaration BGColor (ColorName (T.pack "red")))
        then
            putStrLn "Test 03 succeeded."
        else
            putStrLn ("Test 03 failed. Got: \"" ++ (show result') ++ "\".")
    else
        let (Left err) = result in putStrLn (errorBundlePretty err)

test04 :: IO ()
test04 = do
    let input = "p { background-color : red; border-radius: 3px; }"
        result = parse cssDeclarationBlock "" (T.pack input)
    if isRight result
    then do
        let (Right (Block _ result')) = result
        if result' == [ColorDeclaration BGColor (ColorName (T.pack "red")), SizeDeclaration BorderRadius (SizePx 3)]
        then
            putStrLn "Test 04 succeeded."
        else
            putStrLn ("Test 04 failed. Got: \"" ++ (show result') ++ "\".")
    else
        let (Left err) = result in putStrLn (errorBundlePretty err)

test05 :: IO ()
test05 = do
    let input = "rgb(255, 0, 1)"
        result = parse cssRGBVal "" (T.pack input)
    if isRight result
    then do
        let (Right result') = result
        let (ColorHexVal r g b _) = result'
        if (r == 255) && (g == 0) && (b == 1)
        then
            putStrLn "Test 05 succeeded."
        else
            putStrLn ("Test 05 failed. Got: \"" ++ (show result') ++ "\".")
    else
        let (Left err) = result in putStrLn (errorBundlePretty err)

test06 :: IO ()
test06 = do
    let input = "p { background-color: blue; }"
        result = parse cssDeclarationBlock "" (T.pack input)
    if isRight result
    then do
        let (Right result') = result
        let (Block selector _ ) = result'
        if (ElemSelector (T.pack "p")) == selector
        then
            putStrLn "Test 06 succeeded."
        else
            putStrLn ("Test 06 failed. Got: \"" ++ (show result') ++ "\".")
    else
        let (Left err) = result in putStrLn (errorBundlePretty err)

main :: IO ()
main = do
    test01
    test02
    test03
    test04
    test05
    test06
