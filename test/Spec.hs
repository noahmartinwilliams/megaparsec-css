module Main where

import Text.Megaparsec
import Data.Text as T
import Data.Either
import Text.Megaparsec.CSS.Colors
import Text.Megaparsec.CSS.Declarations
import Text.Megaparsec.CSS.Types

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
    let input = "{ background-color : red; border-radius: 3px; }"
        result = parse cssDeclarationBlock "" (T.pack input)
    if isRight result
    then do
        let (Right result') = result
        if result' == [ColorDeclaration BGColor (ColorName (T.pack "red")), SizeDeclaration BorderRadius (SizePx 3)]
        then
            putStrLn "Test 04 succeeded."
        else
            putStrLn ("Test 04 failed. Got: \"" ++ (show result') ++ "\".")
    else
        let (Left err) = result in putStrLn (errorBundlePretty err)

main :: IO ()
main = do
    test01
    test02
    test03
    test04
