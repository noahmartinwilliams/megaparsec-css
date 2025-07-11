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
        result = parse cssColorProperty "" input
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
        result = parse cssColorValNamed "" input
    if isRight result
    then do
        let (Right result') = result
        if result' == (ColorName "red")
        then
            putStrLn "Test 02 succeeded."
        else
            putStrLn ("Test 02 failed. Got: \"" ++ (show result') ++ "\".")
    else
        let (Left err) = result in putStrLn (errorBundlePretty err)

test03 :: IO ()
test03 = do
    let input = "background-color : red;"
        result = parse cssColorDeclaration "" input
    if isRight result
    then do
        let (Right result') = result
        if result' == (ColorDeclaration BGColor (ColorName "red"))
        then
            putStrLn "Test 03 succeeded."
        else
            putStrLn ("Test 03 failed. Got: \"" ++ (show result') ++ "\".")
    else
        let (Left err) = result in putStrLn (errorBundlePretty err)

main :: IO ()
main = do
    test01
    test02
    test03
