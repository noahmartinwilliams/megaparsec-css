module Main where

import Text.Megaparsec
import Data.Text as T
import Data.Either
import Text.Megaparsec.CSS.Properties
import Text.Megaparsec.CSS.Values

test01 :: IO ()
test01 = do
    let input = "background-color"
        result = parse cssColorProperty "" input
    if isRight result
    then do
        let (Right result') = result
        if result' == "background-color"
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
        if result' == "red"
        then
            putStrLn "Test 02 succeeded."
        else
            putStrLn ("Test 02 failed. Got: \"" ++ (show result') ++ "\".")
    else
        let (Left err) = result in putStrLn (errorBundlePretty err)

main :: IO ()
main = do
    test01
    test02
