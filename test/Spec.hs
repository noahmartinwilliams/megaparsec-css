module Main where

import Text.Megaparsec
import Data.Text as T
import Data.Either
import Text.Megaparsec.CSS.Properties

test01 :: IO ()
test01 = do
    let input = "background-color"
        result = parse cssProperty "" (T.pack input)
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

main :: IO ()
main = do
    test01
