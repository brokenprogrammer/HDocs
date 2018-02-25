module Main where

import Data.Text

import Template

main :: IO ()
main = putStrLn $ unpack $ template "Hello, Haskell!"
