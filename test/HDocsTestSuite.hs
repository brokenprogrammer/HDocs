module Main where

import TemplateTest
import System.Exit
import Test.HUnit

-- main :: IO Counts
-- main = do results <- runTestTT $ TestList [ oneVariable,
--                                             differentValue
--                                           ]
--             (if errors results + failiures results == 0
--             then exitWith ExitSuccess
--             else exitWith (ExitFailure 1))

--results <- 

main :: IO Counts
main = do  { results <- runTestTT $ TestList  [oneVariable, differentValue] ;
             if errors results + failures results == 0
                then exitWith ExitSuccess
                else exitWith (ExitFailure 1)
           }