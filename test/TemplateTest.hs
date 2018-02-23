module TemplateTest where

import Test.HUnit
import Template

testTemplate :: Test
testTemplate = 
    TestLabel "First Test" $
    TestCase $ assertEqual "Unexpected result" [] "Testing" template "test"