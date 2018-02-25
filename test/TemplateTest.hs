module TemplateTest where

import Test.HUnit
import Template

oneVariable :: Test
oneVariable = 
    TestLabel "One Variable" $
    TestCase $ assertEqual "Unexpected result" expected 
        (templateSet myTemplate "name" "Oskar")
        where
            expected = template "Hello Oskar"
            myTemplate = template "Hello ${name}"

differentValue :: Test
differentValue = 
    TestLabel "Different Value" $
    TestCase $ assertEqual "Unexpected result" expected
        (templateSet myTemplate "name" "someone else")
        where
            expected = template "Hello someone else"
            myTemplate = template "Hello ${name}"


-- TODO: result could be implemented in a cleaner way? - Oskar Mendel 2018-02-25
multipleVariables :: Test
multipleVariables = 
    TestLabel "Multiple Variables" $
    TestCase $ assertEqual "Unexpected result" expected result
        where
            expected = template "Hello Oskar, did you make this for school?"
            result = templateSet (templateSet myTemplate "purpose" "school") "name" "Oskar"
            myTemplate = template "Hello ${name}, did you make this for ${purpose}?"
