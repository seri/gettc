import TopCoder
import Data.List
import Test.HUnit
import Text.Parsec.Error (ParseError)


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False


shouldParse :: (Eq a, Show a) => String -> Parser a -> String -> a -> Assertion
shouldParse context parser source expected = do
    let result = parse parser context source
    assertEqual context (Right expected) result

shouldFail :: (Eq a, Show a) => String -> Parser a -> String -> Assertion
shouldFail context parser source = do
    let result = parse parser context source
    assertBool context (isLeft result)


testParsePrimitives :: Test
testParsePrimitives = TestCase (do
    shouldParse "parse an int" parseInt "123456" 123456
    shouldParse "parse a normal string" parseString "\"Hello World\"" "Hello World"
    shouldParse "parse a nasty string with a quote" parseString "\"The \"Red Wedding\"" "The \"Red Wedding"
    shouldParse "parse a nasty string with two quotes" parseString "\"The \"Red\" Wedding\"" "The \"Red\" Wedding"
    shouldParse "parse a negative double" parseDouble "-20.14" (-20.14)
    shouldFail  "parse an invalid foat" parseFloat "x2014"
    shouldParse "parse a quoted character" parseChar "'c'" 'c'
    shouldParse "parse a unquoted character" parseChar "c" 'c'
    shouldParse "parse a boolean value" parseBool "fAlSe" False)

testParseArray :: Test
testParseArray = TestCase (do
    shouldParse "parse an empty int array" (parseList parseInt) "[]" []
    shouldParse "parse a double array" (parseList parseDouble) "[ 1.23, 4.56, 78.9 ]" [1.23, 4.56, 78.9]
    shouldParse "parse a string array" (parseList parseString) "[\"Hello\",\"World\"]" ["Hello", "World"])


getVars :: Parser ([[String]], Char, Double, Bool, Char, [Int])
getVars = do names <- spaces >> (parseList (parseList parseString)) ; spaces >> next
             grade <- spaces >> parseChar ; spaces >> next
             year <- spaces >> parseDouble ; spaces >> next
             released <- spaces >> parseBool ; spaces >> next
             code <- spaces >> parseChar ; spaces >> next
             numbers <- spaces >> (parseList parseInt)
             return (names, grade, year, released, code, numbers)

complexInput :: String
complexInput = intercalate "\n" [ "[ [ \"Jon Snow\""
                                , "  , \"Lord Varys\""
                                , "  , \"The \"Little Finger\"\" ]"
                                , ", [ ] ]"
                                , ", A, 20.14, false, 'X'"
                                , ", [ -2 , 0 , 1 , 4 ]" ]

testParseEverything :: Test
testParseEverything = TestCase (do
    let result = parse getVars "parsing variables" complexInput
    let Right (names, grade, year, released, code, numbers) = result
    assertEqual "string[][] names" [["Jon Snow", "Lord Varys", "The \"Little Finger\""], []] names
    assertEqual "char grade" 'A' grade
    assertEqual "double year" 20.14 year
    assertEqual "boolean released" False released
    assertEqual "char code" 'X' code
    assertEqual "int[] numbers" [-2, 0, 1, 4] numbers)


tests = TestList [ TestLabel "testParsePrimitives" testParsePrimitives
                 , TestLabel "testParseArray" testParseArray
                 , TestLabel "testParseEverything" testParseEverything ]

main = do
    runTestTT tests
