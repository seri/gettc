import TopCoder
import Test.HUnit
import Text.Parsec.Error (ParseError)
import Data.Either

instance Eq ParseError where
    (==) e1 e2 = True

testParsePrims :: Test
testParsePrims = TestCase 
    (do let x = parse parseInt "" "123456"
        assertEqual "" (Right 123456) x
        let s = parse (spaces >> parseString) "" " \"Hello World\" "
        assertEqual "" (Right "Hello World") s
        let y = parse parseDouble "" "123.456"
        assertEqual "" (Right 123.456) y
        let c = parse parseChar "" "'c'"
        assertEqual "" (Right 'c') c)

testParseArray :: Test
testParseArray = TestCase
    (do let xs = parse (parseList parseInt) "" "[]"
        assertEqual "" (Right []) xs
        let ys = parse (parseList parseDouble) "" "[ 1.23, 4.56, 78.9 ]"
        assertEqual "" (Right [1.23, 4.56, 78.9]) ys
        let ss = parse (parseList parseString) "" "[\"Hello\",\"World\"]"
        assertEqual "" (Right ["Hello", "World"]) ss)

getVars :: Parser (String, Char, Int, [String])
getVars = do handle <- spaces >> parseString ; next
             gender <- spaces >> parseChar ; next
             age <- spaces >> parseInt ; next  
             names <- spaces >> (parseList parseString)
             return (handle, gender, age, names)

testRealLife :: Test
testRealLife = TestCase
    (do let ret = parse getVars "" "\n\t\"Seri\",\t'M'\n\t,\n24\t, \n[\"Quoc\",\n\"Anh\",\"Trinh\"]"
        let Right (handle, gender, age, names) = ret
        assertEqual "" "Seri" handle
        assertEqual "" 'M' gender
        assertEqual "" 24 age
        assertEqual "" ["Quoc", "Anh", "Trinh"] names)

tests = TestList [ TestLabel "testParsePrims" testParsePrims 
                 , TestLabel "testParseArray" testParseArray]

main = runTestTT tests
