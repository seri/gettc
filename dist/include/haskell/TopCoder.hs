module TopCoder ( next
                , parseChar
                , parseString
                , parseBool
                , parseInt
                , parseLong
                , parseFloat
                , parseDouble
                , parseList
                , Parser
                , parse
                , spaces
                ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token
import Data.Char
import Data.Maybe


next :: Parser ()
next = do 
    char ','
    return ()


parseChar :: Parser Char
parseChar = do 
    char '\''
    ret <- anyChar
    char '\''
    return ret

parseBool :: Parser Bool
parseBool = do 
    s <- many1 letter
    case map toLower s of
        "true" -> return True
        "false" -> return False
        otherwise -> fail "expect true or false"


maybeNegative :: Num a => Parser a -> Parser a
maybeNegative parser = (do
    char '-'
    n <- parser
    return (-n)) <|> parser

parseIntegral :: (Integral a, Read a) => Parser a
parseIntegral = maybeNegative ((many1 digit) >>= (return . read))

parseInt = parseIntegral :: Parser Int
parseLong = parseIntegral :: Parser Integer

parseReal :: (RealFloat a, Read a) => Parser a
parseReal = maybeNegative ((many1 (digit <|> char '.')) >>= (return . read))

parseFloat = parseReal :: Parser Float
parseDouble = parseReal :: Parser Double


expect :: Char -> Parser ()
expect c = do
    cc <- optionMaybe (char c) 
    if (isJust cc) 
        then return ()
        else fail "disappointing"

parseClosingQuote :: Parser Char
parseClosingQuote = do 
    char '"' >> spaces
    choice [eof, expect ',', expect ']']
    return '"'

parseString :: Parser String
parseString = do 
    char '"'
    ret <- manyTill anyChar (try (lookAhead parseClosingQuote))
    char '"'
    return ret


parseElems :: Read a => Parser a -> Parser [a]
parseElems parser = (do
    spaces
    x <- parser
    spaces
    (do 
        next
        xs <- parseElems parser
        return (x:xs)) <|> return [x]) <|> return []


parseList :: Read a => Parser a -> Parser [a]
parseList parser = do 
    spaces >> char '[' >> spaces
    xs <- parseElems parser
    spaces >> char ']'
    return xs