module TopCoder ( next
                , parseChar
                , parseString
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

next :: Parser ()
next = do char ','
          return ()

parseChar :: Parser Char
parseChar = do char '\''
               ret <- anyChar
               char '\''
               return ret

parseString :: Parser String
parseString = do char '\"'
                 ret <- many1 (noneOf "\"")
                 char '\"'
                 return ret

parseInt :: Parser Int
parseInt = do char '-'
              n <- parseInt
              return (-n)
       <|> do cs <- many1 digit
              return (read cs)
 
parseLong :: Parser Integer
parseLong = do char '-'
               n <- parseLong
               return (-n)
        <|> do cs <- many1 digit
               return (read cs)

parseFloat :: Parser Float
parseFloat = do char '-'
                x <- parseFloat
                return (-x)
         <|> do cs <- many1 (digit <|> char '.')
                return (read cs)

parseDouble :: Parser Double
parseDouble = do char '-'
                 x <- parseDouble
                 return (-x)
          <|> do cs <- many1 (digit <|> char '.')
                 return (read cs)


parseElems :: Read a => Parser a -> Parser [a]
parseElems f = do x <- f
                  do next
                     xs <- parseElems f
                     return (x:xs)
                   <|> return [x] 
               <|> return []

parseList :: Read a => Parser a -> Parser [a]
parseList f = do spaces >> char '['
                 xs <- parseElems (spaces >> f)
                 spaces >> char ']'
                 return xs 
