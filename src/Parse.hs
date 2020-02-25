-- Referenced https://markkarpov.com/tutorial/megaparsec.html
module Parse where

import DataTypes

{-# OPTIONS_GHC -Wall #-}
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)
import Data.Char

type Parser = ParsecT Void String Identity

--Parses an expression into different kinds of expressions
parseExpression = space *> ((string "(") *> parseExpression <* (string ")")
                        <|> Con <$> digits
                        <|> try parseUnOp
                        -- <|> try parseBinOp
                        <|> Var <$> parseString) <* space

-- --Gets characters until it hits a bad character as defined in f
parseString :: Parser String
parseString = space *> (some (satisfy f)) <*space
    where f x = not (elem x "\t\r {}()[].=:")

parseUnOp = UnOp <$> parseString <*> (parseExpression)
-- parseBinOp = BinOp <$> parseString

digits :: Parser Int
digits = do ds <- some digit
            return (foldl1 shiftl ds)         
            where shiftl m n = 10*m+n

digit :: Parser Int
digit = cvt <$> satisfy isDigit  
        where cvt d = fromEnum d - fromEnum '0'