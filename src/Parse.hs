module Parse where

import DataTypes

{-# OPTIONS_GHC -Wall #-}
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)

type Parser = ParsecT Void String Identity

--Parses an expression into different kinds of expressions
parseExpression = space *> try ((string "(") *> parseExpression <* (string ")")
                        <|> (parseUnOp)
                        <|> return (Const 1)) <* space


-- --Gets characters until it hits a bad character as defined in f
parseString :: Parser String
parseString = space *> (some (satisfy f)) <*space
    where f x = not (elem x "\t\r {}()[].=:")

parseUnOp = UnOp <$> parseString <*> parseExpression

-- parseBinOp = BinOp <$> parseExpression <*
-- Need to find way to do infix parsing