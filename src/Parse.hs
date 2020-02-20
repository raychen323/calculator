module Parse where

import DataTypes

{-# OPTIONS_GHC -Wall #-}
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)

type Parser = ParsecT Void String Identity

--Checks for properties (induct, forall, equality) and their respective syntax
parseExp :: Parser Expression
parseExp = space *> try ((Application (Reference "Add") <$> ([] <$> (parseExp <* (string "+") <*> parseExp)))
-- parseExp = space *> try ((string "derive" *> (Application (Reference "derive") <$> )))
        --   <|>  (string "forall" *> (Forall <$> parseString <* (string ".") <*> parseProperty))
        --   <|> Equality <$> parseExpression <* string "=" <*> parseExpression)

--Parses an expression into Applications and References
-- parseExpression :: Parser Expression
-- parseExpression = parseHelper >>= rest
--         where rest x = do {y<-some parseHelper; return (Application x y)}
--                     <|> return x

-- --Removes outer parentheses
-- parseHelper :: Parser Expression          
-- parseHelper = space *> ((string "(") *> space *> parseExpression <* space <* (string ")")
--                         <|> (Reference <$> parseString)) <* space

-- --Gets characters until it hits a bad character as defined in f
-- parseString :: Parser String
-- parseString = space *> (some (satisfy f)) <*space
--     where f x = not (elem x "\t\r {}()[].=:")