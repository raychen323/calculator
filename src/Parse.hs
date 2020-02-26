-- Referenced Heavily from https://markkarpov.com/tutorial/megaparsec.html
module Parse where

import DataTypes

{-# OPTIONS_GHC -Wall #-}
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Data.Char
import Data.String
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = ParsecT Void String Identity

-- Parses an expression into different kinds of expressions
parseExpression = space *> ((string "(") *> parseExpression <* (string ")")
                        <|> Con <$> digits
                        <|> try parseBinOp
                        <|> try parseUnOp
                        <|> Var <$> parseString) <* space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseBinHelper :: Parser Expression
parseBinHelper = choice
  [ parens parseBinOp
  , try parseUnOp
  , Var <$> parseString
  , Con <$> digits
  ]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

symbol :: String -> Parser String
symbol = L.symbol sc

-- --Gets characters until it hits a bad character as defined in f
parseString :: Parser String
parseString = space *> (some (satisfy f)) <*space
    where f x = not (elem x "\t\r {}()[].=:+-*/^")

parseUnOp = UnOp <$> parseString <*> (parseExpression)

parseBinOp :: Parser Expression
parseBinOp = makeExprParser parseBinHelper operatorTable

binary :: String -> (Expression -> Expression -> Expression) -> Operator Parser Expression
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: String -> (Expression -> Expression) -> Operator Parser Expression
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [ binary "*" (BinOp "Mult")
    , binary "/" (BinOp "Div")
    ]
  , [ binary "+" (BinOp "Sum")
    , binary "-" (BinOp "Min")
    ]
  ]


digits :: Parser Int
digits = do ds <- some digit
            return (foldl1 shiftl ds)         
            where shiftl m n = 10*m+n

digit :: Parser Int
digit = cvt <$> satisfy isDigit  
        where cvt d = fromEnum d - fromEnum '0'

