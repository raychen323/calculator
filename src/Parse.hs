-- Referenced Heavily from https://markkarpov.com/tutorial/megaparsec.html
module Parse where

import DataTypes

{-# OPTIONS_GHC -Wall #-}
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = ParsecT Void String Identity

-- Parses the string following d/d as the variable we are deriving with respect to
parseDerive :: Parser Derive
parseDerive = Derive <$> (string "d/d" *> parseString) <*> parseExpression

-- Parses an expression into different kinds of expressions
parseExpression :: Parser Expression
parseExpression = space *> (try parseBinOp
                        <|> try parseUnOp 
                        <|> Con <$> float
                        <|> Var <$> parseString
                        <|> (string "(") *> parseExpression <* (string ")")) <* space

-- Removes parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

--Helper for binop parser
parseBinHelper :: Parser Expression
parseBinHelper = space *> choice
  [ parens parseBinOp
  , try parseUnOp
  , Con <$> float
  , Var <$> parseString
  ] <* space

--Removes spaces
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Parses out comments
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

-- parses unary operations
parseUnOp :: Parser Expression
parseUnOp = UnOp <$> parseString <*> (string "(" *> (parseExpression) <* string ")")

--Parses infix bin operations
parseBinOp :: Parser Expression
parseBinOp = makeExprParser parseBinHelper operatorTable

-- helper for parsing binary operations
binary :: String -> (Expression -> Expression -> Expression) -> Operator Parser Expression
binary  name f = InfixL  (f <$ symbol name)

-- parses prefix/postfix operations
prefix, postfix :: String -> (Expression -> Expression) -> Operator Parser Expression
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

--Used to declare operations
operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ 
    [prefix "-" (UnOp "Neg")]
  , [ binary "^" (BinOp "Pow")
    ]
  , [ binary "*" (BinOp "Mult")
    , binary "/" (BinOp "Div")
    ]
  , [ binary "+" (BinOp "Sum")
    , binary "-" (BinOp "Min")
    ]
  ]

-- parses floats
float :: Parser Float
float = space *> (try (L.float)
        <|> (fromIntegral <$> L.decimal)) <* space