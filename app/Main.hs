module Main where
import Laws
import Presentable
import Simplify
import Parse
import Text.Megaparsec
import Text.Megaparsec.Char()
import DataTypes

-- Applys calculation to our parser result
solve :: String -> Calculation
solve eq = calculate (laws var) (UnOp "derive" output) where
  Right (Derive var output) = parse parseDerive "" eq

-- genLaw :: String -> String
-- genLaw eq = show output where
--   Right output = parse parseExpression "" eq

main :: IO ()
main = do  
    putStrLn "Please type in your equation."
    eq <- getLine
    -- putStrLn(genLaw(eq)) -- use this for running parser for generating laws
    putStrLn(finalOutput(pretty(solve(eq))))
    main
