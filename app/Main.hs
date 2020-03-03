module Main where
import TryCalculating
import Presentable
import Simplify
import Parse
import Text.Megaparsec
import Text.Megaparsec.Char()
import DataTypes

solve :: String -> Calculation
solve eq = calculate laws output where
  Right output = parse parseExpression "" eq

genLaw :: String -> String
genLaw eq = show output where
  Right output = parse parseExpression "" eq

main :: IO ()
main = do  
    putStrLn "Please type in your equation."
    eq <- getLine
    -- putStrLn(genLaw(eq)) -- use this for running parser for generating laws
    putStrLn(show(pretty(solve(eq))))
    main
