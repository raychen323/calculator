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

main :: IO ()
main = do  
    putStrLn "Please type in your equation."
    eq <- getLine
    putStrLn(show(pretty(solve(eq))))
    main
