module Main where
import Laws
import Presentable
import Simplify
import Parse
import Text.Megaparsec
import Text.Megaparsec.Char()
import DataTypes

-- Applies calculation to our parser result
solve :: String -> Calculation
solve eq = calc where
  calc = case (parse parseDerive "" eq) of
        Left _ -> (Calc (Var "error, parsing failed") [])
        Right (Derive var output) -> calculate (laws var) (UnOp "derive" output)


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
