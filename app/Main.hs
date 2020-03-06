module Main where
import Presentable
main :: IO ()
main = do  
    putStrLn "Please type in your equation."
    eq <- getLine
    putStrLn(finalOutput(pretty(solve(eq))))
    main
