module TryCalculating where
import DataTypes
import Simplify
import Parse
import Text.Megaparsec


test = calculate laws output where
    Right output = parse parseExpression "" "derive (sin(x)*(x^(1/2)))"

laws = [law1, law2, law3, law4, law5, law6, law7, law8, law9]
law1 = Law "sin" (UnOp "derive" (UnOp "sin" (Var "x"))) (BinOp "Mult" (UnOp "cos" (Var "x")) (UnOp "derive" (Var "x")))
law2 = Law "cos" (UnOp "derive" (UnOp "cos" (Var "x"))) (BinOp "Mult" (UnOp "Neg" (UnOp "sin" (Var "x"))) (UnOp "derive" (Var "x")))
law3 = Law "const" (UnOp "derive" (Con 2)) (Con 0)
law4 = Law "ln" (UnOp "derive" (UnOp "ln" (Var "x"))) (BinOp "Mult" (BinOp "Div" (Con 1) (Var "x")) (UnOp "derive" (Var "x")))
law5 = Law "addition" (UnOp "derive" (BinOp "Sum" (Var "a") (Var "b"))) (BinOp "Sum" (UnOp "derive" (Var "a")) (UnOp "derive" (Var "b")))
law6 = Law "power" (UnOp "derive" (BinOp "Pow" (Var "x") (Var "y"))) (BinOp "Mult" (BinOp "Pow" (Var "x") (Var "y")) (UnOp "derive" (BinOp "Mult" (Var "y") (UnOp "ln" (Var "x")))))
law7 = Law "multiplication" (UnOp "derive" (BinOp "Mult" (Var "a") (Var "b"))) (BinOp "Sum" (BinOp "Mult" (UnOp "derive" (Var "a")) (Var "b")) (BinOp "Mult" (Var "a") (UnOp "derive" (Var "b"))))
law8 = Law "plus" (BinOp "Sum" (Var "x") (Var "x")) (BinOp "Mult" (Con 2) (Var "x"))
law9 = Law "self" (UnOp "derive" (Var "x")) (Con 1)