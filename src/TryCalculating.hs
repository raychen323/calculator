module TryCalculating where
import DataTypes
import Simplify
import Parse

test = calculate [law1,law2, law3] (BinOp "Sum" (Con 3) (Con 3))

law1 = Law "addition" (UnOp "derive" (BinOp "Sum" (Var "a") (Var "b"))) (BinOp "Sum" (UnOp "derive" (Var "a")) (UnOp "derive" (Var "b")))
law2 = Law "multiplication" (UnOp "derive" (BinOp "Mult" (Var "a") (Var "b"))) (BinOp "Sum" (BinOp "Mult" (UnOp "derive" (Var "a")) (Var "b")) (BinOp "Mult" (Var "a") (UnOp "derive" (Var "b"))))
law3 = Law "plus" (BinOp "Sum" (Var "x") (Var "x")) (BinOp "Mult" (Con 2) (Var "x"))
