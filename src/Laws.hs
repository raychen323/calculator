module Laws where
import DataTypes

laws :: [Law]
laws = [
        -- general
    Law "times zero" (BinOp "Mult" (Con 0) (Var "x")) (Con 0)
    ,   Law "times zero" (BinOp "Mult" (Var "x") (Con 0)) (Con 0)
    ,   Law "identity" (BinOp "Mult" (Con 1) (Var "x")) (Var "x")
    ,   Law "identity" (BinOp "Mult" (Var "x") (Con 1)) (Var "x")
    ,   Law "identity" (BinOp "Sum" (Var "x") (Con 0)) (Var "x")
    ,   Law "identity" (BinOp "Sum" (Con 0) (Var "x")) (Var "x")
    ,   Law "identity" (BinOp "Pow" (Var "x") (Con 1)) (Var "x")
    ,   Law "power" (BinOp "Div" (Var "const") (Var "x")) (BinOp "Mult" (Var "const") (BinOp "Pow" (Var "x") (UnOp "Neg" (Con 1.0))))
    ,   Law "distributive" (BinOp "Sum" (BinOp "Mult" (Var "x") (Var "y")) (BinOp "Mult" (Var "z") (Var "y"))) (BinOp "Mult" (BinOp "Sum" (Var "x") (Var "z")) (Var "y"))
    ,   Law "duplicate" (BinOp "Sum" (Var "x") (Var "x")) (BinOp "Mult" (Con 2) (Var "x"))
    ,   Law "mult by inv" (BinOp "Mult" (Var "x") (BinOp "Div" (Con 1) (Var "y"))) (BinOp "Div" (Var "x") (Var "y"))
    ,   Law "self div" (BinOp "Div" (Var "x") (Var "x")) (Con 1)
    ,   Law "self div" (BinOp "Mult" (Var "x") (BinOp "Div" (Con 1) (Var "x"))) (Con 1)
    ,   Law "self div" (BinOp "Mult" (BinOp "Div" (Con 1) (Var "x")) (Var "x")) (Con 1)
    ,   Law "power inverse" (BinOp "Mult" (BinOp "Pow" (Var "x") (Var "p")) (BinOp "Div" (Var "const") (Var "x"))) (BinOp "Mult" (Var "const") (BinOp "Pow" (Var "x") (BinOp "Min" (Var "p") (Con 1))))
    ,   Law "add powers" (BinOp "Mult" (BinOp "Pow" (Var "x") (Var "p")) (BinOp "Mult" (Var "const") (BinOp "Pow" (Var "x") (Var "a")))) (BinOp "Mult" (Var "const") (BinOp "Pow" (Var "x") (BinOp "Sum" (Var "p") (Var "a"))))
    -- derivative laws
    ,   Law "sin" (UnOp "derive" (UnOp "sin" (Var "x"))) (BinOp "Mult" (UnOp "cos" (Var "x")) (UnOp "derive" (Var "x")))
    ,   Law "cos" (UnOp "derive" (UnOp "cos" (Var "x"))) (BinOp "Mult" (UnOp "Neg" (UnOp "sin" (Var "x"))) (UnOp "derive" (Var "x")))
    ,   Law "ln" (UnOp "derive" (UnOp "ln" (Var "x"))) (BinOp "Mult" (BinOp "Div" (Con 1) (Var "x")) (UnOp "derive" (Var "x")))
    ,   Law "addition" (UnOp "derive" (BinOp "Sum" (Var "a") (Var "b"))) (BinOp "Sum" (UnOp "derive" (Var "a")) (UnOp "derive" (Var "b")))
    ,   Law "power" (UnOp "derive" (BinOp "Pow" (Var "x") (Var "y"))) (BinOp "Mult" (BinOp "Pow" (Var "x") (Var "y")) (UnOp "derive" (BinOp "Mult" (Var "y") (UnOp "ln" (Var "x")))))
    ,   Law "multiplication" (UnOp "derive" (BinOp "Mult" (Var "a") (Var "b"))) (BinOp "Sum" (BinOp "Mult" (UnOp "derive" (Var "a")) (Var "b")) (BinOp "Mult" (Var "a") (UnOp "derive" (Var "b"))))
    ,   Law "const" (UnOp "derive" (Var "const")) (Con 0)
    ,   Law "self" (UnOp "derive" (Var "x")) (Con 1)
        ]