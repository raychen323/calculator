module Laws where
import DataTypes
import Parse
import Text.Megaparsec

-- Returns a list of our laws, parameter var is only used for the self derivative law, since we know that the derivative should be 1.

parseLaws :: [(String, String, String)] -> [Law]
parseLaws [x] = parseLaw x
parseLaws (x:xs) = parseLaw x ++ parseLaws xs
parseLaw (name, left, right) = [Law name leftExpr rightExpr] where
                                        leftExpr = case (parse parseExpression "" left) of
                                            Left _ -> (Var "error, parsing failed")
                                            Right output -> output
                                        rightExpr = case (parse parseExpression "" right) of
                                            Left _ -> (Var "error, parsing failed")
                                            Right output -> output

stringLaws = [
     ("Times zero", "0*x", "0")
    ,("Times zero", "x*0", "0")
    ,("Multiplicative Identity", "1*x", "x")
    ,("Multiplicative Identity", "x*1", "x")
    ,("Additive Identity", "x+0", "x")
    ,("Additive Identity", "0+x", "x")
    ,("Subtract Zero", "x-0", "x")
    ,("One exponent",  "x^1", "x")
    ,("Distributive", "(x*y)+(z*y)", "(x+z)*y")
    ,("Additive Property", "x+x", "2*x")
    ,("Multiplicative Identity", "x*(1/y)", "(x/y)")
    ,("Divide by Self", "x/x", "1")
    ,("Multiplicative Inverse", "(1/x)*x", "1")
    ,("Combine Powers", "(x^p)*(const/x)", "(const * (x^(p-1)))")
    ,("Combine Powers", "(x^p)*(const*(x^a))", "const*(x^(a+p))")
    ,("Neg Derivative", "d/dvar(-x)", "-(d/dvar(x))")
    ,("Sin Derivative", "d/dvar(sin(x))", "sin(d/dvar(x))*cos(x)")
    ,("Cos Derivative", "d/dvar(cos(x))", "(-(sin(x))*(d/dvar(x)))")
    ,("Ln Derivative", "d/dvar(ln(x))", "((1.0/x)*(d/dvar(x)))")
    ,("Additive Derivative", "d/dvar(a+b)", "((d/dvar(a))+(d/dvar(b)))")
    ,("Power Derivative", "d/dvar(x^y)", "((x^y)*(d/dvar((y*ln(x)))))")
    ,("Multiplicative Derivative", "d/dvar(x*y)", "(((d/dvar(x))*y)+(x*(d/dvar(y))))")
    ,("Division Derivative", "d/dvar(x/y)", "((((d/dvar(x))*y)-(x*(d/dvar(y))))/(y^2.0))")
    ,("Constant Derivative", "d/dvar(const)", "0")
    ,("Self Derivative", "d/dvar(var)", "1")
    ]

laws :: [Law]
laws = parseLaws stringLaws
lawsHardcode = [
        -- general
        Law "times zero" (BinOp "Mult" (Con 0) (Var "x")) (Con 0)
    ,   Law "times zero" (BinOp "Mult" (Var "x") (Con 0)) (Con 0)
    ,   Law "identity" (BinOp "Mult" (Con 1) (Var "x")) (Var "x")
    ,   Law "identity" (BinOp "Mult" (Var "x") (Con 1)) (Var "x")
    ,   Law "identity" (BinOp "Sum" (Var "x") (Con 0)) (Var "x")
    ,   Law "identity" (BinOp "Sum" (Con 0) (Var "x")) (Var "x")
    ,   Law "identity" (BinOp "Min" (Var "x") (Con 0)) (Var "x")
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
    ,   Law "neg" (BinOp "derive" (Var "var") (UnOp "Neg" (Var "x"))) (UnOp "Neg" (BinOp "derive" (Var "var") (Var "x")))
    ,   Law "sin" (BinOp "derive" (Var "var") (UnOp "sin" (Var "x"))) (BinOp "Mult" (UnOp "cos" (Var "x")) (BinOp "derive" (Var "var") (Var "x")))
    ,   Law "cos" (BinOp "derive" (Var "var") (UnOp "cos" (Var "x"))) (BinOp "Mult" (UnOp "Neg" (UnOp "sin" (Var "x"))) (BinOp "derive" (Var "var") (Var "x")))
    ,   Law "ln" (BinOp "derive" (Var "var") (UnOp "ln" (Var "x"))) (BinOp "Mult" (BinOp "Div" (Con 1) (Var "x")) (BinOp "derive" (Var "var") (Var "x")))
    ,   Law "addition" (BinOp "derive" (Var "var") (BinOp "Sum" (Var "a") (Var "b"))) (BinOp "Sum" (BinOp "derive" (Var "var") (Var "a")) (BinOp "derive" (Var "var") (Var "b")))
    ,   Law "power" (BinOp "derive" (Var "var") (BinOp "Pow" (Var "x") (Var "y"))) (BinOp "Mult" (BinOp "Pow" (Var "x") (Var "y")) (BinOp "derive" (Var "var") (BinOp "Mult" (Var "y") (UnOp "ln" (Var "x")))))
    ,   Law "multiplication" (BinOp "derive" (Var "var") (BinOp "Mult" (Var "a") (Var "b"))) (BinOp "Sum" (BinOp "Mult" (BinOp "derive" (Var "var") (Var "a")) (Var "b")) (BinOp "Mult" (Var "a") (BinOp "derive" (Var "var") (Var "b"))))
    ,   Law "division" (BinOp "derive" (Var "var") (BinOp "Div" (Var "a") (Var "b"))) (BinOp "Div" (BinOp "Min" (BinOp "Mult" (BinOp "derive" (Var "var") (Var "a")) (Var "b")) (BinOp "Mult" (Var "a") (BinOp "derive" (Var "var") (Var "b")))) (BinOp "Pow" (Var "b") (Con 2)))
    ,   Law "const" (BinOp "derive" (Var "var") (Var "const")) (Con 0)
    ,   Law "self" (BinOp "derive" (Var "var") (Var "var")) (Con 1)
        ]