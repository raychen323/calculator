module Laws where
import DataTypes
import Parse
import Text.Megaparsec

--Parses lawss from string form into law data type
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

-- A list of our laws, arranged in priority
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
    ,("Subtractive Derivative", "d/dvar(a-b)", "((d/dvar(a))-(d/dvar(b)))")
    ,("Sin Derivative", "d/dvar(sin(x))", "(d/dvar(x))*cos(x)")
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