module Simplify where

import DataTypes

type Equation = (Expression,Expression)

calculate :: [Law] -> Expression -> Calculation 
calculate laws e = Calc e (manyStep rws e)   
    where rws e = (constStep e) ++ [Step name f | Law name expr1 expr2 <- laws, f <- rewrites (expr1, expr2) e] 


constRules :: Expression -> Expression
constRules (BinOp "Sum" (Con x) (Con y)) = Con (x+y)
constRules (BinOp "Min" (Con x) (Con y)) = Con (x-y)
constRules (BinOp "Mult" (Con x) (Con y)) = Con (x*y)
constRules (BinOp "Div" (Con x) (Con y)) = Con (x-y)
constRules (BinOp "Pow" (Con x) (Con y)) = Con (x^y)
constRules (BinOp oper x y) = BinOp oper (constRules x) (constRules y)
constRules (UnOp oper x) = UnOp oper (constRules x)
constRules x = x

constStep e = [Step "const operation" f | f <- (next constRules e)]
    where next constRules e | (e == (constRules e)) = []
                            | otherwise = [constRules e]

manyStep :: (Expression -> [Step]) -> Expression -> [Step]
manyStep rws e  
 = case steps of
    [] -> []        
    (o@(Step _ e) : _) -> o:manyStep rws e   
 where steps = rws e 

rewrites :: Equation -> Expression -> [Expression]
rewrites eqn (BinOp oper expr1 expr2) = (rewritesHelper eqn (BinOp oper expr1 expr2))  ++ [BinOp oper a' expr2 | a' <- rewrites eqn expr1] ++ [BinOp oper expr1 b' | b' <- rewrites eqn expr2]

rewrites eqn (UnOp oper expr) = (rewritesHelper eqn (UnOp oper expr)) ++ [UnOp oper a' | a' <- (rewrites eqn expr)]

rewrites eqn x = rewritesHelper eqn x



rewritesHelper (e1, e2) exp = [apply sub e2 | sub <- prune(match e1 exp)]


crossProduct :: [Subst] -> [Subst] -> [Subst]
crossProduct [] _ = []
crossProduct (x:xs) ys = crossProductHelper x ys ++ crossProduct xs ys

crossProductHelper x [] = []
crossProductHelper x (y:ys) = [x++y] ++ crossProductHelper x ys

prune :: [Subst] -> [Subst]
prune [] = []
prune(x:xs) = (pruneHelper x []) ++ prune(xs)

pruneHelper :: Subst -> Subst -> [Subst]
pruneHelper [] _ = []
pruneHelper (y:[]) rol = [rol ++ [y]]
pruneHelper (y:ys) rol | (pruneHelperHelper y ys) = (pruneHelper ys (rol++[y]))
                       | otherwise = []

pruneHelperHelper :: (String , Expression) -> (Subst) -> Bool
pruneHelperHelper _ [] = True
pruneHelperHelper (var1,exp1) ((var2,exp2):ys) = if (var1 == var2) then 
                                                    if (exp1 == exp2) 
                                                        then (True && pruneHelperHelper (var2,exp2) ys)
                                                            else False
                                                else (True && pruneHelperHelper (var2,exp2) ys)


match :: Expression -> Expression -> [Subst]
match (Var "const") (Con a) = [unitSub "doNotUse" (Con a)]
match (Var "const") _ = []
match (Var x) (Var y) | x==y = [unitSub "x" (Var x)]
                      | otherwise = [] 
match (Var x) y = [unitSub x y]
match (Con a) (Con b) = if a == b then
        [unitSub "doNotUse" (Con a)]
    else []
match (BinOp oper expr1 expr2) (BinOp oper' expr1' expr2') = if oper == oper' then
    let match1 = match expr1 expr1'
        match2 = match expr2 expr2'
        in crossProduct match1 match2
    else []
match (UnOp oper expr) (UnOp oper' expr') = if oper == oper' then
    match expr expr' else []
match _ _ = []                       

type Subst = [(VarName,Expression)]



apply :: Subst -> Expression -> Expression
apply sub x@(Con _) = x
apply sub (Var v) = binding sub v
apply sub (BinOp oper expr1 expr2) = BinOp oper (apply sub expr1) (apply sub expr2)
apply sub (UnOp oper expr1) = UnOp oper (apply sub expr1)



type VarName = String 
unitSub :: VarName -> Expression -> Subst 
unitSub v e = [(v,e)]

binding :: Subst -> VarName -> Expression 
binding ((v',e):sub) v | v' == v = e                                  
    | otherwise = binding sub v 
binding [] v = error "Could not find binding"

combine :: [[Subst]] -> [Subst]
combine = filterUnifiable . cp

cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

filterUnifiable = concatMap unifyAll 
unifyAll :: [Subst] -> [Subst] 
unifyAll = foldr f []   
    where f sub subs = concatMap (unify sub) subs

unify :: Subst -> Subst -> [Subst] 
unify s1 s2 = if compatible s1 s2 then [s1 ++ s2] else [] 

compatible :: Subst -> Subst -> Bool 
compatible sub1 sub2   
    = and [e1 == e2 | (v1, e1) <- sub1, (v2, e2) <-sub2, v1==v2]