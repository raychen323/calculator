module Simplify where

import DataTypes

type Equation = (Expression,Expression)

calculate :: [Law] -> Expression -> Calculation 
calculate laws e = Calc e (manyStep rws e)   
    where rws e = [Step name f | Law name expr1 expr2 <- laws, f <- rewrites (expr1, expr2) e] 

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



rewritesHelper (e1, e2) exp = [apply sub e2 | sub <- match e1 exp]


crossProduct :: [Subst] -> [Subst] -> [Subst]
crossProduct [] _ = []
crossProduct (x:xs) ys = crossProductHelper x ys ++ crossProduct xs ys

crossProductHelper x [] = []
crossProductHelper x (y:ys) = [x++y] ++ crossProductHelper x ys

match :: Expression -> Expression -> [Subst]
match (Var x) y = [unitSub x y]
match x@(Con _) y = []
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