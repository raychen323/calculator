module Simplify where

import DataTypes

type Equation = (Expression,Expression)

-- calculate :: [Law] -> Expression -> Calculation 
-- calculate laws e = Calc e (manyStep rws e)   
--     where rws e = [Step name f | Law name eq <- laws, f <- rewrites eqn e] 

-- manyStep :: (Expression -> [Step]) -> Expression -> [Step]
-- manyStep rws e  
--  = case steps of
--     [] -> []        
--     (o@(Step _ e) : _) -> o:manyStep rws e   
--  where steps = rws e 

-- rewrites :: Equation -> Expression -> [Expression] 
-- -- rewrites eqn (Compose as)
-- --    = map Compose (rewriteSeg eqn as ++ anyOne (rewritesA eqn) as) 

-- rewrites eqn (BinOp op expr1 expr2)
--     = BinOp op (rewritesSeg eqn [expr1]) (rewriteSeg eqn [expr2])
-- rewrites eqn (UnOp op expr)
--     = UnOp op (rewriteSeg eqn [expr])
-- rewrites eqn expr
--     = expr


-- rewritesA eqn (Var v) = [] 
-- rewritesA eqn (Con k es) 
--   = map (Con k) (anyOne (rewrites eqn) es)

-- rewritesSeg :: Equation -> [Expression] -> [[Expression]]
-- rewritesSeg (e1,e2) as  
--  = [as1 ++ deCompose (apply subst e2) ++ as3      
--    | (as1,as2,as3) <- split3 as      
--    , subst <- match (e1, Compose as2) ] 


-- matchA :: (Expression, Expression) -> [Subst] 
-- matchA (Var v, e) = [unitSub v e] 
-- matchA (Con k1 es1, Compose [Con k2 es2]) | k1 ==k2 
--   = combine (map match (zip es1 es2))

match :: Expr -> Expr -> [Subst]
match (Var x) y = [unitSub x y]
match (Con _)@x y = []
match (BinOp oper expr1 expr2) (BinOp oper' expr1' expr2') = if oper == oper' then
    let match1 = match expr1 expr1'
        match2 = match expr2 expr2'
        return [ unitSub x y | x<-[a,b], y<-[c,d] ]
    else []
match (UnOp oper expr) (UnOp oper' expr') = if oper == oper' then
    return (match expr expr') else []
                                  

type Subst = [(VarName,Expression)]



apply :: Subst -> Expression -> Expression
apply sub (Con _)@x = x
apply sub (Var v) = binding sub v
apply sub (BinOp oper expr1 expr2) = BinOp oper (apply sub expr1) (apply sub expr2)
apply sub (UnOp oper expr1) = UnOp oper (apply sub expr1)



type VarName = String 
unitSub :: VarName -> Expression -> Subst 
unitSub v e = [(v,e)]

-- apply :: Subst -> Expression -> Expression
-- apply sub (Compose as) = Compose (concatMap (applyA sub) as) 

-- applyA :: Subst -> Expression -> [Expression] 
-- applyA sub (Var v) = deCompose (binding sub v) 
-- applyA sub (Con k es) = [Con k (map (apply sub) es)] 

binding :: Subst -> VarName -> Expression 
binding ((v',e):sub) v | v' == v = e                                  
    | otherwise = binding sub v 
binding [] v = error "Could not find binding"

-- combine :: [[Subst]] -> [Subst]
-- combine = filterUnifiable . cp b
-- filterUnifiable = concatMap unifyAll 
-- unifyAll :: [Subst] -> [Subst] 
-- unifyAll = foldr f []   
--     where f sub subs = concatMap (unify sub) subs

-- unify :: Subst -> Subst -> [Subst] 
-- unify s1 s2 = if compatible s1 s2 then [s1 ++ s2] else [] 

-- compatible :: Subst -> Subst -> Bool 
-- compatible sub1 sub2   
--     = and [e1 == e2 | (v1, e1) <- sub1, (v2, e2) <-sub, v1==v2]