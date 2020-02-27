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

-- rewritesA eqn (Var v) = [] 
-- rewritesA eqn (Con k es) 
--   = map (Con k) (anyOne (rewrites eqn) es)

-- rewritesSeg :: Equation -> [Expression] -> [[Expression]]
-- rewritesSeg (e1,e2) as  
--  = [as1 ++ deCompose (apply subst e2) ++ as3      
--    | (as1,as2,as3) <- split3 as      
--    , subst <- match (e1, Compose as2) ]



rewrites :: Equation -> Expression -> [Expression]
rewrites eqn (BinOp oper expr1 expr2) = [BinOp oper a' b | a' <- rewrites eqn expr1] ++ [BinOp oper a b' | b' <- rewrites eqn expr2]

rewrites eqn (UnOp oper expr) = [UnOp oper (rewrites eqn expr1)]

rewrites eqn (Var v) = match apply v


match :: Expression -> Expression -> [Subst]
match (Var x) y = [unitSub x y]
match x@(Con _) y = []
match (BinOp oper expr1 expr2) (BinOp oper' expr1' expr2') = if oper == oper' then
    let match1 = match expr1 expr1'
        match2 = match expr2 expr2'
        in match1 ++ match2
    else []
match (UnOp oper expr) (UnOp oper' expr') = if oper == oper' then
    match expr expr' else []
                                  

type Subst = [(VarName,Expression)]



apply :: Subst -> Expression -> Expression
apply sub x@(Con _) = x
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