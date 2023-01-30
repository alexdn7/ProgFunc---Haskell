data Expr = Const Int 
                    | Expr :+: Expr
                    | Expr :*: Expr 
                    deriving Eq

data Operation = Add | Mult deriving (Eq, Show)
data Tree = Lf Int 
                | Node Operation Tree Tree 
                deriving (Eq, Show)


-- 1.1. Să se instantieze clasa Show pentru tipul de date Expr, astfel încât să se afiseze mai simplu expresiile.

instance Show Expr where
    show (Const x) = show x
    show (val1 :+: val2) = "(" ++ show val1 ++ " + " ++ show val2 ++ ")"
    show (val1 :*: val2) = "(" ++ show val1 ++ " * " ++ show val2 ++ ")"


    -- 1.2. Să se scrie o functie evalExp :: Expr -> Int care evaluează o expresie determinând valoarea acesteia.
    
evalExp :: Expr -> Int
evalExp (Const x)  =  x
evalExp (val1 :+: val2) = evalExp val1 + evalExp val2
evalExp (val1 :*: val2) = evalExp val1 * evalExp val2

    
--    Exemplu:

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3)) 
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)

test11 = evalExp exp1 -- 6
test12 = evalExp exp2 -- 14
test13 = evalExp exp3 -- 13
test14 = evalExp exp4 --  16


-- 1.3. Să se scrie o functie evalArb :: Tree -> Int care evaluează o expresie modelată sub formă de arbore, determinând valoarea acesteia.

evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add val1 val2) = evalArb val1 + evalArb val2
evalArb (Node Mult val1 val2) = evalArb val1 * evalArb val2

--Exemple

arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

test21 = evalArb arb1 == 6
test22 = evalArb arb2 == 14
test23 = evalArb arb3 == 13
test24 = evalArb arb4 == 16


--1.4. Să se scrie o functie expToArb :: Expr -> Tree care transformă o expresie în arborele corespunzător.

expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (val1 :+: val2) = Node Add (expToArb val1) (expToArb val2)
expToArb (val1 :*: val2) = Node Mult (expToArb val1) (expToArb val2)


-- va urma
