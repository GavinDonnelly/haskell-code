-----------------------------------------------------------------------
 --
 -- Filename: PropositionalLogic.hs
 --
 -- Synopsis: Propositional Logic
 -- 
 -- Author: Gavin Donnelly
 --
 -- Version: See VERSION below
 --
 -----------------------------------------------------------------------
 
 ----------------------------REVISION HISTORY:--------------------------
 -- 
 -- v1.O  14/11/2013: Initial version. 
 -- v2.0  28/11/2013: Finished, for detailed changes see commits on 
 --                   StrathLab Repo, functional programming, Gavin Donnely
-----------------------------------------------------------------------

type Variable  = String
type Valuation = [(Variable, Bool)] -- Valuation of variables to truth values 

-- A (propositional) formula is
data Prop = Falsum         -- a contradiction, or
          | Var Variable   -- a variable, or
          | Not Prop       -- a negation of a formula, or
          | Or  Prop Prop  -- a disjunction of two formulae, or
          | And Prop Prop  -- a conjunction of two formulae, or
          | Imp Prop Prop  -- a conditional of two formulae.
            deriving (Eq, Show)

-- A running example
p = Var "p"
q = Var "q"
example = And p (Or q (Not q))

-- an auxiliary function that removes duplicates from
-- a given list. E.g., nub [2,5,2,1,2,5] == [2,5,1]

nub :: Eq a => [a] -> [a]
nub = nub' []
  where
    nub' _    []        = []
    nub' seen (x:xs)
        | x `elem` seen = nub' seen xs
        | otherwise     = x : nub' (x:seen) xs

vars :: Prop -> [Variable]
vars = nub . vars'
    where
      vars' Falsum    = []
      vars' (Var v)   = [v]
      vars' (Not f)   = vars' f
      vars' (Or  f g) = vars' f ++ vars' g
      vars' (And f g) = vars' f ++ vars' g
      vars' (Imp f g) = vars' f ++ vars' g

eval :: Valuation -> Prop -> Bool
eval val Falsum    = False
eval val (Var v)   = case (lookup v val) of
                       Nothing -> error ("Unbound variable: " ++ v)
                       Just t  -> t
eval val (Not f)   = not (eval val f)
eval val (Or  f g) = (eval val f) || (eval val g)
eval val (And f g) = (eval val f) && (eval val g)
eval val (Imp f g) = eval val (Or (Not f) g)

valuations :: [Variable] -> [Valuation]
valuations []     = [[]]
valuations (v:vs) = map ((v,True):) ds ++ map ((v,False):) ds 
    where ds = valuations vs

-- given a formula returns the models of that formula, where a model 
-- is a valuation which makes the formula true. E.g.,
-- models example == [[("p",True),("q",True)],[("p",True),("q",False)]]

models :: Prop -> [Valuation]
models f = [ d | d <- valuations (vars f), eval d f ]

-- A contradiction is a proposition that is always false.
contradiction :: Prop -> Bool
contradiction = null . models

--A tautology is a proposition that is always true.
tautology :: Prop -> Bool
tautology = contradiction . Not


-- a function that transforms any propositional formula to
-- its Disjunctive Normal Form (DNF): The DNF of a contradiction is
-- Falsum. A formula, that is not a contradiction, is in disjunctive
-- normal form if it is a disjunction of conjunctions where every
-- variable or its negation is represented exactly once in each
-- conjunction. The individual conjunctions that make up the DNF are
-- called minterms. For instance, the DNF of "example" is
--
-- Or (And (Var "p") (Var "q")) (And (Var "p") (Not (Var "q")))
--
-- and it has two minterms, namely (And (Var "p") (Var "q")) and
-- (And (Var "p") (Not (Var "q")))

dnf :: Prop -> Prop
dnf p = case models p of
          [] -> Falsum
          ms -> disjunction (map minterm ms)


-- given a nonempty list of formulae returns the disjunction of them, as in
-- disjunction [f0, f1, f2] == Or f0 (Or f1 f2)

disjunction :: [Prop] -> Prop
disjunction [f] = f
disjunction (f:fs) = Or f (disjunction fs)

-- Or shorter: 
-- disjunction = foldl1 Or

-- given a valuation (a row in the truth table) returns a conjunction 
-- with one term per variable, each of which is (Var v) if v is True 
-- and (Not (Var v)) if v is False. 
-- E.g., minterm [("p",True),("q",False)] == And (Var "p") (Not (Var "q"))

minterm :: Valuation -> Prop
minterm [(v, True )] = Var v
minterm [(v, False)] = Not (Var v)
minterm (b:bs) = And (minterm [b]) (minterm bs)
