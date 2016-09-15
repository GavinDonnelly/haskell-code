-----------------------------------------------------------------------
 --
 -- Filename: ImperativeLanguage.hs
 --
 -- Synopsis: An Imperative Language
 -- 
 -- Author: Gavin Donnelly
 --
 -- Version: See VERSION below
 --
 -----------------------------------------------------------------------
 
 ----------------------------REVISION HISTORY:--------------------------
 -- 
 -- v1.O  05/11/2013: Initial version. 
 -- v2.0  12/11/2013: Finished, for detailed changes see commits on 
 --                   StrathLab Repo, functional programming, Gavin Donnely
-------------------------------------------------------------------------

import Prelude hiding (lookup)

data AExpr = Num Int
           | Add AExpr AExpr
           | Sub AExpr AExpr
           | Var String
            deriving (Show, Eq)

data BExpr = Boo Bool
           | LEq AExpr AExpr
           | GEq AExpr AExpr
           | Not BExpr
           | And BExpr BExpr 
          deriving (Show, Eq)

data Command = While BExpr Command
             | Assign String AExpr
             | If BExpr Command Command
             | Seq Command Command
             | Inc String
             | Pass
            deriving (Show, Eq)

type Memory = [(String, Int)]

lookup :: Memory -> String -> Maybe Int
lookup []         _ = Nothing
lookup ((s,n):ms) t = if s == t then Just n else lookup ms t

update :: Memory -> (String, Int) -> Memory
update [] (t,k) = [(t,k)]
update ((s,n):ms) (t,k)
    | s == t    = (s,k) : ms
    | otherwise = (s,n) : update ms (t,k)

aEval :: Memory -> AExpr -> Int
aEval _ (Num n)      = n
aEval m (Add e1 e2)  = aEval m e1 + aEval m e2
aEval m (Sub e1 e2)  = aEval m e1 - aEval m e2
aEval m (Var x)      = case lookup m x of
                         Just v  -> v
                         Nothing -> error $ "Unbound variable: " ++ show x

bEval :: Memory -> BExpr -> Bool
bEval _ (Boo b)      = b
bEval m (LEq e1 e2)  = (aEval m e1)  <=  (aEval m e2)
bEval m (GEq e1 e2)  = (aEval m e1)  >=  (aEval m e2)
bEval m (Not e)      = not $ bEval m e
bEval m (And e1 e2)  = bEval m e1 && bEval m e2

exec :: Memory -> Command -> Memory
exec m (While e c)  = if bEval m e
                      then exec (exec m c) (While e c)
                      else m
exec m (Assign s e) = update m (s, aEval m e)
exec m (If e c1 c2) = if bEval m e
                      then exec m c1
                      else exec m c2
exec m (Seq c1 c2)  = let m' = exec m c1 in exec m' c2
exec m (Inc s)      = update m (s, 1 + aEval m (Var s))
exec m Pass         = m


prog :: [Command] -> Command
prog = foldl1 Seq

run :: Command -> Memory
run = exec []

equal :: AExpr -> AExpr -> BExpr
equal e1 e2 = (e1 `LEq` e2) `And` (e1 `GEq` e2)

notEqual :: AExpr -> AExpr -> BExpr
notEqual e1 e2 = Not (equal e1 e2)

-- Implementionation of:
--
--    a := 1071
--    b := 462
--    while a /= b {
--       if a >= b
--          a := a - b
--       else
--          b := b - a
--    }

euc = let a = Var "a"
         b = Var "b"
     in prog [ Assign "a" (Num 1071)
             , Assign "b" (Num 462)
             , While (a `notEqual` b) $
                     If (a `GEq` b)
                            (Assign "a" (a `Sub` b))
                            (Assign "b" (b `Sub` a))
             ]
-- The result of running euc is 21 = gcd(1071, 462). This is because euc
-- implements the Euclidean algorithm

-- for-loop command
-- pseudo-code:
--
--    for s := e1 to e2 {
--      c
--    }

for :: String -> AExpr -> AExpr -> Command -> Command
for s e1 e2 c = Assign s e1 `Seq`
                While (Var s `LEq` e2) (c `Seq` Inc s)

-- Implementionation of:
--
--    n := 5
--    nsqr := 0
--    for i := 1 to n {
--       nsqr := nsqr + n
--    }

sqr = prog [
        Assign "n" (Num 5)
      , Assign "nsqr" (Num 0)
      , for "i" (Num 1) (Var "n") $
                 Assign "nsqr" $ Add (Var "nsqr") (Var "n")
      ]

-- The pseudo-code for the expanded version of sqr is
--
--    n := 5
--    nsqr := 0
--    i := 0
--    while i <= n {
--       nsqr := nsqr + n
--       i++
--    }
