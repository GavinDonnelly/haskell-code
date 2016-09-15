-----------------------------------------------------------------------
 --
 -- Filename: sorting-v1_1.hs
 --
 -- Synopsis: Programming Haskell with Sort functions
 --
 -- Some basic concepts with sorting, this is not unquie to Haskell
 -- or even functional programing these are common program desgins.
 --
 -- Using the example of marks from a test to implement sorting.
 -- 
 -- Author: Gavin Donnelly
 --
 -- Version: See VERSION below
 --
 -----------------------------------------------------------------------
 
 ----------------------------REVISION HISTORY:--------------------------
 -- 
 -- v1.O  08/09/2013: Initial version. 
 -- v1.1  10/09/2013: Done.
 -----------------------------------------------------------------------

 ----------------------------TO DO:-------------------------------------
 --
------------------------------------------------------------------------         

-- insertion sort algorithm

-- Takes an element and a list and places that element in the list in the right order given by
-- function f
insertBy :: Ord b => (a -> b) -> a -> [a] -> [a] 
insertBy f y [] = [y]
insertBy f y (x:xs) 
      | f x < f y = x : insertBy f y xs
      | f x >= f y = y: x:xs    

-- uses insertBy recursively to sort a list by the order given by f       
inssortBy :: Ord b => (a -> b) -> [a] -> [a] 
inssortBy f [] = []
inssortBy f (x:xs) = insertBy f x (inssortBy f xs)

ident :: Int -> Int
ident x = x
type Result = (Mark,Mark,Mark)
type Mark = Int

-- sorts results by the highest total of marks
sortTotal :: [Result] -> [Result]
sortTotal xs = inssortBy tot xs
      where tot (m1,m2,m3) = -(m1+m2+m3)

-- sorts results by the highest marks of module i             
sortbyModule :: Int -> [Result] -> [Result]
sortbyModule i xs = inssortBy ind xs
          where ind (m1,m2,m3) 
            | i == 1 = -m1
            | i == 2 = -m2
            | i == 3 = -m3
            | otherwise = error ("Number too High, only 3 Modules")
             
-- given a result calculates if they have failed or not 
fl :: Result -> Bool
fl (m1,m2,m3) 
      | (m1 >= 40 && m2 >= 40 && m3 >= 40) = False
      | otherwise = True
      
-- sorts results by highest total pass to lowest total fail
sortCut :: [Result] -> [Result]
sortCut xs = inssortBy crit xs
      where crit (m1,m2,m3)
          = pOrf(fl (m1,m2,m3),(m1,m2,m3));

-- decides ordering used by sortCut         
pOrf :: (Bool,Result) -> Int
pOrf (b,(m1,m2,m3)) 
    | b == False = -(m1+m2+m3)
        | b == True = (300-(m1+m2+m3))
     
            
g500 :: [Result]
g500 = [(21,34,56), (43,75,24), (36,95,67),
        (68,23,98), (45,19,46), (68,93,43),
        (16,37,54), (58,96,45), (45,78,67),
        (40,40,40), (37,94,22), (75,42,48),
        (87,56,75), (56,36,64), (46,89,45),
        (34,89,90), (65,75,32), (48,67,67)]