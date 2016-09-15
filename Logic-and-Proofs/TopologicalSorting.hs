-----------------------------------------------------------------------
 --
 -- Filename: TopologicalSorting.hs
 --
 -- Synopsis: Topological sorting
 -- 
 -- Author: Gavin Donnelly
 --
 -- Version: See VERSION below
 --
 -----------------------------------------------------------------------
 
 ----------------------------REVISION HISTORY:--------------------------
 -- 
 -- v1.O  03/02/2014: Initial version. 
 -- v2.0  23/02/2014: Finished, for detailed changes see commits on 
 --                   StrathLab Repo, functional programming, Gavin Donnely
-----------------------------------------------------------------------

type DiGraph a = [(a, [a])]

g :: DiGraph Int
g = [ (2,  [11])
    , (3,  [])
    , (5,  [])
    , (7,  [])
    , (8,  [3, 7])
    , (9,  [8, 11])
    , (10, [3, 11])
    , (11, [5, 7])
    ]

g' :: DiGraph Int
g' = [ (2,  [11])
     , (3,  [9])  -- Creates a cycle
     , (5,  [])
     , (7,  [])
     , (8,  [3, 7])
     , (9,  [8, 11])
     , (10, [3, 11])
     , (11, [5, 7])
     ]

-- given a DiGraph this will return the list of sources
sources :: DiGraph a -> [a]
sources g = [ v | (v, vs) <- g, null vs ]


-- helper which removes dulipcates from the list
nub :: Eq a => [a] -> [a]
nub = nub' []
    where
      nub' _  []        = []
      nub' xs (y:ys)
          | y `elem` xs = nub' xs ys
          | otherwise   = y : nub' (y:xs) ys

minus :: Eq a => [a] -> [a] -> [a]
xs `minus` ys = [ x | x <- xs, x `notElem` ys ]

-- sink is a node with no outgoing edges, so the sinks of g are 2,9 nd 10.
sinks :: Eq a => DiGraph a -> [a]
sinks g = vs `minus` concat vss where (vs,vss) = unzip g

-- removes a single node from a DiGraph
rm :: Eq a => a -> DiGraph a -> DiGraph a
rm v g = [ (u, us `minus` [v]) | (u,us) <- g, u /= v ]

-- reomves a list of nodes
remove :: Eq a => [a] -> DiGraph a -> DiGraph a
remove vs g = foldr rm g vs

-- returns topological sorting of a given digraph.
topsort :: Eq a => DiGraph a -> [a]
topsort [] = []
topsort g = case sources g of
              [] -> error "digraph has a cycle"
              vs -> vs ++ topsort (remove vs g)

--puesdo-code:
-- topsort :: Eq a => DiGraph a -> Maybe [a]
-- topsort [] = Just []
-- topsort g = case sources g of
--               [] -> Nothing
--               vs -> case topsort (remove vs g) of
--                       Nothing -> Nothing
--                       Just xs -> Just (vs ++ xs)

-- Real world example: 
--  A common security method used for online banking is to ask the user 
-- for three random characters from a passcode. For example, if the passcode 
-- was 531278, they may ask for the 2nd, 3rd, and 5th characters; the expected 
-- reply would be: 317.

-- if it is assumed keylog is fifty successful login attempts
keylog :: [String]
keylog = map show [
           319, 680, 180, 690, 129, 620, 762, 689, 762, 318
         , 368, 710, 720, 710, 629, 168, 160, 689, 716, 731
         , 736, 729, 316, 729, 729, 710, 769, 290, 719, 680
         , 318, 389, 162, 289, 162, 718, 729, 319, 790, 680
         , 890, 362, 319, 760, 316, 729, 380, 319, 728, 716
         ]

-- the three characters are always asked for in order, to analyse the file to 
-- determine the shortest possible secret passcode of unknown length:

nodes = nub (concat keylog)

edges = concatMap (\[x,y,z] -> [(x,y), (y,z)]) keylog

incoming v = nub [ x | (x,y) <- edges, y == v ]

keylogDAG = [ (v, incoming v) | v <- nodes ]

passcode = topsort keylogDAG
