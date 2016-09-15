--------------------------------------------------------------------
-- File: Quilt.hs
-- Time-stamp: <98/07/14 12:00:29 rlc3>                             
-- Created: 05/08/97                                                
-- Author: Roy L. Crole, University of Leicester, UK                          
-- Contents: Simple quilt processing functions
--------------------------------------------------------------------           

module Quilt where

data Quilt = Qt [String]

instance Show Quilt where
    showsPrec _ (Qt []) = ('\n':)
    showsPrec i (Qt (l:ls)) = ('\n':) . (l++) . showsPrec i (Qt ls)

plain :: Int -> Int -> Quilt
plain n m = Qt [[ '.' | i <- [1..n] ] | j<-[1..m]]

fancy :: Char -> Char -> Int -> Quilt
fancy c1 c2 n = Qt [(copy c1 (j-1))++['.']++(copy c2 (n-j)) | j<-[1..n] ]

flipV :: Quilt -> Quilt
flipV (Qt q) = Qt (reverse q)

flipH :: Quilt -> Quilt
flipH (Qt q) = Qt (map reverse q)

sewH :: Quilt -> Quilt -> Quilt
sewH (Qt p) (Qt q)
    | height (Qt p) == height (Qt q) = Qt (zipWith (++) p q)
    | otherwise = error "cannot join quilts of different height"

sewV :: Quilt -> Quilt -> Quilt
sewV (Qt p) (Qt q)
    | width (Qt p) == width (Qt q) = Qt (p++q)
    | otherwise = error "cannot join quilts of different width"

width :: Quilt -> Int
width  (Qt p) = length (head p)

height :: Quilt -> Int
height (Qt p) = length p

border :: Char -> Quilt -> Quilt
border c (Qt q) = Qt ([cc]++(map wrap q)++[cc])
  where
    cc = copy c (width (Qt q) + 2)
    wrap l = [c] ++ l ++ [c]

rotate :: Quilt -> Quilt
rotate (Qt q) = Qt (foldr (zipWith (:)) (copy [] (width (Qt q))) (reverse q))

copy :: a -> Int -> [a]
copy c n = [ c | i <- [1..n] ]

rows :: Quilt -> [String]
rows (Qt ls) = ls
