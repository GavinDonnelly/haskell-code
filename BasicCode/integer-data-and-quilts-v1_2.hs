-----------------------------------------------------------------------
 --
 -- Filename: integer-data-and-quilts-v1_2.hs
 --
 -- Synopsis: Programming Haskell with Integers and Data Sets (and Quilts)
 --
 -- Some basic concepts with when dealing with integers in 
 -- Haskell and then some common functions and some other functional
 -- programing things
 -- 
 -- Author: Gavin Donnelly
 --
 -- Version: See VERSION below
 --
 -----------------------------------------------------------------------
 
 ----------------------------REVISION HISTORY:--------------------------
 -- 
 -- v1.O  03/09/2013: Initial version. 
 -- v1.1  03/09/2013: Lots of basic functions.
 -- v1.2  05/09/2013: Messing about with quilts and patterns
 -----------------------------------------------------------------------

 ----------------------------TO DO:-------------------------------------
 --
------------------------------------------------------------------------         

module IntegerDataQuilts where

import Quilt

-- addup using recursion
addup :: Int -> Int
addup 0 = 0
addup n = n+addup(n-1)

-- max of 3 ints
max3 :: (Int, Int, Int) -> Int
max3 (x,y,z) 
  | (x >= y) && (x >= z) = x
  | y >= z = y
  | y<z = z

--copy using recursion
copy :: Int -> a -> [a]
copy 0 a = []
copy n a = a:copy(n-1) a

-- copy using list comprehension and not recursion
copy2 :: Int -> a -> [a]
copy2 n a = [ a | x<-[1..n] ]

-- add one to every element in a list using list recuriosn
addone :: [Int] -> [Int]
addone [] = []
addone xs = (head xs + 1):addone (tail xs)

-- the same using map
addone2 :: [Int] -> [Int]
addone2 xs = map plus1 xs
      where plus1 x = x+1 

-- times in by 2
times2 :: Int -> Int
times2 x = 2*x

-- less than 10
lessThanTen :: Int -> Int
lessThanTen x = if( x<10) then 1
        else 0

-- Adds the results of function f up to n, so if n is 4 it adds the results from
-- f 1 to f 4
add :: (Int -> Int) -> Int -> Int
add f 0 = f 0
add f n = (f n) + add f (n-1)

-- checks if the results of function f up to n contains equal elements
allEqual :: (Int -> Int) -> Int -> Bool
allEqual f 0 = True
allEqual f n = if (f n == f (n-1)) then allEqual f (n-1)
        else False

fold :: (a -> b -> a) -> a -> [b] -> a
fold f a [] = a
fold f a (x:xs) = fold f (f a x) xs

-- uses fold to calculate the n factorial, eg if n is 5 = (5*4*3*2*1)
fac :: Int -> Int
fac n = fold (*) 1 [1..n]

-- uses fold to calculate the max integer in a list
maxList :: [Int] -> Int
maxList xs = fold max (head xs) (tail xs)

-- uses fold to convert a list of ints into the number they represent 
-- in the order of the list
numeral :: [Int] -> Int
numeral xs = fold func1 0 xs
        where func1 i j = 10*i +j
 
-- converts an integer into a list of its digits
numeral2 :: Int -> [Int]
numeral2 0 = []
numeral2 x = numeral2 (x `div` 10) ++ [x `mod` 10]

-- Examples of quilts
eg :: Int -> Quilt

eg 1 = plain 10 9
eg 2 = fancy '%' '@' 10
eg 3 = sewV (eg 2) (eg 1)
eg 4 = rotate (eg 2)
eg 5 = (rotate . rotate) (eg 2)
eg 6 = rotate (rotate (eg 2))
eg _ = error "No such example"

-- for any n gives an n × n quilt with the pattern:
-- .-----
-- $.----
-- $$.---
-- $$$.--
-- $$$$.-
-- $$$$$.

triangle :: Int -> Quilt
triangle = fancy '$' '-'

-- n is the 2n × n pattern formed by sewing two triangles
-- together:
-- -----..-----
-- ----.$$.----
-- ---.$$$$.---
-- --.$$$$$$.--
-- -.$$$$$$$$.-
-- .$$$$$$$$$$.

pile :: Int -> Quilt
pile n = sewH (flipH tri) tri
  where
    tri = triangle n

stripe :: Int -> Quilt
stripe n = sewH q (rotate (rotate q))
  where
    q = fancy '.' '$' n

--sewing piles onto a plain square.

--#######################################
--#######################################
--##----..--------..--------..----.....##
--##---.$$.------.$$.------.$$.---.....##
--##--.$$$$.----.$$$$.----.$$$$.--.....##
--##-.$$$$$$.--.$$$$$$.--.$$$$$$.-.....##
--##.$$$$$$$$..$$$$$$$$..$$$$$$$$......##
--##.....----..--------..--------..----##
--##.....---.$$.------.$$.------.$$.---##
--##.....--.$$$$.----.$$$$.----.$$$$.--##
--##.....-.$$$$$$.--.$$$$$$.--.$$$$$$.-##
--##......$$$$$$$$..$$$$$$$$..$$$$$$$$.##
--#######################################
--####################################### 

thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))

moneypiles :: Quilt
moneypiles = (border '#' . border '#') $ sewV money (flipH money)
  where
    money = thrice (sewH (pile 5)) (plain 5 5)

-- foursome, for a square quilt of size n will produce the square 
-- quilt of size 2n formed by successively rotating the pattern 
-- through 90 degrees

foursome :: Quilt -> Quilt
foursome q = p `sewV` (rotate (rotate p))
  where
    p = q `sewH` (rotate q)

--.---------$$$$$$$$$.
--$.--------$$$$$$$$.-
--$$.-------$$$$$$$.--
--$$$.------$$$$$$.---
--$$$$.-----$$$$$.----
--$$$$$.----$$$$.-----
--$$$$$$.---$$$.------
--$$$$$$$.--$$.-------
--$$$$$$$$.-$.--------
--$$$$$$$$$..---------
-----------..$$$$$$$$$
----------.$-.$$$$$$$$
---------.$$--.$$$$$$$
--------.$$$---.$$$$$$
-------.$$$$----.$$$$$
------.$$$$$-----.$$$$
-----.$$$$$$------.$$$
----.$$$$$$$-------.$$
---.$$$$$$$$--------.$
--.$$$$$$$$$---------.

windmill :: Quilt
windmill = foursome $ triangle 9

-----------..---------
----------.$$.--------
---------.$$$$.-------
--------.$$$$$$.------
-------.$$$$$$$$.-----
------.$$$$$$$$$$.----
-----.$$$$$$$$$$$$.---
----.$$$$$$$$$$$$$$.--
---.$$$$$$$$$$$$$$$$.-
--.$$$$$$$$$$$$$$$$$$.
--.$$$$$$$$$$$$$$$$$$.
---.$$$$$$$$$$$$$$$$.-
----.$$$$$$$$$$$$$$.--
-----.$$$$$$$$$$$$.---
------.$$$$$$$$$$.----
-------.$$$$$$$$.-----
--------.$$$$$$.------
---------.$$$$.-------
----------.$$.--------
-----------..---------

diamond :: Quilt
diamond = foursome . flipH $ triangle 9

--turns any rectangular quilt into a square one by sewing on a plain 
-- patch of the appropriate size

-- if width of quilt is less than its height, place the patch on the left
-- if width of quilt is greater than its height, place the patch on top
squareup :: Quilt -> Quilt
squareup q
    | w < h     = sewH (plain (h - w) h) q
    | w > h     = sewV (plain w (w - h)) q
    | otherwise = q
    where
      w = width q
      h = height q

flower :: Int -> Quilt
flower n = foursome (squareup (stripe n))
