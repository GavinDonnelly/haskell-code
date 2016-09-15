-----------------------------------------------------------------------
 --
 -- Filename: strings-and-lists-v1_4.hs
 --
 -- Synopsis: Programming Haskell with Strings and Lists
 --
 -- Some basic concepts with when dealing with strings and lists in 
 -- Haskell and then some common list functions and some other functional
 -- programing things.
 -- 
 -- For more Information about folds a complex process see:
 -- https://wiki.haskell.org/Foldr_Foldl_Foldl'
 --
 -- Author: Gavin Donnelly
 --
 -- Version: See VERSION below
 --
 -----------------------------------------------------------------------
 
 ----------------------------REVISION HISTORY:--------------------------
 -- 
 -- v1.O  07/09/2013: Initial version. 
 -- v1.1  09/09/2013: Added some really basic list uses and functions.
 -- v1.2  09/09/2013: Some more complicated list manipulations, with 
 --					  postions and subs, the idea of sparseSubs is there
 --					  final product is not there yet.
 -- v1.3  11/09/2013: finished up the code around sparseSubs along with
 --					  a filtering option.
 -- v1.4  11/09/2013: added some test data.

 ----------------------------TO DO:-------------------------------------
 --
 -- v1.4  11/09/2013: Clean up the main.
 -- v1.4  11/09/2016: Adding this to github, need to clean up a bit. 
------------------------------------------------------------------------
import Data.List

-- Some basic list exprestions 

lChar :: Int -> String
lChar 1 = take 3 testAlpha
-- outputs first n in list (Output: "abc")

lChar 2 = drop 3 testAlpha
-- outputs list with first n removed (Output: "cde")

lChar 3 = reverse (lChar 1)
-- takes in a list and outputs the reverse of it 

lChar 4 = (drop 1 (take 2 (lChar 1))) ++ reverse (lChar 2)
-- takes first two elemnets from list l1 ("ab") and drops first one("b"), 
-- then joins it to the reverse of l2("ed") = ("bed")

lChar 5 = unwords testHelloList
-- turns list of strings into one string with whitespace between each list element

lChar 6 = ['A'..'Z']
-- returns list from first to last element aka string

charPair :: Int -> (String, String)
charPair 1 = unzip (lCharPair 1)
-- puts pairs back to 2 lists.

lCharPair :: Int -> [(Char, Char)]
lCharPair 1 = zip (lChar 1) (lChar 2)
-- Returns two lists as input and pairs each corrosponding element together 

lIntCharPair :: Int -> [(Int, Char)]
lIntCharPair 1 = [(x, y) | x <- [1..5], y <- ['a'..'e']]

lStr :: Int -> [String]
lStr 1 = words testHello
-- takes string and returns list of strings with each word in string, so 
-- effectivly splits at white space

iInt :: Int -> Int
iInt 1 = head testNumList
-- Returns first item from list

iInt 2 = last testNumList
-- Returns last item from list

iInt 3 = testNumList !! 3
-- Returns item from position on int given starting at postion 0

iInt 4 = length testNumList
-- Returns length of list

iInt 5 = maximum testNumList
-- Returns max int in list

iInt 6 = minimum testNumList
-- Returns min int in list

iInt 7 = sum testNumList
-- Returns sum of all elements in list

iInt 8 = product testNumList
-- Returns product of all elements in list

iInt 9 = foldr (+) 0 testNumList
-- foldr n x, recurse through the list x applying n to each element 
-- elements together the applying z. 
--(more coplicated than this but on the surface, , but i dont want to write a big thing)
{-
foldr (+) 0 [1..5] -->
1 + (foldr (+) 0 [2..5]) -->
1 + (2 + (foldr (+) 0 [3..5])) -->
1 + (2 + (3 + (foldr (+) 0 [4..5]))) -->
1 + (2 + (3 + (4 + (foldr (+) 0 [5]))))

-}

iInt 10 = foldl (+) 0 testNumList
-- foldl n z x, recurse through the list x applying n to each element 
-- elements together then applying z, from left now. 
--(more coplicated than this but on the surface, but i dont want to write a big thing)
{-
foldl (+) 0 [1..5] -->
 
let z1 =  0 + 1
in foldl (+) z1 [2..5] -->
 
let z1 =  0 + 1
    z2 = z1 + 2
in foldl (+) z2 [3..5] -->
 
let z1 =  0 + 1
    z2 = z1 + 2
    z3 = z2 + 3
in foldl (+) z3 [4..5] -->
 
let z1 =  0 + 1
    z2 = z1 + 2
    z3 = z2 + 3
    z4 = z3 + 4
in foldl (+) z4 [5]

-}

iInt 11 = foldr1 (+) testNumList
-- same as foldr without intger input 
--(more coplicated than this but on the surface, but i dont want to write a big thing)

iInt 12 = foldl1 (+) testNumList
-- same as fold1 without intger input 
--(more coplicated than this but on the surface, but i dont want to write a big thing)

lInt :: Int -> [Int]
lInt 1 = tail testNumList
-- all of list without first item

lInt 2 = init testNumList
-- all of list without last 

lInt 3 = [1..10]
-- returns list from first to last element

lInt 4 = [2,4..20]
-- returns list from first to last element, with multiples

lInt 5 = take 10 $ cycle [1..4]
-- returns cycles on the list from the int of take 

lInt 6 = map (+1) testNumList
-- maps what is given to every element of the list

lInt 7 = filter (>3) testNumList
-- returns a list with only elements given passing the given filter

lInt 8 = replicate 10 0
-- replicate n x, returns a list of x with a size of n

lInt 9 = take 10 $ repeat 0
-- same as replicate, take n $ repeat x take a list of size n, and 
-- repeat with element x

lInt 10 = scanr (+) 0 testNumList
-- does the same as foldr but shows each step as element in list from right

lInt 11 = scanl (+) 0 testNumList
-- does the same as foldl but shows each step as element in list from left

lInt 12 = scanr1 (+) testNumList
-- what foldr1 is to foldr, scanr1 is to scanr

lInt 13 = scanl1 (+) testNumList
-- what foldl1 is to foldr, scanl1 is to scanl

lInt 14 = zipWith (+) testNumList2 testNumList3
-- zipWith x a b, takes two lists (a and b) as input and takes the pair from 
-- each list but then applys x to each pair to make one list


lBool :: Int -> Bool
lBool 1 = elem 3 testNumList
-- Returns True if there is an element in the position given
-- stating at position 1

lBool 2 = null testNumList
-- Returns True if list is null 

lBool 3 = all even testNumList
-- Returns True if the all critera is met, so here all even numbers in list

lBool 4 = any odd testNumList
-- Returns True if the any critera is met, so here any od numbers in list

------------------------------------------------------------------------

-- last char from the string as a string
end :: String -> String
end s = drop (length s - 1) s

-- remove last char from the string
rmLast :: String -> String
rmLast s = take (length s - 1) s

-- lower takes in a list of characters and uses filter in conjuction
-- with the isLower funtion (which outputs if a letter is lower case)
-- to print only the lower case characters from the input as the result

lower :: [Char] -> [Char]
lower s = filter isLower s
           where isLower x = ('a' <= x) && (x <= 'z')

-- surname uses map to go through all elements in the list
-- and if the fisrt character of the string doesn't equal a whitespace
-- it drops it the character until it finds it 

surname :: [String] -> [String]
surname xs = map surfunc xs
             where surfunc x = if(head x == ' ') 
								then drop 1 x
								else surfunc (tail x)
								
-- initials gets the first character of a string then adds a '.' then calls
-- surname on this string and adds the first character of this to get the initials. 
initials :: [String] -> [String]
initials xs = map check xs 
				where check x =  [head x]   ++ "." ++ [head (surname [x]) !! 0]

-- Above is just the basics, incase I am having a bad day and just can't
-- remember anything!!
------------------------------------------------------------------------
-- Now on to some more complex list manipulation

--max of list using list recusion 
maxList :: [Int] -> Int 
maxList [] = 0
maxList xs = max (head xs) (maxList (tail xs))
-----------------------------------------------


-- join using list recusion
join :: [[a]] -> [a]
join [] = []
join xs = head xs ++ join (tail xs)

rev1 :: [a] -> [a]
rev1 [] = []
rev1 xs = rev1(tail xs)++[head xs]

rev2 :: [a] -> [a]
rev2 xs =  rev1(drop (length xs `div`2) xs)++rev1(take (length xs `div` 2) xs) 

rev3 :: [a] -> [a] 
rev3 xs  = func xs []

func :: [a] -> [a] ->[a]
func [] ys = ys
func xs ys =  (func (tail xs) ys) ++ [(head xs)] 

infixl 9 ??

-- function that returns the 
-- position of the first occurrence of an element in a list
-- The position of an element in a list
(??) :: Eq a => [a] -> a -> Int
[]     ?? _  = error "??: No such element in list"
(x:xs) ?? y  = if x == y then 0 else 1 + xs ?? y

-- The list of positions of an element in a list
positions :: Eq a => [a] -> a -> [Int]
positions xs y = [ i | (i,x) <- zip [0..] xs, x == y ]

-- The list of all subsequences/sublists of a given list
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = ys ++ map (x:) ys where ys = subs xs

-- Call a subsequence of xs = [ x0, x1, x2, ... ] sparse if it does not 
-- contain two consecutive elements, such as x0 and x1, or x1 and x2

-- The list of sparse subsequences
sparseSubs :: [a] -> [[a]]
sparseSubs (x:xs@(y:ys)) = map (x:) (sparseSubs ys) ++ sparseSubs xs
sparseSubs xs = subs xs

-- OK but what is the maths here..

{- By the definition obove, the number of sparse subsequences of
 (x0:x1:xs) is the sum of the number of sparse subsequences of
 (x1:xs) and the number of sparse subsequences of xs.

 In other words, if we let f n denote the number of sparse
 subsequences of a list of length n, then

 f n = f (n-1) + f (n-2).

 This is, of course, the recursion for the Fibonacci numbers. So, on
 checking the initial conditions f 0 = 1 and f 1 = 2 it follows that
 f n is the n-th Fibonacci number.

 Now it wouldnt be programing if there wasnt more than one way to do it.
 sparseSubs that involves filtering out the sparse
 sequences from all subsequences -}

sparseSubs' :: [a] -> [[a]]
sparseSubs' xs = [ ys | (js, ys) <- map unzip . subs $ zip [0..] xs, f js ]
    where
      f vs = 1 `notElem` diff vs
      diff (x0:x1:xs) = (x1 - x0) : diff (x1:xs)
      diff _          = []

------------------------------------------------------------------------
------ Main ------------------------------------------------------------
------------------------------------------------------------------------
-- Just a quick demo of everything it does.
main = do
	putStrLn "\nStrings and Lists\n"
	putStrLn "TEST DATA"
	putStrLn ("TestAlpha: " ++ testAlpha)
	putStrLn ("testGavin: " ++ testGavin)
	putStrLn ("testZoo: " ++ testZoo)
	putStr "testNumList: "
	print testNumList
	putStr "testNamesList: "
	print testNamesList
	putStrLn ""
	putStrLn "OUPUTS"
	putStrLn "Input: lChar 1 = take 3 testAlpha"
	putStr "Output: "
	print $ lChar 1
	putStrLn "Input: lChar 2 = drop 3 testAlpha"
	putStr "Output: "
	print $ lChar 2
	putStrLn "Input: zip (lChar 1) (lChar 2)"
	putStr "Output: "
	print $ lCharPair 1
	putStrLn "Input: reverse (lChar 1)"
	putStr "Output: "
	print $ lChar 3 
	putStrLn "Input: (drop 1 (take 2 (lChar 1))) ++ reverse (lChar 2)"
	putStr "Output: "
	print $ lChar 4
	putStrLn "Input: end testGavin"
	putStr "Output: "
	print $ end testGavin
	putStrLn "Input: lower testGavin"
	putStr "Output: "
	print $ lower testGavin
	putStrLn "Input: surname testNamesList"
	putStr "Output: "
	print $ surname testNamesList
	putStrLn "Input: initials testNamesList"
	putStr "Output: "
	print $ initials testNamesList
	putStrLn "Input: testZoo ?? 'e'" 
	putStr "Output: "
	print $ testZoo ?? 'e'
	putStrLn "Input: positions testZoo 'e'" 
	putStr "Output: "
	print $ positions testZoo 'e'
	putStrLn "Input: positions testZoo 'a'" 
	putStr "Output: "
	print $ positions testZoo 'a'
	putStrLn "Input: subs testAlpha" 
	putStr "Output: "
	print $ subs testAlpha
	putStrLn "Input: sparseSubs testAlpha" 
	putStr "Output: "
	print $ sparseSubs testAlpha
	putStrLn ""
	putStrLn "End Output, Author: Gavin Donnelly"

------------------------------------------------------------------------
-----------   TEST DATA   ----------------------------------------------
------------------------------------------------------------------------
testAlpha :: String
testAlpha = "abcde"

testGavin :: String
testGavin = "Gavin"

testHello :: String
testHello = "Hello World"

testZoo :: String
testZoo = "Zookeeper"

testHelloList :: [String]
testHelloList = ["Hello", "world"] 

testNumList :: [Int]
testNumList = [1, 2, 3, 4, 5]

testNumList2 :: [Int]
testNumList2 = [1, 2, 3]

testNumList3 :: [Int]
testNumList3 = [4, 5, 6]

testNamesList :: [String]
testNamesList = ["Gavin Donnelly", "Davin Gonnelly","Haskell Curry", "Caskell Hurry"]

------------------------------------------------------------------------
-----------   MAIN OUTPUT   --------------------------------------------
------------------------------------------------------------------------
{-Strings and Lists

TEST DATA
TestAlpha: abcde
testGavin: Gavin
testZoo: Zookeeper
testNumList: [1,2,3,4,5]
testNamesList: ["Gavin Donnelly","Davin Gonnelly","Haskell Curry","Caskell Hurry"]

OUPUTS
Input: l1 = take 3 testAlpha
Output: "abc"
Input: l2 = drop 3 testAlpha
Output: "de"
Input: zip l1 l2
Output: [('a','d'),('b','e')]
Input: reverse l1
Output: "cba"
Input: (drop 1 (take 2 l1)) ++ reverse l2 
Output: "bed"
Input: end testGavin
Output: "n"
Input: lower testGavin
Output: "avin"
Input: surname testNamesList
Output: ["Donnelly","Gonnelly","Curry","Hurry"]
Input: initials testNamesList
Output: ["G.D","D.G","H.C","C.H"]
Input: testZoo ?? 'e'
Output: 4
Input: positions testZoo 'e'
Output: [4,5,7]
Input: positions testZoo 'a'
Output: []
Input: subs testAlpha
Output: ["","e","d","de","c","ce","cd","cde","b","be","bd","bde","bc","bce","bcd","bcde",
"a","ae","ad","ade","ac","ace","acd","acde","ab","abe","abd","abde","abc","abce","abcd","abcde"]
Input: sparseSubs testAlpha
Output: ["ac","ace","ad","a","ae","bd","b","be","c","ce","d","","e"]

End Output, Author: Gavin Donnelly-}
