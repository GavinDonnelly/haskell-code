import Data.List
-----------------------------------------------------------------------
 --
 -- Filename: cards_v1-1.hs
 --
 -- Synopsis: Implement BlackJack (or Pontoon) in Haskell
 --
 -- I dont know if this will work, but the basics of should just see 
 -- where it goes. 
 --
 -- Rules of the Game: you get delt two cards and you want to get as close 
 -- to 21 without going over, so 21 is the best score and down. You can 
 -- twist, and take another card, you can contiune this untill you go bust 
 -- (over 21) or decide to stick (keep the number you have eg. 19)
 -- 
 -- Author: Gavin Donnelly
 --
 -- Version: See VERSION below
 --
 -----------------------------------------------------------------------
 
 ----------------------------REVISION HISTORY:--------------------------
 -- 
 -- v1.O  19/10/2013: Initial version. 
 -- v1.1  20/10/2013: Did some of the basics and set out how the ting should
 --					  function with the game system, need to fisnh
 --
  ----------------------------TO DO:-------------------------------------
 --
 -- v1.1  15/09/2016: Adding this to github, need to clean up a bit and 
 --					  actually finish it.
-------------------------------------------------------------------------

-- Some types for out system
type Value = String -- King, Jack, Seven etc. Number of card
type Suit = String -- Spades etc. Suit of card
type Card = (Value, Suit) -- Card is its suit and value, just like real life
type Pack = [Card] -- a pack of cards, duh!
type Hand = [Card] -- the cards the current player has

suits::[Suit]
suits = ["Spades", "Clubs", "Hearts", "Diamond"]

values :: [Value]
values = ["Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", 
          "Nine", "Ten", "Jack", "Queen", "King"]

pack :: Pack 
pack = [(v,s)| v<-values, s<-suits]

val :: Card -> [Int]
val (v,s)   | v == "Ace" = [1,11] 
			| v == "Two" = [2]
			| v == "Three" = [3]
			| v == "Four" = [4]
			| v == "Five" = [5]
			| v == "Six" = [6]
			| v == "Seven" = [7]
			| v == "Eight" = [8]
			| v == "Nine" = [9]
			| otherwise = [10]

--interleave
interleave :: [a] -> [a] -> [a]
interleave [] ys =  ys
interleave xs [] = xs
interleave xs ys = head xs:head ys : interleave(tail xs) (tail ys)

-- shuffle
shuffle :: Pack -> Int -> Pack
shuffle p n = interleave (take n p) (drop n p) 

--shuffleList
shuffleList :: Pack -> [Int] -> Pack
shuffleList p [] = p
shuffleList p xs = shuffleList (shuffle p (head xs)) (tail xs) 


-- return value of list of cards
handValue :: Hand -> [Int]
handValue [] = 0
handValue (x:xs) =  if (val x ==[1,11])
            		then    (map sum (sequence [[1,11], handValue xs]))
                	else [sum [(val x)]++([handValue xs])]

--func1 [g| g<-(val x)] ([f |f<-[(val y)|y<-xs]])

--func1 :: [Int]->[[Int]]->[Int]
--func1 ys [] = []
--func1 [] ys = []
--func1 ys xs =  [(head ys)+f| f<-(head xs)] ++ func1 (tail ys) (tail xs)
			  -- where f2 f = f

--val(head xs) [ handValue (tail xs)]44 f<-(join[val y| y<-xs])  g,f)| g<-(val x),
  
--[(val x) | x<-xs ]

-- if bust or not
bust :: Hand -> Bool
bust [] = True
bust h = (handValue h == [])



-- twist 
type Player = Hand->Bool

twist16 :: Player
twist16 h   | bust h == True = False
			| maxList (handValue h) >= 17 = False
			| otherwise = True

--play game
play :: Player -> (Hand,Pack) -> (Hand,Pack)
play pl (h,p) = (h,p)

--game
game :: Pack -> [Player] -> [Hand]
game pac [pl] = [h]

showHand :: [Hand] -> IO()
showHand h = putStr (foldl (\x y -> x ++ "\n" ++ format y) [] h 
                     ++ "\n"  
                     ++ "Winner is player" 
                     ++ show (winner h))

format :: Hand -> String
format [] = []
format ((x1,y1):cs) = x1 ++ "\t" ++ y1 ++ "\n" ++ format cs 

showGame :: Pack -> [Player] -> IO()
showGame p s = showHand (game p s)  

winner :: [Hand] -> Int
winner [] = 0
winner (h:hs) 
   | mL (handValue h) > mL (concat (map handValue hs)) = 1
   | otherwise = winner hs + 1
     where mL xs = foldl max 0 xs

------------------------------------------------------------------------
-----------   TEST FUNCTIONS   -----------------------------------------
------------------------------------------------------------------------

testShuf :: Int-> Int
testShuf n = length (shuffle pack n)

------------------------------------------------------------------------
-----------   TEST DATA   ----------------------------------------------
------------------------------------------------------------------------

testHand1 :: Hand
testHand1 = [("Ace","Clubs"),("Nine","Diamonds"),("Ace","Hearts")]

testHand2 :: Hand
testHand2 = [("Two","Clubs"),("Three","Diamonds"),("Four","Hearts")]
