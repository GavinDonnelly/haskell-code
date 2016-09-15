-----------------------------------------------------------------------
 --
 -- Filename: hangman_v-1-1.hs
 --
 -- Synopsis: Hangman in Haskell
 --
 -- Everyone, knows the game hangman, right??! If you don't the game of
 -- hangman is to guess an unknown word. Each turn the guesser chooses a 
 -- letter. All occurrences of the letter of the unknown word are shown to
 -- the guesser. If no occurrences of the chosen letter the the guesser gets
 -- a piece of the hangman (eg. arm, leg, etc). A full man aka all lifes lost
 -- and the guesser losses. If the guesser guess the word, you GUESSED IT, 
 -- they win.
 --
 -- To define hangman, can model the game by a triple. Unknown (secret) word
 -- to be guessed, the list of guesses that have been made (history) and a list
 -- of guesses that will be made. So ("gavin", "ai", "gdolekropmbvxz") this is 
 -- a game where the unknown word is gavin, 2 guesses have been made, a and i, 
 -- the next guess will be g and then d etc.
 --
 -- Finally the game of Hanfman is in different states, Win (if the guesser has
 -- has won, so lost less than 11 lifes and guessed all the letter of unkown word)
 -- Lose (if the guesser has lost more than 10 lives) and Alive (otherwise).
 -- 
 -- Author: Gavin Donnelly
 --
 -- Version: See VERSION below
 --
 -----------------------------------------------------------------------
 
 ----------------------------REVISION HISTORY:--------------------------
 -- 
 -- v1.O  02/10/2013: Initial version. 
 -- v1.1  04/10/2013: Hangman game works !!
 -----------------------------------------------------------------------

 ----------------------------TO DO:-------------------------------------
 --
------------------------------------------------------------------------
import Data.List (elem)

type Secret  = String  -- Unknown word
type History = String  -- Made guesses
type Guesses = String  -- Future guesses
type Hang    = (Secret, History, Guesses) -- The Hnagman triple.

data Result = Win | Lose | Alive deriving (Show,Eq) -- Possible Results

{- Helper function which takes to lists and returns list of all elements in the
first which arn't in the second (Common Functional programming function) -}
diff :: Eq a => [a] -> [a] -> [a]
diff []     _  = []
diff (x:xs) ys = if x `elem` ys then zs else x:zs
  where
    zs = diff xs ys

-- Status of a game of Hangman
status :: Hang -> Result
status (secret,history,guesses)
    | misses < 11 && null unknowns = Win
    | misses > 10                  = Lose
    | otherwise                    = Alive
  where
    unknowns = diff secret history
    misses = length (diff history secret)

{- One way to implement Hangman is to repeatedly make a guess until the game is 
no longer Alive or there are no more guesses. To do this you need a function 
which takes three inputs: a function, a test (that is a function returning a boolean) 
and an expression. If the expression satisfies the test, then iterateUntil returns 
the expression. Otherwise, iterateUntil repeatedly applies the function to the 
expression until the test is satisfied -}
iterateUntil :: (a -> a) -> (a -> Bool) -> a -> a
iterateUntil f p x = if p x then x else iterateUntil f p (f x)

{- takes a hangman type game and returns true if no more guess can be made.
this can happen when list of guesses is empty or game is not alive -}
done :: Hang -> Bool
done h@(_,_,guesses) = null guesses || status h /= Alive

-- Takes the next guess to be made and adds it to the list of guesses which
-- have been made
makeGuess :: Hang -> Hang
makeGuess (secret,history,x:xs) = (secret,x:history,xs)

{- The Game!, takes a pair of strings as input, firsst string, unknown word
 and the second string is the list of guesses and returns the state of the 
 game after the guesser has either won or lost or no futher gueses-}
hangman  :: Secret -> Guesses -> (Result, Hang)
hangman secret guesses = (status hang, hang)
  where
    hang = iterateUntil makeGuess done (secret,[],guesses)
