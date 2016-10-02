--  File     : CardGuess
--  Author   : Ivan Matheu Malo Robinson - Student ID 661332
--  Purpose  : An implementation of a Game State of the Guess Card Game

module Cardguess (initialGuess, nextGuess, GameState) where

import Card
import Data.List


type GameState = [[Card]] 
myDeck = [minBound..maxBound]::[Card] -- Calls a Deck of Cards


-- initialGuess takes the number of cards in the answer as input and returns 
-- a pair of an initial guess,which should be a list of the specifed number
-- of cards,and a game state.

initialGuess :: Int -> ([Card], GameState )
initialGuess n = (initialCards , (combination n myDeck))
    where initialCards = initial_guess_gen n

-- Generates the initial guess cards according to n
initial_guess_gen :: Int -> [Card]
initial_guess_gen n 
   | n == 2 = [Card Diamond R2, Card Heart Ace]
   | n == 3 = [Card Club Queen, Card Diamond Ace, Card Spade R2]
   | n == 4 = [Card Club R5, Card Diamond R5, Card Heart R5, Card Spade R5]

-- creates a combination of possible guesses
combination :: Int -> [Card] -> [[Card]]
combination 0 _  = [[]]
combination n cs = [ x:xs| x:cs' <- tails cs, xs <- combination (n-1) cs']

-- nextGuess takes as input a pair of the previous guess and game state, and 
-- the feedback to this guess as a quintuple of counts of correct cards, 
-- low ranks, correct ranks, high ranks, and correct suits, and returns a 
-- pair of the next guess and new game state.

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (card, gs) feedback = (newCard, newGameState)
                          where newGameState = newState (card,gs) feedback
                                newCard     = head newGameState

-- Creates a new state game from the previous GameState
newState :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> GameState
newState ( _ , []) _ = []
newState (card, (g:gs)) feedback
    | response card g == feedback = [g] ++ (newState (card, gs) feedback)
    | otherwise                   = newState (card,gs) feedback

-- Produce the feedback from the 2 list of cards
response :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
response answer guess  = (f1,f2,f3,f4,f5)
    where f1 = count_equal_card  answer guess
    	  f2 = count_lower_rank  answer guess
          f3 = count_equal_rank  answer guess
          f4 = count_higher_rank answer guess
          f5 = count_equal_suit  answer guess 

-- count_equal_card gives the number of correct cards in the fb
count_equal_card :: [Card] -> [Card] -> Int
count_equal_card answer guess = countEqCard answer guess
       
--count_lower_rank gives the number of Low ranks in the fb
count_lower_rank :: [Card] -> [Card] -> Int
count_lower_rank answer guess = result
    where a      = map (rank) answer
          g      = map (rank) guess
          result = count_lower a g

--count_equal_rank gives the number of correct ranks in the fb
count_equal_rank :: [Card] -> [Card] -> Int
count_equal_rank answer guess = result
        where a = sort $ map (rank) answer
              g = sort $ map (rank) guess
              result = count_equal a g

-- count_higher_rank gives the number of Low ranks in the fb
count_higher_rank :: [Card] -> [Card] -> Int
count_higher_rank answer guess = result
    where a      = map (rank) answer
          g      = map (rank) guess
          result = count_higher a g

-- count_equal_suit gives the number of correct suits in the fb
count_equal_suit :: [Card] -> [Card] -> Int
count_equal_suit answer guess = result
    where a = sort $ map (suit) answer
          g = sort $ map (suit) guess
          result = count_equal a g


--------------------------------------------------------------------------------
-- count equal cards 
countEqCard :: (Ord a) => [a] -> [a] -> Int
countEqCard _      []  = 0
countEqCard  []     _  = 0
countEqCard (x:xs) ys 
    | elem x ys        = 1 + countEqCard xs ys
    | otherwise        = countEqCard     xs ys

-- Counts equal types
count_equal :: (Ord a) => [a] -> [a] -> Int
count_equal _ [] = 0
count_equal [] _ = 0
count_equal (x:xs) (y:ys) 
    | x == y    = 1 + count_equal xs     ys
    | x <  y    =     count_equal xs     (y:ys)
    | otherwise =     count_equal (x:xs) ys
 
-- count_lower types
count_lower :: (Ord a) => [a] -> [a] -> Int
count_lower _ [] = 0
count_lower [] _ = 0 
count_lower (x:xs) ys 
    | x < minimum ys  = 1 + count_lower xs ys
    | otherwise       =     count_lower xs ys

--count_higher type
count_higher :: (Ord a) => [a] -> [a] -> Int
count_higher _ [] = 0
count_higher [] _ = 0 
count_higher (x:xs) ys 
    | x > maximum ys   = 1 + count_higher xs ys
    | otherwise        =     count_higher xs ys







