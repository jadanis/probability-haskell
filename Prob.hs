module Prob
( coin
, die
, die_4
, die_6
, die_8
, die_12
, die_20
, makeDie
, makeProb
, coinToDie
, Coin (Heads, Tails)
, Die (Face)
, joinProb
, loadedCoin
, getEvents
, getEvent
, getEvents_
, getEvent_
) where

import Data.Ratio
import Control.Monad
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import System.Random

-- From Learn You a Haskell
newtype Prob a = Prob {getProb :: [(a,Rational)] } deriving (Show,Read)

-- From Learn You a Haskell
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\(x,r) -> (x,p*r)) innerxs

-- From Learn You a Haskell
instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x, p)) xs

-- I added this as current compilers need a Monad to be an instance of Applicative 
instance Applicative Prob where
    pure x = Prob [(x,1%1)]
    (Prob m) <*> (Prob xs) = Prob [(f x,p*q) | (f,q) <- m, (x,p) <- xs]

-- From Learn You a Haskell
instance Monad Prob where
    return = pure
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

-- from okapies
groupProb :: (Eq a) => [(a,Rational)] -> [[(a,Rational)]]
groupProb = groupBy ((==) `on` fst)

-- from okapies
sortProb :: (Ord a) => [(a,Rational)] -> [(a,Rational)]
sortProb = sortBy (comparing fst)

-- from okapies
joinProb :: (Eq a, Ord a) => Prob a -> Prob a
joinProb (Prob xs) =
    Prob [(fst $ head ys, sum $ map snd ys) | ys <- groupProb . sortProb $ xs]

makeProb :: [a] -> Prob a
makeProb xs = Prob [(x, 1 % n) | x <- xs]
    where n = fromIntegral $ length xs

makeDie :: Int -> Prob Die
makeDie 0 = Prob []
makeDie n = makeProb [(Face m) | m <- [1..n]]

-- From Learn You a Haskell
data Coin = Heads | Tails deriving (Show, Eq, Ord)

-- From Learn You a Haskell
coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

-- From Learn You a Haskell
loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

-- Added Die type
data Die = Face Int deriving (Show, Read, Ord, Eq)

-- standard six sided fair die
die :: Prob Die
die = makeDie 6

-- redundant to follow pattern below
die_6 :: Prob Die
die_6 = die

-- die_x for x sided fair die
die_4 :: Prob Die
die_4 = makeDie 4

die_8 :: Prob Die
die_8 = makeDie 8

die_12 :: Prob Die
die_12 = makeDie 12

die_20 :: Prob Die
die_20 = makeDie 20


-- to create a '2 sided die' from a Coin just in case
coinToDie :: Coin -> Die
coinToDie c = Face $ if c == Heads then 2 else 1

-- given a Prob a creates a [a] where number of occurences of x <- [a] is proportional to the associated Probability weight
getEventList :: Prob a -> [a]
getEventList (Prob xs) = let probs = [p | (x,p) <- xs]
                             n = foldl lcm 1 [denominator m | m <- probs]
                             ys = [(x,fromIntegral $ (numerator p)*(div n (denominator p))) | (x,p) <- xs ]
                          in join $ map (\(x,p) -> replicate p x) ys

-- using a RandomGen to create a random list of indeces of length n then map these onto
-- the result of getEventList
getEvents :: (RandomGen g) => g -> Prob a -> Int -> [a]
getEvents _ _ 0 = []
getEvents g (Prob xs) n = let events = getEventList (Prob xs)
                              indcs = take n $ randomRs (1,length events) g :: [Int]
                          in map (events !!) (map (\x -> x-1) indcs)

-- gets one random event given a RandomGen and Prob a
getEvent :: (RandomGen g) => g -> Prob a -> a
getEvent g (Prob xs) = head $ getEvents g (Prob xs) 1

-- for use with IO provides the StdGen
getEvents_ :: Prob a -> Int -> IO [a]
getEvents_ (Prob xs) n = do
    gen <- getStdGen
    return $ getEvents gen (Prob xs) n

-- for use with IO provides the StdGen
getEvent_ :: Prob a -> IO a
getEvent_ (Prob xs) = do
    gen <- getStdGen
    return $ getEvent gen (Prob xs)
