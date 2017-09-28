module Main where

import Data.List (genericLength, group, mapAccumL, partition)
import Data.Map  ((!), Map, insert, singleton)

type Sequence int = int -> [int]

ilog2 :: Integral int => int -> int
ilog2 = truncate . logBase 2 . fromIntegral

nats :: (Num a, Enum a) => a -> [a]
nats = enumFromTo 1

a :: Integral int => int -> Map int int -> int -> (Map int int, int)
a limit memo i = (memo', a')
    where
        a' = 1 + i + (memo ! ilog2 i)

        memo' | i <= limit = insert i a' memo
              | otherwise = memo

as :: Integral int => Sequence int
as n = ((1:) . snd . mapAccumL (a lg) (singleton 0 1) . nats) n
    where
        lg = ilog2 n

bs :: Integral int => Sequence int
bs n = (snd . partition (`elem` as n) . nats) n

cs :: Integral int => Sequence int
cs n = (zipWith subtract (nats n) . bs) n

ds :: Integral int => Sequence int
ds = map genericLength . group . cs

main :: IO ()
main = (print . ds) (10^6)

{- $ time stack exec Elias-omega-coding
 - fromList [1,2,3,1,4,1,1,1,2,1,1,1,1,1,1,1,5,1,1,1]
 -
 - real	106m50.100s
 - user	106m49.756s
 - sys	0m0.162s
 -}
