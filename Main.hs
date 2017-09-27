module Main where

import Data.Foldable        (foldl')
import Data.Sequence hiding (null)
import Prelude       hiding (length, zipWith)

ilog2 :: Integral int => int -> Int
ilog2 = truncate . logBase 2 . fromIntegral

nats :: Integral int => int -> Seq int
nats = fromList . enumFromTo 1

groupBy :: (a -> a -> Bool) -> Seq a -> Seq (Seq a)
groupBy eq xs'
    | null xs'  = empty
    | otherwise = (x <| ys) <| groupBy eq zs
        where
            (x :< xs) = viewl xs'
            (ys, zs)  = spanl (eq x) xs

group :: Eq a => Seq a -> Seq (Seq a)
group = groupBy (==)

as :: Integral int => int -> Seq int
as n = foldl' (\a i -> a |> (i + a `index` ilog2 i + 1)) (singleton 1) [1 .. n]

bs :: Integral int => int -> Seq int
bs n = (snd . partition (`elem` as n) . nats) n

cs :: Integral int => int -> Seq int
cs n = (zipWith subtract (nats n) . bs) n

ds :: Integral int => int -> Seq Int
ds = fmap length . group . cs

main :: IO ()
main = (print . ds) 100000

{- $ time runghc Main.hs
 - fromList [1,2,3,1,4,1,1,1,2,1,1,1,1,1,1,1,5]
 -
 - real	5m42.221s
 - user	5m42.448s
 - sys	0m0.200s
 -}
