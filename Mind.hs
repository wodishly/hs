{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}

module Mind where

import Data.List
import Data.Maybe
import Data.Function
import Debug.Trace
import Control.Exception

type Shift a = a -> a
type Shell a = a -> [a]
type Shed a = [a] -> a

-- For fastenings

loudness :: Int
loudness = 0

ly :: Show a => Shift a
ly = ly' id

ly' :: Show b => (a -> b) -> Shift a
ly' f x = applyWhen (loudness > 0) (trace (show $ f x)) x

-- For frith

nothing :: String
nothing = "∅"

-- return the content of a singleton
only :: Shed a
only xs = assert (length xs == 1) (head xs)

-- assert `f x`, then return `x`
assertively :: (a -> Bool) -> Shift a
assertively f x = assert (f x) x

-- unpack and return a contentful maybe
recklessly :: Maybe a -> a
recklessly x = assert (isJust x) (fromJust x)

-- For fairness

-- assert that l == r, and then return their sameworth
samely :: Eq a => a -> a -> a
samely l r = assert (l == r) l

mid :: (a,b,c) -> b
mid (_,y,_) = y

nright :: (a,b,c) -> (a,b)
nright (x,y,_) = (x,y)

-- For foldworthies

none :: Foldable t => (a -> Bool) -> t a -> Bool
none f = not.any f

-- return all but the last n elements
leave :: Int -> Shift [a]
leave n xs = take (length xs - n) xs

-- return the last n elements
keep :: Int -> Shift [a]
keep n xs = drop (length xs - n) xs

-- hit only the nth element
hit :: Int -> Shift a -> Shift [a]
hit n f xs = take n xs ++ [f (xs!!n)] ++ drop (n+1) xs

-- todo: tests
hits :: [Int] -> Shift a -> Shift [a]
hits [] f xs = xs
hits ns f xs = hits (tail ns) f (hit (head ns) f xs)

-- max of a list, so named for alikeness with foldr (but likely meaningless)
maxr :: Shed Int
maxr = foldr max 0

-- how my heart yearns within me!
shell :: Shell a
shell = (: [])

-- split xs into a list of lists xss where each `last xss` gladdens `f`
split :: (a -> Bool) -> Shell [a]
split f xs = split' f (map singleton xs)

split' :: (a -> Bool) -> Shift [[a]]
split' f (a:b:rest) = lif
  (f.last $ a)
  (a : split' f (b:rest))
  (split' f ((a++b):rest))
split' _ xs = xs

full :: [a] -> Bool
full = not.null

-- if `x` is an initial segment of `y`
begins :: Eq a => [a] -> [a] -> Bool
begins y x = x == take (length x) y

-- vzw. terminal
ends :: Eq a => [a] -> [a] -> Bool
ends y x = x == keep (length x) y

-- [1,2,3,4,…] -> [(1,2), (2,3), (3,4), …]
atwain :: [a] -> [(a,a)]
atwain xs = zip (init xs) (tail xs)

-- For flitworthies

-- lif c t p = c ? t : p
lif :: Bool -> a -> a -> a
lif True x _ = x
lif False _ y = y

-- return `good` unless `f good`, whereupon return `bad`
lunless :: (a -> Bool) -> a -> a -> a
lunless f good bad = lif (f good) bad good

-- worldly begetting
implies :: Bool -> Bool -> Bool
implies p q = not p || q

-- For fainwell

padR :: Int -> Shift String
padR n s = lif (length s < n) (padR n (s++" ")) s

-- wrapper to handle oddness
twifold :: a -> [a] -> [(a,a)]
twifold x xs = twifold' (xs ++ lif (odd (length xs)) [x] [])

-- put the latter half of the list as the second component of tuples to the former half
twifold' :: [a] -> [(a,a)]
twifold' xs = (\l -> zip (take l xs) (drop l xs)) (div (length xs) 2)