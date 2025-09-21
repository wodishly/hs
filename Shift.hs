{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}

module Shift where

import Prelude hiding (Word)
import Data.Maybe
import Data.Function (applyWhen)
import Data.Bifunctor

import Mind
import Token
import Loud
import Breath
import Mark

queue :: Shed (Shift a)
queue = foldr (.) id

-- if `cs`, then do shift `f` on loud `l`
shif :: [Loud -> Bool] -> Shift (Shift Loud)
shif cs f l = lif ((and.sequence cs) l) (f l) l

borrow :: Loudmark -> Loud -> Shift Loud
borrow m l = set (get m l)

-- blind to prosodic structure !!
-- kill all in `w` that meet `c`
kill :: (Loud -> Bool) -> Shift Bright
kill c w = makeBright $ filter (not.c) (flatten w)

-- beget `x` before all in `w` that meet `c`
begetl :: Loud -> (Loud -> Bool) -> Shift Bright
begetl = beget id

-- beget `x` after all in `w` that meet `c`
begetr :: Loud -> (Loud -> Bool) -> Shift Bright
begetr = beget reverse

beget :: Shift Flight -> Loud -> (Loud -> Bool) -> Shift Bright
beget f x c w = makeBright $ beget' f x c (flatten w)

beget' :: Shift Flight -> Loud -> (Loud -> Bool) -> Shift Flight
beget' f x c = foldr (\l -> (++) (f (lif (c l) [x] [] ++ [l]))) []

sweep :: Shift Flight
sweep = filter (/= unloud)

sweepWord :: Shift Bright
sweepWord = float sweep

-- send a flight to a flight of tuples that encodes awareness of the nearest neighbors

leftly :: Int -> Flight -> [(Flight, Loud)]
leftly 0 ls = map ([], ) ls
leftly n ls = zipWith (\x y -> bimap ((++) (fst x)) (samely (snd x)) y)
  (zipWith (\x y -> (catMaybes x, y))
    (map shell $ replicate n Nothing ++ map Just (leave n ls)) ls)
  (leftly (n-1) ls)

rightly :: Int -> Flight -> [(Loud, Flight)]
rightly 0 ls = map (, []) ls
rightly n ls = zipWith (\x y -> bimap (samely (fst x)) ((++) (snd x)) y)
  (rightly (n-1) ls)
  (zipWith (\x y -> (x, catMaybes y))
    ls (map shell $ map Just (drop n ls) ++ replicate n Nothing))

bothly :: Int -> Int -> Flight -> [(Flight, Loud, Flight)]
bothly m n ls = zipWith (\x y -> (fst x, samely (snd x) (fst y), snd y))
  (leftly m ls) (rightly n ls)

-- deepshift to shoalshift
float :: Shift Flight -> Shift Bright
float f = map (shiftRime f f.shiftOnset f)

-- shoalshift to deepshift
sink :: Shift Bright -> Shift Flight
sink f = flatten.f.makeBright

workAll :: Shift Loud -> Shift Bright
workAll = float.map

workFirst :: Shift Loud -> Shift Bright
workFirst f bs = hit 0 (lif (full.onset.head $ bs) shiftOnset shiftInset (hit 0 f)) bs

onbear :: Shift Flight
onbear ls = map onbear' (bothly 1 1 ls)

onbear' :: (Flight, Loud, Flight) -> Loud
onbear' (l, m, r) = applyWhen (not (any (any (worth' Bear)) [l, r]))
  (lif (isThroat m) onbearThroat (applyWhen (worth' Smooth m) (on Bear))) m

gainbear :: Shift Bright
gainbear = makeBright.onbear.flatten

nosesame :: Shift Bright
nosesame w = makeBright $ map nosesame' (rightly 1 $ flatten w)

nosesame' :: (Loud, Flight) -> Loud
nosesame' (l, [r]) = applyWhen (worth' Nose l && isRough r && (not.worth' Thru) r) (borrow Mouth r) l
nosesame' (l, []) = l

-- todo: this
stavefold :: Shift Bright
stavefold bs = bs

-- todo: prove if this is a natural transformation omg i exit
lurk :: [Shift Bright] -> Shift (Shift Bright)
lurk shs f = queue (shs++[f])

-- do `f` until `br == f br`
gainly :: Shift Bright -> Shift Bright
gainly f br = lunless (/= br) (f br) (gainly f (f br))

--workFirst' :: Shift a -> Shift [a]
--workFirst' f xs = f (head xs) : tail xs
--
--workLast' :: Shift a -> Shift [a]
--workLast' f xs = init xs ++ [f (last xs)]
--
--lift1LB :: Shift Loud -> Shift Breath
--lift1LB f b = Breath (workFirst' f (onset b)) (rime b) False
--
--lift1BW :: Shift Breath -> Shift Word
--lift1BW f w = Word $ workFirst' f (breaths w)
--
--workFirst :: Shift Loud -> Shift Word
--workFirst = lift1BW.lift1LB
--
-- for fanding
--softenFirst :: Word -> Word
--softenFirst = workFirst (shif [not.worth' Smooth, not.worth' Thru] (on Stave))
--
--offstaveLast :: Shift Flight
--offstaveLast = workLast' $ offs [Throat, Stave]