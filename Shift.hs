{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Use lambda-case" #-}

module Shift where

import Prelude hiding (Word)
import Data.Maybe
import Data.Function (applyWhen)
import Data.Bifunctor

import Mind
import Mark
import Token
import Loud
import Breath

queue :: Shed (Shift a)
queue = foldr (.) id

-- if `cs`, then do shift `f` on loud `l`
shif :: [Loud -> Bool] -> Shift (Shift Loud)
shif cs f l = lif ((and.sequence cs) l) (f l) l

borrow :: Loudmark -> Loud -> Shift Loud
borrow = (set .) . get

-- blind to prosodic structure !!
-- kill all in `w` that meet `c`
kill :: (Loud -> Bool) -> Shift Bright
kill c = makeBright . filter (not.c) . flatten

-- beget `x` before all in `w` that meet `c`
begetl :: Loud -> (Loud -> Bool) -> Shift Bright
begetl = beget id

-- beget `x` after all in `w` that meet `c`
begetr :: Loud -> (Loud -> Bool) -> Shift Bright
begetr = beget reverse

beget :: Shift Flight -> Loud -> (Loud -> Bool) -> Shift Bright
beget f x c = makeBright . beget' f x c . flatten

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
float = (makeBright .) . flip (.) flatten

-- shoalshift to deepshift
sink :: Shift Bright -> Shift Flight
sink = (flatten .) . flip (.) makeBright

workAll :: Shift Loud -> Shift Bright
workAll = float.map

workFirst :: Shift Loud -> Shift Bright
workFirst f bs = hit 0 (lif (full.onset.head $ bs) shiftOnset shiftInset (hit 0 f)) bs

offbear :: Shift Flight
offbear = map (\x -> lif (isDerm x) (lif (isThroat x) (offbearThroat x) (unbear x)) x)

onbear :: Shift Flight
onbear = map onbear' . bothly 1 1

onbear' :: (Flight, Loud, Flight) -> Loud
onbear' (l, m, r) = applyWhen (not (any (any (worth' Bear)) [l, r]))
  (lif (isThroat m) onbearThroat (applyWhen (worth' Smooth m) (on Bear))) m

gainbear :: Shift Bright
gainbear = float (onbear.offbear)

nosesame :: Shift Bright
nosesame = makeBright . map nosesame' . rightly 1 . flatten

nosesame' :: (Loud, Flight) -> Loud
nosesame' (l, [r]) = applyWhen (worth' Nose l && isRough r && (not.worth' Thru) r) (borrow Mouth r) l
nosesame' (l, []) = l

stavefold :: Shift Bright
stavefold = float stavefold'

stavefold' :: Shift Flight
stavefold' ls = case ls of
--  (a:b:rest) -> lif (worth' Bear a && (on Long a == on Long b)) (stavefold' (on Long b:rest)) (a: stavefold' (b:rest))
  (a:b:rest) -> lif (all (\x -> worth' Bear x && not (isDerm x)) [a,b]) (stavefold' (on Long a:rest)) (a: stavefold' (b:rest))
  stuff -> stuff

lurk :: [Shift Bright] -> Shift (Shift Bright)
lurk = (queue .) . (flip (.) shell . (++))

-- do `f` until `br == f br`
gainly :: Shift Bright -> Shift Bright
gainly f br = until (== f br) f br