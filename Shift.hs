{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}

module Shift where

import Prelude hiding (Word)
import Data.Function (applyWhen)

import Mind
import Token
import Loud
import Breath
import Word

queue :: [Shift a] -> Shift a
queue = foldr (.) id

-- if `cs`, then do shift `f` on loud `l`
shif :: [Loud -> Bool] -> Shift Loud -> Shift Loud
shif cs f l = lif ((and.sequence cs) l) (f l) l

borrow :: Mark -> Loud -> Shift Loud
borrow m l = set (get m l)

-- blind to prosodic structure !!
-- kill all in `w` that meet `c`
kill :: (Loud -> Bool) -> Shift Word
kill c w = makeWord $ filter (not.c) (flatten w)

-- beget `x` before all in `w` that meet `c`
begetl :: Loud -> (Loud -> Bool) -> Shift Word
begetl = beget L

-- beget `x` after all in `w` that meet `c`
begetr :: Loud -> (Loud -> Bool) -> Shift Word
begetr = beget R

beget :: Hand -> Loud -> (Loud -> Bool) -> Shift Word
beget h x c w = makeWord $ beget' h x c (flatten w)

beget' :: Hand -> Loud -> (Loud -> Bool) -> Shift Flight
beget' h x c = foldr (\l -> (++) (lif (h == L) id reverse (lif (c l) [x] [] ++ [l]))) []

sweep :: Shift Flight
sweep = filter (/= unloud)

sweepWord :: Shift Word
sweepWord = liftBW.liftFB $ sweep

-- send a flight to a flight of tuples that encodes awareness of the nearest neighbor

leftly :: Flight -> [(Maybe Loud, Loud)]
leftly ls = zip (Nothing : map Just (leave 1 ls)) ls

rightly :: Flight -> [(Loud, Maybe Loud)]
rightly ls = zip ls (map Just (drop 1 ls) ++ [Nothing])

bothly :: Flight -> [(Maybe Loud, Loud, Maybe Loud)]
bothly ls = zipWith (\x y -> (fst x, samely (snd x) (fst y), snd y)) (leftly ls) (rightly ls)

liftLF :: Shift Loud -> Shift Flight
liftLF = map

liftFB :: Shift Flight -> Shift Breath
liftFB f br = Breath (f (onset br)) ((\x -> Rime (f (inset x)) (f (offset x))) (rime br)) False

liftBW :: Shift Breath -> Shift Word
liftBW f (Word br) = Word (map f br)

workAll :: Shift Loud -> Shift Word
workAll = liftBW.liftFB.liftLF

workFirst :: Shift Loud -> Shift Word
workFirst f (Word bs) = Word ((\(Breath o r sh) -> lif
    (full o)
    (Breath ((\o -> f (head o) : tail o) o) r sh) -- onsetful
    (Breath [] ((\(Rime i o) -> Rime (f (head i) : tail i) o) r) sh) -- onsetless
    )
  (head bs) : tail bs)

onbear :: Shift Flight
onbear ls = map onbear' (bothly ls)

onbear' :: (Maybe Loud, Loud, Maybe Loud) -> Loud
onbear' (l, m, r) = applyWhen (all (maybe True (not.worth' Bear)) [l, r])
  (lif (isThroat m)
    onbearThroat
    (applyWhen (worth' Smooth m) (on Bear)))
  m

gainbear :: Shift Word
gainbear = makeWord.onbear.flatten

nosesame :: Shift Word
nosesame w = makeWord $ map nosesame' (rightly $ flatten w)

nosesame' :: (Loud, Maybe Loud) -> Loud
nosesame' (l, Just r) = applyWhen (worth' Nose l && isRough r) (borrow Mouth r) l
nosesame' (l, _) = l

lurk :: [Shift Word] -> Shift Word -> Shift Word
lurk shs f = queue (shs++[f])

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