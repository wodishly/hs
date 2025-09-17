{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Tide where

import Prelude hiding (Word)
import Data.Function (applyWhen)

import Mind
import Loud
import Breath
import Word
import Shift
import Bend

unring :: Shift Word
unring = workAll $ shif
  [worths [Choke, Ring]]
  (queue [offs [Lip]])

-- ky gy gyh > th dh *dhh
toothen :: Shift Word
toothen = workAll $ shif
  [isRough, worth' Fore]
  (queue [ons [Blade, Thru, Step], offs [Body]])

-- bh dh *dhh gh > f lh z h
unspread :: Shift Word
unspread = workAll $ shif [worth' Spread] unspread'

unspread' :: Shift Loud
unspread' l
  | worth' Lip l = dirty "f"
  | worth' Blade l = dirty "lh"
  | worths [Fore, Body] l = dirty "z"
  | otherwise = dirty "h"

-- h3 > w
outthroat :: Shift Word
outthroat = queue [workAll $ shif
  [(== dirty "h3")]
  (become $ dirty "w"), outthroat']

-- h3o > h3e
outthroat' :: Shift Word
outthroat' w = makeWord $ map outthroat'' (leftly $ flatten w)

outthroat'' :: (Maybe Loud, Loud) -> Loud
outthroat'' (Just l, r) = applyWhen
  (l == dirty "h3")
  (shif [worths [Bear, Ring]] (queue [on Fore, offs [Lip, Back]]))
  r
outthroat'' (_, r) = r

-- H > 0
unthroat :: Shift Word
unthroat = queue [kill isThroat, workAll $ shif [isThroat, worth' Bear] (become $ dirty "i")]

-- R -> yr
unbreath :: Shift Word
unbreath = queue [
  workAll $ shif [isDerm] (\x -> lif (isThroat x) (offbearThroat x) (offs [Long, Bear] x)),
  begetl (dirty "i") isDerm
 ]

-- m, n > w, y
soften :: Shift Word
soften = workFirst $ shif
  [worth' Nose, not.worth' Bear]
  (queue [soften', ons [Thru, High, Tight], offs [Nose, Choke, Blade]])

soften' :: Shift Loud
soften' l
  | worth' Lip l = ons [Ring, Back] l
  | otherwise = ons [Fore] l

-- CHV > CCV
-- todo: should probably be VCHV > VCCV
throatsame :: Shift Word
throatsame w = makeWord $ map throatsame' (bothly $ flatten w)

throatsame' :: (Maybe Loud, Loud, Maybe Loud) -> Loud
throatsame' (Just l, m, Just r) = applyWhen
  (isThroat m && not (worth' Bear l) && worth' Bear r)
  (become l)
  m
throatsame' (_, m, _) = m

lengthen :: Shift Word
lengthen w = makeWord $ map lengthen' (rightly $ flatten w)

lengthen' :: (Loud, Maybe Loud) -> Loud
lengthen' (l, Just r) = applyWhen (worth' Bear l && isThroat r) (on Long) l
lengthen' (l, _) = l

tideshift :: Shift Word
tideshift = queue $ map (lurk [ly, gainbear, nosesame]) [id
  , toothen
  , unspread
  , soften
  , unbreath
  , unthroat
  , unring
  , outthroat
  , lengthen
  , throatsame
 ]