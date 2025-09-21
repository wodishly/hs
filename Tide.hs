{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Tide where

import Prelude hiding (Word)
import Data.Function (applyWhen)

import Mind
import Loud
import Breath
import Shift
import Bend
import Mark

unring :: Shift Bright
unring = workAll $ shif
  [worths [Choke, Ring]]
  (offs [Lip])

-- ky gy gyh > th dh *dhh
toothen :: Shift Bright
toothen = workAll $ shif
  [isRough, worth' Fore]
  (queue [ons [Blade, Thru, Step], offs [Body]])

-- bh dh *dhh gh > f lh z h
unspread :: Shift Bright
unspread = workAll $ shif
  [worth' Spread]
  unspread'

unspread' :: Shift Loud
unspread' l
  | worth' Lip l = dirty "f"
  | worth' Blade l = dirty "lh"
  | worths [Fore, Body] l = dirty "z"
  | otherwise = dirty "h"

-- h3 > w
outthroat :: Shift Bright
outthroat = queue [workAll $ shif
  [(== dirty "h3")]
  (become $ dirty "w"), outthroat']

-- h3o > h3e
outthroat' :: Shift Bright
outthroat' w = makeBright $ map outthroat'' (leftly 1 $ flatten w)

outthroat'' :: (Flight, Loud) -> Loud
outthroat'' ([l], r) = applyWhen
  (l == dirty "h3")
  (shif [worths [Bear, Ring]] (queue [on Fore, offs [Lip, Back]]))
  r
outthroat'' (_, r) = r

-- H > 0
unthroat :: Shift Bright
unthroat = queue [kill isThroat, workAll $ shif [isThroat, worth' Bear] (become $ dirty "i")]

-- R -> yr
unbreath :: Shift Bright
unbreath = queue [
  workAll $ shif [isDerm] (\x -> lif (isThroat x) (offbearThroat x) (offs [Long, Bear] x)),
  begetl (dirty "i") isDerm
 ]

-- m, n > w, y
soften :: Shift Bright
soften = workFirst $ shif
  [worth' Nose, not.worth' Bear]
  (queue [soften', ons [Thru, High, Tight], offs [Nose, Choke, Blade]])

soften' :: Shift Loud
soften' l
  | worth' Lip l = ons [Ring, Back] l
  | otherwise = ons [Fore] l

-- CHV > CCV
-- todo: should probably be VCHV > VCCV
throatsame :: Shift Bright
throatsame w = makeBright $ map throatsame' (bothly 1 1 $ flatten w)

throatsame' :: (Flight, Loud, Flight) -> Loud
throatsame' ([l], m, [r]) = applyWhen
  (isThroat m && not (worth' Bear l) && worth' Bear r)
  (become l)
  m
throatsame' flf = mid flf

lengthen :: Shift Bright
lengthen w = makeBright $ map lengthen' (rightly 1 $ flatten w)

lengthen' :: (Loud, Flight) -> Loud
lengthen' (l, [r]) = applyWhen (worth' Bear l && isThroat r) (on Long) l
lengthen' (l, _) = l

tideshift :: Shift Bright
tideshift = queue $ map (lurk [gainbear, nosesame]) [id
  , toothen
  , unspread
  , soften
  , gainly unbreath
  , unthroat
  , unring
  , outthroat
  , lengthen
  , throatsame
 ]