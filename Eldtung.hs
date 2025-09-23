module Eldtung where

import Data.Bifunctor

import Mind
import Mark
import Loud hiding (Root)
import Breath
import Shift
import Bend
import Data.Function (applyWhen)

rootByMean :: String -> Root
rootByMean = only . flip filter roothoard . flip (.) meaning . (==)

roothoard :: [Root]
roothoard = map ((uncurry Root . first dirtys) . nright) allhoard

data Yoke = OYoke | GYoke | RYoke | NYoke | TYoke | Unyoke deriving (Eq)

-- todo: likely useless
bud :: Yoke -> Flight
bud OYoke = dirtys "o"
bud GYoke = dirtys "ey"
bud RYoke = dirtys "er"
bud NYoke = dirtys "en"
bud _ = []

-- infer yoke from stem shape
-- ken :: Flight -> Stem
-- ken ls = case clean (last ls) of
--   "o" -> ken' 1 OYoke ls
--   "r" -> ken' 2 RYoke ls
--   "n" -> ken' 2 NYoke ls
--   "y" -> ken' 2 GYoke ls
--   "w" -> ken' 2 GYoke ls
--   _   -> ken' 0 Unyoke ls
-- 
-- ken' :: Int -> Yoke -> Flight -> Stem
-- ken' n y ls = Stem (leave n ls) y Unkind ""

allhoard :: [(String, String, Yoke)]
allhoard = concat [kinhoard, rimhoard, godhoard]

kinhoard :: [(String, String, Yoke)]
kinhoard = [
   ("ph2t", "father", RYoke)
 , ("meh2t", "mother", RYoke)
 ]

godhoard :: [(String, String, Yoke)]
godhoard = [
   ("bhewdh", "bede", OYoke)
 , ("dyew", "heaven", Unyoke)
 , ("dhegyh", "earth", NYoke)
 ]

rimhoard :: [(String, String, Yoke)]
rimhoard = [
   ("h1oyn", "one", OYoke)
 , ("dw", "two", OYoke)
 , ("tr", "three", GYoke)
 , ("qetw", "four", RYoke)
 , ("penqe", "five", Unyoke)
 , ("sweky", "six", Unyoke)
 , ("septM", "seven", Unyoke)
 , ("h3ekyteh3", "eight", Unyoke)
 , ("h1newn", "nine", Unyoke)
 , ("dekym", "ten", Unyoke)
 ]

stemhoard :: [Stem]
stemhoard = stemhoard' allhoard

stemhoard' :: [(String, String, Yoke)] -> [Stem]
stemhoard' = map (\(l,m,y) -> Stem (Left (Root (dirtys l) m)) (bud y) Unkind)

endinghoard :: [[Ending]]
endinghoard = stack 2 $ map (\(ls, sh) -> Ending (dirtys ls) sh) [
   ("s", ons [Main] $ def Bow)
 , ("m", ons [Main, Mean] $ def Bow)
 , ("es", ons [] $ def Bow)
 , ("ey", ons [Mean] $ def Bow)
 , ("es", ons [Many, Main] $ def Bow)
 , ("ms", ons [Many, Main, Mean] $ def Bow)
 , ("oh1om", ons [Many] $ def Bow)
 , ("bhys", ons [Many, Mean] $ def Bow)
 ]

mend :: Stem -> Board
mend = bend endinghoard

zg :: Shift Flight
zg (a:b:c:rest) = lif (a == dirty "e" && not (any (worth' Bear) [b,c])) [] [a] ++ zg (b:c:rest)
zg xs = xs

unring :: Shift Bright
unring = workAll $ shif
  [worths [Choke, Ring]]
  (off Lip)

-- ky gy gyh > th dh *dhh
toothen :: Shift Bright
toothen = workAll $ shif
  [isRough, worth' Fore]
  (queue [ons [Blade, Thru, Step], off Body])

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
outthroat' = makeBright . map outthroat'' . leftly 1 . flatten

outthroat'' :: (Flight, Loud) -> Loud
outthroat'' xy = applyWhen (length (fst xy) == 1 && only (fst xy) == dirty "h3")
  (shif [worths [Bear, Ring]] (queue [on Fore, offs [Lip, Back]])) (snd xy)

-- H > 0
unthroat :: Shift Bright
unthroat = queue [kill isThroat, workAll $ shif [isThroat, worth' Bear] (become $ dirty "i")]

-- R -> yr
unbreath :: Shift Bright
unbreath = queue [
  workAll $ shif [isDerm] (lifmap isThroat offbearThroat unbear),
  begetl (dirty "i") isDerm
 ]

-- m, n > w, y
soften :: Shift Bright
soften = workFirst $ shif
  [worth' Nose, not.worth' Bear]
  (queue [soften', ons [Thru, High, Tight], offs [Nose, Choke, Blade]])

soften' :: Shift Loud
soften' = lifmap (worth' Lip) (ons [Ring, Back]) (on Fore)

-- CHV > CCV
-- todo: should probably be VCHV > VCCV
throatsame :: Shift Bright
throatsame = makeBright . map throatsame' . bothly 1 1 . flatten

throatsame' :: (Flight, Loud, Flight) -> Loud
throatsame' ([l], m, [r]) = applyWhen
  (isThroat m && not (worth' Bear l) && worth' Bear r)
  (become l) m
throatsame' x = mid x

lengthen :: Shift Bright
lengthen = makeBright . map lengthen' . (rightly 1 . flatten)

lengthen' :: (Loud, Flight) -> Loud
lengthen' (l, [r]) = applyWhen (worth' Bear l && isThroat r) (on Long) l
lengthen' (l, _) = l

tideshift :: Shift Bright
tideshift = queue [id
  , nosesame.toothen
  , unspread
  , soften
  , gainly (stavefold.gainbear.nosesame.unbreath)
  , stavefold.gainbear.nosesame.unthroat
  , nosesame.unring
  , stavefold.gainbear.outthroat
  , stavefold.gainbear.lengthen
  , gainly nosesame.throatsame
 ]