{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}

module Loud where

import Data.Char
import Data.Maybe
import Data.Tuple
import Data.Foldable
import Data.Function (applyWhen)
import Control.Monad

import Mind
import Token

type Flight = [Loud]

data Mark = Tung
          | Bear
            | Smooth | Nose
            | Throat | Stave | Spread | Clench
            | Long
          | Choke
            | Thru | Side | Step | Strong
            | Mouth | Lip | Ring | Blade | Far | Wide | Body | High | Fore | Back | Root | Low | Tight
  deriving (Eq)

data Branch = Branch {
  mark :: Mark,
  worth :: Bool,
  children :: [Branch]
} deriving (Eq)

type Loud = Branch

axled :: Mark -> Bool
axled = flip elem [Tung, Bear, Smooth, Throat, Choke, Thru, Mouth]

below :: Mark -> Mark -> Bool
below m m' = or $ sequence [elem m', any (flip below m')] (below' m)

below' :: Mark -> [Mark]
below' Tung = [Bear, Choke]
below' Bear = [Smooth, Throat, Long]
below' Smooth = [Nose]
below' Throat = [Stave, Spread, Clench]
below' Choke = [Thru, Mouth]
below' Thru = [Side, Step, Strong]
below' Mouth = [Lip, Blade, Body, Root]
below' Lip = [Ring]
below' Blade = [Far, Wide]
below' Body = [High, Fore, Back]
below' Root = [Low, Tight]
below' _ = []

above :: Mark -> Mark -> Bool
above = flip below

meanLoud :: Mark -> Loud
meanLoud m = Branch m True (map (off'.meanLoud) (below' m))

meanBear :: Loud
meanBear = ons [Bear, Smooth, Stave, Thru, Body, Low] $ meanLoud Tung

meanChoke :: Loud
meanChoke = ons [Choke] $ meanLoud Tung

unloud :: Loud
unloud = off Tung $ meanLoud Tung

leaf :: Branch -> Bool
leaf = null.children

-- (worth b) = b.worth
worth' :: Mark -> Loud -> Bool
worth' m' (Branch m w cs) = m==m' && w || any (worth' m') cs

worths :: [Mark] -> Loud -> Bool
worths ms l = all (flip worth' l) ms

isSteadfast :: Mark -> Bool
isSteadfast = flip elem [Bear, Choke, Thru]

isGlide :: Loud -> Bool
isGlide x = none (flip worth' x) [Bear, Choke]

isDerm :: Loud -> Bool
isDerm = worths [Bear, Choke]

isRough :: Loud -> Bool
isRough = not.worth' Smooth

isThroat :: Loud -> Bool
isThroat l = all ($ l) [not.worth' Root, worths [Thru, Body]]

onbearThroat :: Shift Loud
onbearThroat = ons [Bear, Smooth, Stave] . offs [Step, Strong]

offbearThroat :: Shift Loud
offbearThroat = offs [Bear, Smooth, Throat] . ons [Step, Strong]

on' :: Shift Loud
on' l = on (mark l) l

off' :: Shift Loud
off' l = off (mark l) l

un' :: Shift Loud
un' l = un (mark l) l

ons :: [Mark] -> Shift Loud
ons = (flip.foldr) on

offs :: [Mark] -> Shift Loud
offs = (flip.foldr) off

get :: Mark -> Loud -> Branch
get = (recklessly .) . get'

set :: Branch -> Shift Loud
set b l = if mark b == mark l
  then b
  else fandUp $ Branch (mark l) (worth l) (map (set b) (children l))

become :: Loud -> Shift Loud
become = const

-- todo: write fandDown, refactor off['/s]? as needed
fandUp :: Shift Branch
fandUp (Branch m w cs) = (\x -> Branch m (lif (isSteadfast m || not (any worth x)) w True) x)
                         (map fandUp cs)

on :: Mark -> Shift Loud
on n (Branch m w cs) = if m==n
  then Branch m True cs
  else fandUp (Branch m w $ map (on n) cs)

off :: Mark -> Shift Loud
off n (Branch m w cs) = if m==n
  then Branch m False (applyWhen (not (isSteadfast m)) (map off') cs)
  else Branch m w (map (off n) cs)

un :: Mark -> Shift Loud
un n (Branch m w cs) = if m==n
  then lif w off' on' (Branch m w cs)
  else Branch m w (map (un n) cs)

get' :: Mark -> Loud -> Maybe Branch
get' m l = if m == mark l
  then Just l
  else join $ find isJust $ map (get' m) (children l)

newtype Withmete = Withmete (Bool, Bool, Mark)
instance Show Withmete where
  show :: Withmete -> String
  show (Withmete (l, r, m)) = "[" ++ showWorth m l ++ "/" ++ showWorth m r ++ show m ++ "]"

withmete :: Loud -> Loud -> [Withmete]
withmete left right = lif (worth right /= worth (get (mark right) left))
  [Withmete (worth (get (mark right) left), worth right, mark right)]
  []
  ++ concatMap (withmete left) (children right)

dirty :: String -> Loud
dirty c = fromMaybe (error $ "dirty of /" ++ c ++ "/ not found")
                    (lookup (fromMaybe c (lookup c shades)) bundles)

clean :: Loud -> String
clean l = fromMaybe (error $ "clean of " ++ show l ++ " not found")
                    (lookup l (map swap bundles))

dirtys :: String -> Flight
dirtys x = map dirty (betoken x)

cleans :: Flight -> String
cleans = concatMap clean

instance Show Loud where
  show :: Loud -> String
  show l = init (show' 0 l)

-- todo: consider bundling mark -> bool into a `WorthedMark` type
showWorth :: Mark -> Bool -> String
showWorth m b = lif b "+" (lif (axled m) "-" "0")

show' :: Int -> Loud -> String
show' n (Branch m w cs)
  = concat (replicate n " ") ++ "[" ++ showWorth m w ++ show m ++ "]"
 ++ "\n" ++ (\x -> lif (not $ null x) x "") (concatMap (show' $ n+1) cs)

twishow :: Loud -> String
twishow l = init $ concatMap (\x -> uncurry (++) x ++ "\n")
                             (twifold "" $ (\x -> map (padR $ maximum $ map length x) x)
                                            (lines $ show l))

instance Show Mark where
  show :: Mark -> String
  show m = applyWhen (axled m) (map toUpper) $ case m of
    Tung -> "tung"
    Bear -> "bear"
    Smooth -> "smooth"
    Nose -> "nose"
    Throat -> "throat"
    Stave -> "stave"
    Spread -> "spread"
    Clench -> "clench"
    Choke -> "choke"
    Thru -> "thru"
    Side -> "side"
    Step -> "step"
    Strong -> "strong"
    Mouth -> "mouth"
    Lip -> "lip"
    Ring -> "ring"
    Blade -> "blade"
    Far -> "far"
    Wide -> "wide"
    Body -> "body"
    High -> "high"
    Fore -> "fore"
    Back -> "back"
    Root -> "root"
    Low -> "low"
    Tight -> "tight"
    Long -> "long" -- todo: i am fearful

-- reach with `clean` and `dirty`
bundles :: [(String, Loud)]
bundles = [
   ("ʔ", meanChoke)

 , ("p", ons [Lip] $ dirty "ʔ")
 , ("t", ons [Blade] $ dirty "ʔ")
 , ("k", ons [High] $ dirty "ʔ")
 , ("kʸ", ons [Fore] $ dirty "k")
 , ("kʷ", ons [Ring] $ dirty "k")

 , ("b", ons [Stave] $ dirty "p")
 , ("d", ons [Stave] $ dirty "t")
 , ("g", ons [Stave] $ dirty "k")
 , ("gʸ", ons [Fore] $ dirty "g")
 , ("gʷ", ons [Ring] $ dirty "g")

 , ("bʰ", ons [Spread] $ dirty "b")
 , ("dʰ", ons [Spread] $ dirty "d")
 , ("gʰ", ons [Spread] $ dirty "g")
 , ("gʸʰ", ons [Spread, Fore] $ dirty "g")
 , ("gʷʰ", ons [Spread, Ring] $ dirty "g")

 , ("f", ons [Thru, Step] $ dirty "p")
 , ("θ", ons [Thru, Step] $ dirty "t")
 , ("ð", ons [Stave] $ dirty "θ")

 , ("s", ons [Thru, Step, Strong] $ dirty "t")
 , ("z", ons [Stave] $ dirty "s")
 , ("ɬ", ons [Thru, Step, Side] $ dirty "t")
 , ("ɮ", ons [Stave] $ dirty "ɬ")
 , ("h₂", ons [Body] $ offs [Blade] $ dirty "s")
 , ("h₁", ons [Fore] $ dirty "h₂")
 , ("h₃", ons [Ring] $ dirty "h₂")
 , ("h", ons [Thru, Step] $ dirty "ʔ")

 , ("m", ons [Nose] $ dirty "b")
 , ("n", ons [Nose] $ dirty "d")
 , ("l", ons [Side] $ dirty "r")
 , ("r", ons [Thru] $ offs [Nose] $ dirty "n")
 , ("y", offs [Bear] $ dirty "i")
 , ("w", offs [Bear] $ dirty "u")

 , ("ə₂", onbearThroat $ dirty "h₂")
 , ("ə₁", onbearThroat $ dirty "h₁")
 , ("ə₃", onbearThroat $ dirty "h₃")
 , ("m̩", ons [Bear] $ dirty "m")
 , ("n̩", ons [Bear] $ dirty "n")
 , ("l̩", ons [Bear] $ dirty "l")
 , ("r̩", ons [Bear] $ dirty "r")
 , ("i", ons [High] $ offs [Low] $ dirty "e")
 , ("u", ons [High] $ offs [Low] $ dirty "o")

 , ("e", ons [      Fore, Tight] $ offs [Low] $ dirty "a")
 , ("o", ons [Ring, Back, Tight] $ offs [Low] $ dirty "a")
 , ("a", meanBear)

 , ("m̩̄", ons [Long] $ dirty "m̩")
 , ("n̩̄", ons [Long] $ dirty "n̩")
 , ("l̩̄", ons [Long] $ dirty "l̩")
 , ("r̩̄", ons [Long] $ dirty "r̩")
 , ("ī", ons [Long] $ dirty "i")
 , ("ū", ons [Long] $ dirty "u")
 , ("ē", ons [Long] $ dirty "e")
 , ("ō", ons [Long] $ dirty "o")
 , ("ā", ons [Long] $ dirty "a")
 ]