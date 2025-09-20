module Eldtung where

import Data.Bifunctor

import Mind
import Loud hiding (Root)
import Breath
import Word
import Shift
import Bend
import Mark

rootByMean :: String -> Root
rootByMean s = only $ filter (\x -> s == mean x) roothoard

roothoard :: [Root]
roothoard = map ((uncurry Root . first dirtys) . (\(x,y,z) -> (x,y))) allhoard

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
 ]

stemhoard :: [Stem]
stemhoard = map (\(l,m,y) -> Stem (dirtys l) y Unkind m) allhoard
--  map (\(x,y) -> Stem (loud (rootByMean y)) x Name y) [
--  (OYoke, "one")
--  , (OYoke, "two")
--  , (GYoke, "three")
--  , (RYoke, "four")
--  , (Unyoke, "five")
--  , (Unyoke, "six")
--  , (Unyoke, "seven")
--  , (Unyoke, "eight")
--  ]

-- todo: encode these as roots
endinghoard :: [[Flight]]
endinghoard = map (map dirtys) [
  ["s", "", "m", "ey", "eh1", "es", "es", "y"],
  ["es", "es", "ms", "bhys", "bhys", "bhys", "oh1om", "sw"]]

mend :: Stem -> Board
mend = bend endinghoard

zg :: Shift Flight
zg (a:b:c:rest) = lif (a == dirty "e" && not (any (worth' Bear) [b,c])) [] [a] ++ zg (b:c:rest)
zg xs = xs

-- infer yoke from stem shape
ken :: Flight -> Stem
ken ls = case clean (last ls) of
  "o" -> ken' 1 OYoke ls
  "r" -> ken' 2 RYoke ls
  "n" -> ken' 2 NYoke ls
  "y" -> ken' 2 GYoke ls
  "w" -> ken' 2 GYoke ls
  _   -> ken' 0 Unyoke ls

ken' :: Int -> Yoke -> Flight -> Stem
ken' n y ls = Stem (leave n ls) y Unkind ""