module Eldtung where

import Data.Bifunctor

import Mind
import Loud hiding (Root)
import Breath
import Shift
import Bend
import Mark

rootByMean :: String -> Root
rootByMean s = only $ filter (\x -> s == meaning x) roothoard

roothoard :: [Root]
roothoard = map ((uncurry Root . first dirtys) . nright) allhoard

-- todo: put this elsewhere
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
 ]

stemhoard :: [Stem]
stemhoard = map (\(l,m,y) -> Stem (Left (Root (dirtys l) m)) (bud y) Unkind) allhoard

--endinghoard :: [[Ending]]
--endinghoard = map (map (\x -> Ending (dirtys x) unshape)) [
--  ["s", "", "m", "ey", "eh1", "es", "es", "y"],
--  ["es", "es", "ms", "bhys", "bhys", "bhys", "oh1om", "sw"]]

endinghoard :: [[Ending]]
endinghoard = stack 2 $ map (\(ls, sh) -> Ending (dirtys ls) sh) [
   ("s", ons [Main] $ def Allmark)
 , ("m", ons [Main, Mean] $ def Allmark)
 , ("es", ons [] $ def Allmark)
 , ("ey", ons [Mean] $ def Allmark)
 , ("es", ons [Many, Main] $ def Allmark)
 , ("ms", ons [Many, Main, Mean] $ def Allmark)
 , ("oh1om", ons [Many] $ def Allmark)
 , ("bhys", ons [Many, Mean] $ def Allmark)
 ]

mend :: Stem -> Board
mend = bend endinghoard

zg :: Shift Flight
zg (a:b:c:rest) = lif (a == dirty "e" && not (any (worth' Bear) [b,c])) [] [a] ++ zg (b:c:rest)
zg xs = xs