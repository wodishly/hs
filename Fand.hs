module Fand where

import Control.Monad

import Mind
import Mark
import Loud hiding (Root)
import Breath
import Shift
import Bend
import Eldtung

fandroot :: Root
fandroot = Root (dirtys "gyneh3") "know"

fandstem :: Stem
fandstem = Stem (Left fandroot) (dirtys "to") Name

fanderstem :: Stem
fanderstem = Stem (Right fandstem) (dirtys "to") Name

fandeststem :: Stem
fandeststem = Stem (Right fanderstem) (dirtys "to") Name

fandending :: Ending
fandending = Ending (dirtys "syo") (Branch Quick True [Branch Many True []])

allfand :: IO ()
allfand = do
  mapM_ putStrLn (concatMap (concatMap
    (map (cleans.flatten.tideshift.makeBright.louds))
      .shapes.mend) stemhoard)

stavefoldfand :: IO ()
stavefoldfand = foldl (>>) mempty
  (map (print.stavefold.makeBright.dirtys)
  ["snom", "snoom", "snooom", "snoooom",
   "snōm", "snoōm", "snōom", "snoōom",
   "o", "oo", "ō", "ōo", "oō", "ōō", 
   "owo", "owō", "ōwo", "ōwō"])