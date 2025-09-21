module Fand where

import Loud hiding (Root)
import Bend
import Mark
import Eldtung
import Breath
import Tide
import Shift
import Mind
import Control.Monad

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
      .shapes.bend endinghoard) stemhoard)