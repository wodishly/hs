module Fand where

import Loud hiding (Root)
import Bend

fandroot :: Root
fandroot = Root (dirtys "gyneh3") "know"

fandstem :: Stem
fandstem = Stem (Left fandroot) (dirtys "to") Name

allfand :: IO ()
allfand = do
  print fandroot
  print fandstem