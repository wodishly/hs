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
  putStrLn (assertively (== "√∅ \"\"") (show unroot))
  putStrLn (assertively (== "√∅-∅ (-) \"\"") (show unstem))
  putStrLn (assertively (== "-∅ [-unmark]") (show unending))
  print (assertively (== " (-) [-unmark]") (show $ bendOne unending unstem))
  putStrLn ""

  -- todo: the rest of these when im not lazy
  putStrLn (assertively (== "√gʸneh₃ \"know\"") (show fandroot))
  print fandstem
  print fanderstem
  print fandeststem
  print fandending
  putStrLn ""

  print (bendOne fandending fandstem)
  print (bend [[fandending]] fandstem)
  print (bend [[fandending, unending]] fandstem)
  print (bend [[fandending], [unending]] fandstem)
  putStrLn ""

  print (stemhoard!!1)
  print (mend (stemhoard!!1))
  print (lend [[unending]] (stemhoard!!1))
  forM_ (concatMap (map (cleans.flatten.tideshift.makeBright.louds))
                   ((shapes.mend.(!!1)) stemhoard)
        ) putStrLn