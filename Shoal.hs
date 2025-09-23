{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}

import Prelude hiding (Word)

import Control.Monad
import Control.Exception

import Mind
import Token
import Mark
import Loud hiding (Root)
import Breath
import Shift
import Bend
import Eldtung
import Fand

allshow :: String -> String
allshow = twishow . map (cleans.flatten) . concat . allwork

allwork :: String -> [[Bright]]
allwork = map (map (tideshift.makeBright.louds)) . shapes . mend
  . flip (flip Stem []) Unkind . Left . flip Root "" . dirtys