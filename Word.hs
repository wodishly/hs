{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
module Word where

import Prelude hiding (Word)
import Data.Maybe
import Data.Bifunctor

import Mind
import Loud hiding (Root)
import Breath
import Mark

newtype Word = Word { breaths :: [Breath] }

instance Show Word where
  show :: Word -> String
  show = cleans.flatten



-- todo: put this elsewhere
data Yoke = OYoke | GYoke | RYoke | NYoke | TYoke | Unyoke deriving (Eq)

data Kind = Name | Deed | Unkind deriving (Eq)

instance Show Kind where
  show :: Kind -> String
  show Name = "N"
  show Deed = "D"
  show Unkind = "-"

--sprout :: Kind -> Yoke -> Root -> Stem
--sprout k y r = Stem (loud r) y k (mean r)

-- flatten prosodic structure of word into flight of loudness
flatten :: Word -> Flight
flatten (Word (b:bs)) = concat (thrifork b++[flatten (Word bs)])
flatten w = []

-- todo: likely useless
bud :: Yoke -> Flight
bud OYoke = dirtys "o"
bud GYoke = dirtys "ey"
bud RYoke = dirtys "er"
bud NYoke = dirtys "en"
bud _ = []

-- nudge the breathbreak so that the bad onset becomes a good offset
nudge :: Shift Word
nudge (Word (a:b:rest))
  = Word ((\(o, b') -> shiftOffset (++o) a
                     : breaths (nudge $ Word (maybeToList b'++rest)))
          (trimBadOnset b))
nudge w = w

trimBadOnset :: Breath -> (Onset, Maybe Breath)
trimBadOnset b
  | (not.hasRime) b = (onset b, Nothing)
  | (isGoodOnset.onset) b = ([], Just b)
  | otherwise = first ([(head.onset) b] ++) (trimBadOnset $ shiftOnset tail b)

-- todo: an actual version of this
isGoodOnset :: Onset -> Bool
isGoodOnset o = implies (length o > 1) (elem o (map dirtys ["t", "tp"]))

makeWord :: Flight -> Word
makeWord = nudge . Word . map makeBreath . split (worth' Bear)

-- in loudness
lengthL :: Word -> Int
lengthL = length' id

-- in tokens
lengthT :: Word -> Int
lengthT = length' cleans

length' :: Foldable f => (Flight -> f a) -> Word -> Int
length' f (Word bs)
  = sum (map (\b -> (sum . map (length . (\g -> (f.g) b))) thrifork') bs)