{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
module Word where

import Prelude hiding (Word)
import Data.Maybe
import Data.Bifunctor

import Mind
import Loud hiding (Root)
import Breath

newtype Word = Word { breaths :: [Breath] }

instance Show Word where
  show :: Word -> String
  show = cleans.flatten

-- flatten prosodic structure of word into flight of loudness
flatten :: Word -> Flight
flatten (Word (b:bs)) = concat (thrifork b++[flatten (Word bs)])
flatten w = []

class Meaningful a where
  loud :: a -> Flight
  mean :: a -> String

data Root = Root Flight String deriving (Eq)

instance Show Root where
  show :: Root -> String
  show (Root l m) = "√" ++ cleans l ++ " \"" ++ m ++ "\""

instance Meaningful Root where
  loud :: Root -> Flight
  loud (Root f s) = f
  mean :: Root -> String
  mean (Root f s) = s

data Ending = Ending Flight [Mark] deriving (Eq)
instance Meaningful Ending where
  loud :: Ending -> Flight
  loud (Ending f ms) = f
  mean :: Ending -> String
  mean (Ending f ms) = show ms

-- todo: put this elsewhere
data Yoke = OYoke | GYoke | RYoke | NYoke | TYoke | Unyoke deriving (Eq)

data Kind = Name | Deed | Unkind deriving (Eq)

instance Show Kind where
  show :: Kind -> String
  show Name = "N"
  show Deed = "D"
  show Unkind = "-"

data Stem = Stem {
  root :: Flight,
  yoke :: Yoke,
  kind :: Kind,
  meaning :: String
} deriving (Eq)

unroot :: Root
unroot = Root [] ""

unstem :: Stem
unstem = Stem [] Unyoke Unkind ""

instance Show Stem where
  show :: Stem -> String
  show (Stem r y k m) = cleans r ++ "-" ++ cleans (bud y) ++ "- "
                     ++ "(" ++ show k ++ ") \"" ++ m ++ "\""

instance Meaningful Stem where
  loud :: Stem -> Flight
  loud (Stem r y k m) = r
  mean :: Stem -> String
  mean (Stem r y k m) = m

--sprout :: Kind -> Yoke -> Root -> Stem
--sprout k y r = Stem (loud r) y k (mean r)

-- todo: likely useless
bud :: Yoke -> Flight
bud OYoke = dirtys "o"
bud GYoke = dirtys "ey"
bud RYoke = dirtys "er"
bud NYoke = dirtys "en"
bud _ = []

-- infer yoke from stem shape
ken :: Flight -> Stem
ken ls
  | kenL' "o" ls = kenR' "o" ls
  | otherwise = Stem ls Unyoke Unkind ""

kenL' :: String -> Flight -> Bool
kenL' s ls = ends ls $ dirtys s

kenR' :: String -> Flight -> Stem
kenR' s ls = Stem (leave (length (dirtys s)) ls) OYoke Unkind ""

-- nudge the breathbreak so that the bad onset becomes a good offset
nudge :: Shift Word
nudge (Word (a:b:rest))
  = Word ((\(o, b') -> Breath (onset a)
                              (Rime (inset (rime a)) (offset (rime a) ++ o))
                              False
                       : breaths (nudge $ Word (maybeToList b'++rest)))
          (trimBadOnset b))
nudge w = w

trimBadOnset :: Breath -> (Onset, Maybe Breath)
trimBadOnset b@(Breath o r _)
  | not $ hasRime b = (o, Nothing)
  | isGoodOnset o = ([], Just b)
  | otherwise = first ([head o] ++) $ trimBadOnset $ Breath (tail o) r False

-- todo: an actual version of this
isGoodOnset :: Onset -> Bool
isGoodOnset o = implies (length o > 1) (elem o (map dirtys ["t", "tp"]))

makeWord :: Flight -> Word
makeWord = nudge . Word . map makeBreath . split (worth' Bear)

lengthL :: Word -> Int
lengthL = length' id

lengthT :: Word -> Int
lengthT = length' cleans

length' :: Foldable f => (Flight -> f a) -> Word -> Int
length' f (Word bs)
  = sum (map (\b -> (sum . map (length . (\g -> (f.g) b))) thrifork') bs)