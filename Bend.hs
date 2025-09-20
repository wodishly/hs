{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use section" #-}

module Bend where

import Prelude hiding (Word)

import Mind
import Token
import Loud hiding (Root)
import Breath
import Word
import Shift
import Mark

data Bendmark = Quick | Many
  deriving (Eq)

instance Mark Bendmark where
  axled :: Bendmark -> Bool
  axled = const True
  below :: Bendmark -> Bendmark -> Bool
  below m m' = False
  above :: Bendmark -> Bendmark -> Bool
  above = flip below
  isSteadfast :: Bendmark -> Bool
  isSteadfast = const False

instance Show Bendmark where
  show :: Bendmark -> String
  show m = case m of
    Quick -> "quick"
    Many -> "many"


class Meaningful a where
  loud :: a -> Flight
  mean :: a -> String

data Steam = Steam Root (Branch Bendmark) String
           | Rooot Flight String deriving (Eq)

data Root = Root Flight String deriving (Eq)

instance Meaningful Root where
  loud :: Root -> Flight
  loud (Root f _) = f
  mean :: Root -> String
  mean (Root _ s) = s

data Stem = Stem {
  root :: Flight,
  yoke :: Yoke,
  kind :: Kind,
  meaning :: String
} deriving (Eq)

instance Meaningful Stem where
  loud :: Stem -> Flight
  loud (Stem r _ _ _) = r
  mean :: Stem -> String
  mean (Stem _ _ _ m) = m

data Ending = Ending Flight [Loudmark] deriving (Eq)

instance Meaningful Ending where
  loud :: Ending -> Flight
  loud (Ending f _) = f
  mean :: Ending -> String
  mean (Ending _ ms) = show ms


unroot :: Root
unroot = Root [] ""

unstem :: Stem
unstem = Stem [] Unyoke Unkind ""


instance Show Root where
  show :: Root -> String
  show (Root l m) = "√" ++ cleans l ++ " \"" ++ m ++ "\""

instance Show Stem where
  show :: Stem -> String
  show (Stem r y k m) = cleans r ++ "-" ++ cleans (bud y) ++ "- "
                     ++ "(" ++ show k ++ ") \"" ++ m ++ "\""



-- how my heart yearns within me
bend :: [[Flight]] -> Stem -> Board
bend endings = bend' endings (queue deeps) (queue shoals)

bend' :: [[Flight]] -> Shift Flight -> Shift Word -> Stem -> Board
bend' endings deeps shoals stem = Board $ map (map (\y -> shoals $ makeWord $ deeps (root stem ++ bud (yoke stem) ++ y))) endings

bendDeep :: [[Flight]] -> Stem -> Board
bendDeep endings = bend' endings (queue deeps) id

bendShoal :: [[Flight]] -> Stem -> Board
bendShoal endings = bend' endings id (queue shoals)

deeps :: [Shift Flight]
deeps = [id]--[onbear, zg]

shoals :: [Shift Word]
shoals = [id]

newtype Board = Board { shapes :: [[Word]] }

instance Show Board where
  show :: Board -> String
  show (Board words) = (init.unlines)
                    (map (unwords . map (padR ((maximum . map lengthT . concat) words) . show)) words)

lemma :: Board -> Word
lemma = head.head.shapes

lend :: [[Flight]] -> Stem -> Word
lend endings stem = lemma (bend endings stem)