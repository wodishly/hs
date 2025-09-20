{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use section" #-}

module Bend where

import Mind
import Token
import Loud hiding (Root)
import Breath
import Shift
import Mark

type Shape = Branch Bendmark

unshape :: Shape
unshape = Branch Unmark False []

data Kind = Unkind | Name | Deed
  deriving (Eq)

instance Show Kind where
  show m = "(" ++ k ++ ")" where
    k = case m of
      Unkind -> "-"
      Name -> "N"
      Deed -> "D"

data Bendmark = Unmark | Quick | Many
  deriving (Eq)

-- todo: these
instance Mark Bendmark where
  axled = const True
  below m m' = False
  isSteadfast = const False

instance Show Bendmark where
  show m = case m of
    Unmark -> "unmark"
    Quick -> "quick"
    Many -> "many"

class Kindful a where
  kind :: a -> Kind

class Meanful a where
  mean :: a -> String

class Loudful a where
  louds :: a -> Flight
  showLouds :: a -> String

data Root = Root Flight String deriving (Eq)
data Stem = Stem (Either Root Stem) Flight Kind deriving (Eq)
data Ending = Ending Flight Shape deriving (Eq)

instance Kindful Stem where
  kind (Stem rs f k) = k

instance Meanful Root where
  mean (Root ls s) = "\"" ++ s ++ "\""

instance Meanful Stem where
  mean (Stem rs ls k) = case rs of
    Left r -> mean r
    Right s -> mean s

instance Loudful Root where
  louds (Root ls _) = ls
  showLouds = ("√" ++).unstill.louds

instance Loudful Stem where
  louds (Stem rs ls k) = case rs of
    Left r -> louds r++ls
    Right s -> louds s++ls
  showLouds (Stem rs ls _) = x ++ "-" ++ unstill ls where
    x = case rs of
      Left r -> showLouds r
      Right s -> showLouds s

instance Loudful Ending where
  louds (Ending ls _) = ls
  showLouds = ("-" ++).unstill.louds

instance Show Root where
  show r = showLouds r ++ " " ++ mean r

-- todo: beautification
instance Show Stem where
  show s@(Stem rs _ k) = showLouds s ++ " " ++ show k ++ " " ++ x where
    x = case rs of
      Left r -> mean r
      Right s -> mean s

instance Show Ending where
  show e@(Ending _ sh) = showLouds e ++ " " ++ show sh

unroot :: Root
unroot = Root [] ""

unstem :: Stem
unstem = Stem (Left unroot) [] Unkind

unending :: Ending
unending = Ending [] unshape

data Woord = Woord Bright Kind Shape String deriving (Eq)

instance Kindful Woord where
  kind (Woord bs k sh str) = k

instance Meanful Woord where
  mean (Woord bs k sh str) = str

instance Loudful Woord where
  louds (Woord bs k sh str) = flatten bs
  showLouds (Woord bs k sh str) = cleans (flatten bs)

instance Show Woord where
  show w@(Woord bs k sh str) = showLouds w ++ " " ++ show k ++ " " ++ show sh

bendOne :: Ending -> Stem -> Woord
bendOne (Ending f sh) st
  = Woord ((queue shoals.makeBright.onbear.queue deeps) (louds st++f)) (kind st) sh (mean st)

-- how my heart yearns within me
bend :: [[Ending]] -> Stem -> Board
bend endings = bend' endings (queue deeps) (queue shoals)

bend' :: [[Ending]] -> Shift Flight -> Shift Bright -> Stem -> Board
bend' endings deeps shoals stem
  = Board $ map (map (\e@(Ending ls sh) -> Woord ((shoals.makeBright.deeps) (louds stem ++ louds e)) (kind stem) sh (mean stem))) endings

bendDeep :: [[Ending]] -> Stem -> Board
bendDeep endings = bend' endings (queue deeps) id

bendShoal :: [[Ending]] -> Stem -> Board
bendShoal endings = bend' endings id (queue shoals)

deeps :: [Shift Flight]
deeps = [id]--[onbear, zg]

shoals :: [Shift Bright]
shoals = [id]

newtype Board = Board { shapes :: [[Woord]] }

instance Show Board where
  show b = (init.unlines)
    (map (unwords.map (padR ((maximum.map lengthT) (concatMap (map (makeBright.louds)) (shapes b))).show)) (shapes b))

lemma :: Board -> Woord
lemma = head.head.shapes

lend :: [[Ending]] -> Stem -> Woord
lend endings stem = lemma (bend endings stem)