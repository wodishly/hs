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

data Kind = Unkind | Name | Deed
  deriving (Eq)

instance Show Kind where
  show m = "(" ++ k ++ ")" where
    k = case m of
      Unkind -> "-"
      Name -> "N"
      Deed -> "D"

-- [+main, +mean] = unm
-- [+main, -mean] = dep
-- [-main, +mean] = dat
-- [-main, -mean] = gen
-- main > mean
data Bendmark = Allmark | Quick | Man | Many | Main | Mean
  deriving (Eq)

-- todo: these some more
instance Mark Bendmark where
  axled = const True
  isSteadfast = const True
  below' Allmark = [Quick, Man, Many, Main, Mean]
  below' Main = [Mean]
  below' _ = []

instance Show Bendmark where
  show m = case m of
    Allmark -> "allmark"
    Quick -> "quick"
    Many -> "many"
    Man -> "man"
    Main -> "main"
    Mean -> "mean"

class Kindful a where
  kind :: a -> Kind

class Meaningful a where
  meaning :: a -> String

class Loudful a where
  louds :: a -> Flight
  showLouds :: a -> String

data Root = Root Flight String deriving (Eq)
data Stem = Stem (Either Root Stem) Flight Kind deriving (Eq)
data Ending = Ending Flight Shape deriving (Eq)

instance Kindful Stem where
  kind (Stem _ _ k) = k

instance Meaningful Root where
  meaning (Root _ s) = "\"" ++ s ++ "\""

instance Meaningful Stem where
  meaning (Stem rs _ _) = case rs of
    Left r -> meaning r
    Right s -> meaning s

instance Loudful Root where
  louds (Root ls _) = ls
  showLouds = ("√" ++).unstill.louds

instance Loudful Stem where
  louds (Stem rs ls _) = case rs of
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
  show r = showLouds r ++ " " ++ meaning r

-- todo: beautification
instance Show Stem where
  show s@(Stem rs _ k) = showLouds s ++ " " ++ show k ++ " " ++ x where
    x = case rs of
      Left r -> meaning r
      Right s -> meaning s

instance Show Ending where
  show e@(Ending _ sh) = showLouds e ++ " " ++ show sh

unroot :: Root
unroot = Root [] ""

unstem :: Stem
unstem = Stem (Left unroot) [] Unkind

unshape :: Shape
unshape = Branch Allmark True []

unending :: Ending
unending = Ending [] unshape

data Woord = Woord Bright Kind Shape String deriving (Eq)

instance Kindful Woord where
  kind (Woord _ k _ _) = k

instance Meaningful Woord where
  meaning (Woord _ _ _ str) = str

instance Loudful Woord where
  louds (Woord bs _ _ _) = flatten bs
  showLouds (Woord bs _ _ _) = cleans (flatten bs)

instance Show Woord where
  show w@(Woord _ k sh str) = showLouds w ++ " " ++ show k ++ " " ++ show sh

bendOne :: Ending -> Stem -> Woord
bendOne (Ending f sh) st
  = Woord ((queue shoals.makeBright.onbear.queue deeps) (louds st++f)) (kind st) sh (meaning st)

-- how my heart yearns within me
bend :: [[Ending]] -> Stem -> Board
bend endings = bend' endings (queue deeps) (queue shoals)

bend' :: [[Ending]] -> Shift Flight -> Shift Bright -> Stem -> Board
bend' endings deeps shoals stem
  = Board $ map (map (\e@(Ending _ sh) -> Woord ((shoals.makeBright.deeps) (louds stem ++ louds e)) (kind stem) sh (meaning stem))) endings

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
    (map (unwords.map (padR ((maximum.map lengthT)
                             (concatMap (map (makeBright.louds)) (shapes b))).show))
         (shapes b))

lemma :: Board -> Woord
lemma = head.head.shapes

lend :: [[Ending]] -> Stem -> Woord
lend endings = lemma.bend endings