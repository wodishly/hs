{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use section" #-}

module Bend where

import Mind
import Mark
import Token
import Loud hiding (Root)
import Breath
import Shift

type Shape = Branch Bendmark

data Kind = Unkind | Name | Deed
  deriving (Eq)

instance Show Kind where
  show m = "(" ++ k ++ ")" where
    k = case m of
      Unkind -> "-"
      Name -> "N"
      Deed -> "D"

data Bendmark = Bow
  | Quick | Were
  | Many | More
  | Main | Mean
  deriving (Eq)

instance Mark Bendmark where
  axled = const True
  isSteadfast = const True
  below' Bow = [Quick, Were, Many, More, Main, Mean]
  below' Quick = [Were]
  below' Many = [More]
  below' Main = [Mean]
  below' _ = []

instance Show Bendmark where
  show m = case m of
    Bow -> "Bow"
    Quick -> "quick"
    Were -> "were"
    Many -> "many"
    More -> "more"
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
  meaning (Stem rs _ _) = either meaning meaning rs

instance Loudful Root where
  louds (Root ls _) = ls
  showLouds = ("√" ++).unstill.louds

instance Loudful Stem where
  louds (Stem rs ls _) = either (flip (++) ls . louds) (flip (++) ls . louds) rs
  showLouds (Stem rs ls _) = either showLouds showLouds rs ++ "-" ++ unstill ls

instance Loudful Ending where
  louds (Ending ls _) = ls
  showLouds = ("-" ++).unstill.louds

instance Show Root where
  show r = showLouds r ++ " " ++ meaning r

-- todo: beautification
instance Show Stem where
  show s = showLouds s ++ " " ++ show (kind s) ++ " " ++ meaning s

instance Show Ending where
  show e@(Ending _ sh) = showLouds e ++ " " ++ show sh

unroot :: Root
unroot = Root [] ""

unstem :: Stem
unstem = Stem (Left unroot) [] Unkind

unshape :: Shape
unshape = Branch Bow True []

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

bend :: [[Ending]] -> Stem -> Board
bend es = bend' es (queue deeps) (queue shoals)

-- todo: this is horrific
bend' :: [[Ending]] -> Shift Flight -> Shift Bright -> Stem -> Board
bend' es ds ss stem
  = Board (map (map (\e@(Ending _ sh) ->
    Woord ((ss.makeBright.ds) (louds stem ++ louds e))
          (kind stem) sh (meaning stem)
    )) es)

bendDeep :: [[Ending]] -> Stem -> Board
bendDeep es = bend' es (queue deeps) id

bendShoal :: [[Ending]] -> Stem -> Board
bendShoal es = bend' es id (queue shoals)

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