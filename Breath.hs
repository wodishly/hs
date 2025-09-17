module Breath where

import Data.List

import Mind
import Token
import Loud

type Onset = [Loud]
type Inset = [Loud]
type Offset = [Loud]

data Rime = Rime {
  inset :: Inset,
  offset :: Offset
} deriving (Eq)

instance Show Rime where
  show :: Rime -> String
  show (Rime i o) = "(" ++ concatMap cleans [i,o] ++ ")"

unrime :: Rime
unrime = Rime [] []

data Breath = Breath {
  onset :: Onset,
  rime :: Rime,
  sharp :: Bool
} deriving (Eq)

instance Show Breath where
  show :: Breath -> String
  show (Breath o r s) = "(" ++ cleans o ++ stuff ++ ")"
    where stuff = if s
                    then "(" ++ recklessly (lookup (clean $ head (inset r)) sharps)
                             ++ concatMap cleans [tail (inset r), offset r] ++ ")"
                    else show r

hasOnset :: Breath -> Bool
hasOnset = full.onset

hasInset :: Breath -> Bool
hasInset = full.inset.rime

hasOffset :: Breath -> Bool
hasOffset = full.offset.rime

hasRime :: Breath -> Bool
hasRime b = hasInset b || hasOffset b

isHeavy :: Breath -> Bool
isHeavy = hasOffset

isLight :: Breath -> Bool
isLight = not.isHeavy

makeRime :: Flight -> Rime
makeRime ls = case findIndex (not.worth' Bear) ls of
  Just n -> uncurry Rime (splitAt n ls)
  Nothing -> Rime ls []

makeBreath :: Flight -> Breath
makeBreath ls = case findIndex (worth' Bear) ls of
  Just n -> (\x -> Breath (fst x) (makeRime $ snd x) False) (splitAt n ls)
  Nothing -> Breath ls unrime False

-- todo: can probably use functors to beautify these guys

thrifork :: Breath -> [Flight]
thrifork b = filter full (map ($ b) thrifork')

thrifork' :: [Breath -> Flight]
thrifork' = [onset, inset.rime, offset.rime]