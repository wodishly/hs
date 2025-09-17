{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}

module Bend where

import Prelude hiding (Word)

import Mind
import Token
import Loud
import Breath
import Word
import Shift

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