{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use section" #-}

module Token where

import Mind

shades :: [(String, String)]
shades = [
   ("th", "θ")
 , ("dd", "ð") -- todo: fraught!
 , ("lh", "ɬ")
 , ("lz", "ɮ")

 , ("bh", "bʰ")
 , ("dh", "dʰ")
 , ("gh", "gʰ")

 , ("c", "kʸ")
 , ("ky", "kʸ")
 , ("q", "kʷ")
 , ("kw", "kʷ")

 , ("j", "gʸ")
 , ("gy", "gʸ")
 , ("v", "gʷ")
 , ("gw", "gʷ")

 , ("jh", "gʸʰ")
 , ("gyh", "gʸʰ")
 , ("vh", "gʷʰ")
 , ("gwh", "gʷʰ")

 , ("h1", "h₁")
 , ("h2", "h₂")
 , ("h3", "h₃")

 , ("ng", "ŋ")
 , ("ngw", "ŋʷ")

 , ("@1", "ə₁")
 , ("@2", "ə₂")
 , ("@3", "ə₃")

 , ("M", "m̩")
 , ("N", "n̩")
 , ("L", "l̩")
 , ("R", "r̩")

 , ("MM", "m̩̄")
 , ("NN", "n̩̄")
 , ("LL", "l̩̄")
 , ("RR", "r̩̄")

 , ("A", "ā")
 , ("E", "ē")
 , ("I", "ī")
 , ("O", "ō")
 , ("U", "ū")
 ]

sharps :: [(String, String)]
sharps = [
   ("a", "á")
 , ("e", "é")
 , ("i", "í")
 , ("o", "ó")
 , ("u", "ú")
 ]

--longs :: [(String, String)]
--longs = [
--   ("a", "ā")
-- , ("e", "ē")
-- , ("i", "ī")
-- , ("o", "ō")
-- , ("u", "ū")
-- , ("m̩", "m̩̄")
-- , ("n̩", "n̩̄")
-- , ("l̩", "l̩̄")
-- , ("r̩", "r̩̄")
-- , ("ə₁", "ə̄₁")
-- , ("ə₂", "ə̄₂")
-- , ("ə₃", "ə̄₃")
-- ]

shadesOf :: Int -> [(String, String)]
shadesOf n = filter (\x -> length (fst x) == n) shades

betoken :: Shell String
betoken s = filter (/= ".") (betoken' (maxr (map (length.fst) shades)) (map shell s))

-- inwend on length of n-graph
betoken' :: Int -> Shift [String]
betoken' 0 s = s
betoken' n s = betoken' (n-1) (betoken'' n s)

-- leftfare through `s` on `n`
betoken'' :: Int -> Shift [String]
betoken'' n [] = []
betoken'' n s = lif (length s >= n)
  (lif (elem (concat (scoop n s)) (map fst (shadesOf n)))
    (betoken'' n (leave n s) ++ [concat (scoop n s)])
    (betoken'' n (init s) ++ [last s]))
  s
-- todo: if runtime becomes bad then think about reviving this implementation

--betoken :: String -> [String]
--betoken (first:rest) = filter ("" /=) $ lif ([first] : betoken rest)
--                     ((\(css,cs) -> css ++ betoken cs) (await (first:rest)))
--                     (full $ awaitors first)
--betoken [] = []

--awaitors :: Char -> [String]
--awaitors c = ly' ("awaitors of "++[c],) $ filter (flip begins [c]) (map fst shades)
--
--await :: String -> ([String], String)
--await (first:rest) = ly' ("awaiting "++(first:rest),) $
--  lif ([[first]], rest)
--      (lif ([fromMaybe (ly' (const "oh no") first:[head rest])
--                       (lookup (ly' (const ("lookup",first, head rest)) (first:[head rest])) shades)]
--             , lif [] (tail rest) (full rest))
--           ((\(x, y) -> lif (ly' ("nelem",) ([[first]], concat x ++ y))
--                            (ly' ("yelem",) ([recklessly $ lookup [first, head $ head x] shades, tail $ head x], y))
--                            (ly' ("?elem "++[first]++concat x,) $ elem (first:head x) (map fst shades))) (await rest))
--           (full (awaitors $ head rest)))
--      (ly' (const ("fullness check", rest)) $ full rest)
--await _ = ([], [])
--
---- "bb" -> await ['b', 'b'] -> awaiting… ['b'] await ['b'] -> awaiting… ['b'] ['b'] -> ['b', 'b']
---- "bh" -> await ['b', 'h'] -> awaiting… ['b'] await ['h'] -> awaiting… ['b'] ['h'] -> ['bh']
---- "bh1" -> await ['b', 'h', '1'] -> awaiting… ['b'] await ['h', '1']
----       -> awaiting… ['b'] awaiting… ['h'] await ['1']
----       -> awaiting… ['b'] awaiting… ['h'] ['1'] -> awaiting… ['b'] ['h1'] -> ['b', h1']
--
--