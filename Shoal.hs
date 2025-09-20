import Prelude hiding (Word)

import Control.Monad
import Control.Exception

import Mind
import Token
import Mark
import Loud
import Breath
import Word 
import Shift
import Bend
import Eldtung
import Tide

--main :: IO ()
--main = forM_ [0..(length bundles-1)]
--           $ \i -> assert (fst (bundles!!i) == (clean.dirty.fst $ bundles!!i))
--                          (putStrLn $ "`dirty.clean` == `id` at /" ++ fst (bundles!!i) ++ "/")