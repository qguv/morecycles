\begin{code}
module Jumble where

import Data.List (sort, sortOn)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Sound.Tidal.Pattern
import Sound.Tidal.Utils (enumerate)

-- deterministic version of jumble, which takes a permutation number (for testing)
jumble' :: Integer -> Pattern Bool -> Pattern a -> Pattern a
jumble' 0 _ p = p
jumble' n mp p = undefined

\end{code}
