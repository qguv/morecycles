module I where

import Data.Map

import Sound.Tidal.Pattern
import Sound.Tidal.Show
import Sound.Tidal.UI
import Sound.Tidal.Core
import Sound.Tidal.ParseBP

import SwingTime
import Jumble
import GraceNotes
import Polyshift
import jitter
import RhythMask

{-
string to pattern, e.g.
    ghci> s2p "[a b c d]" :: Pattern String
    (0>¼)|"a"
    (¼>½)|"b"
    (½>¾)|"c"
    (¾>1)|"d"
-}
s2p :: (Enumerable a, Parseable a) => String -> Pattern a 
s2p = parseBP_E

{-
pattern to list of events within one cycle (the first), e.g.
    ghci> let m = s2p "[1 0 1 0]" :: Pattern Bool
    ghci> let p = s2p "[a b c d]" :: Pattern String
    ghci> p2e $ jumble' 1 m p
    [(0>¼)|"a",(½>¾)|"c",(¼>½)|"d",(¾>1)|"b"]
-}
p2e :: Pattern a -> [Event a]
p2e Pattern{query=q} = q (State (Arc 0 1) Data.Map.empty)

{-
string to list of events within one cycle (the first)
    ghci> s2e "[a b c d]" :: [Event String]
    [(0>¼)|"a",(¼>½)|"b",(½>¾)|"c",(¾>1)|"d"]
-}
s2e :: (Enumerable a, Parseable a) => String -> [Event a]
s2e = p2e . s2p
