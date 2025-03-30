\begin{code}
module PolyShift where

-- plays a pattern only on the kth cycle

playOnly n k p = every' n k (<> p) "~" 

{-
The goal is that the below functions will begin with the pattern playing normally during the first cycle. Then it
should delay the start time the next cycle, starting the delay over at the beginning of the cycle if it exceeds one.
This will terminate when it reaches exactly one, i.e., when in n steps, where n is the denominator of the delay time,
since time is represented as fraction of cycles.
-}

helper 0 _ _ _ _ _ = "~"
helper j k l m n p
    | l > 1  = playOnly k m $ (~> (l-1)) p <> helper (j-1) k (l+n) m n p
    | l <= 1 = playOnly k (m+1) $ (~> (l-1)) p <> helper (j-1) k ((l-1)+n) (m+1) n p

polyShift n p = helper (denominator n) (denominator n) 0 0 n p

\end{code}
