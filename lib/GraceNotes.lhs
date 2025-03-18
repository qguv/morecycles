\subsection{gracenotes}

In Western music, 
grace notes are quick, ornamental notes that precede a main note, adding expressiveness and variation.
These notes do not affect the rhythm but add embellishment to a melody.

It would be nice if musicians could specify where grace notes should be added,
allowing for dynamic and varied performances. We call this function \texttt{gracenotes}.

\texttt{gracenotes} takes two patterns as input:
a content pattern (Pattern a) and a mask pattern (Pattern Bool). 
The function produces an output pattern where any event that overlaps with a `True` in the mask pattern
is duplicated with an additional grace note occurring just before it.

