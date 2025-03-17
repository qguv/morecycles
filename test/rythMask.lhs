{-# OPTIONS_GHC -Wno-orphans #-}

"RhythMask": Probability-Based Masking
The idea behind RhythMask is to create a rhythmic effect where some beats are 
probabilistically dropped or kept each cycle, 
rather than being strictly fixed by a binary mask.

Theoretical implementation idea
Input:
-> A content pattern (Pattern a): The original rhythmic sequence.
-> A probability pattern (Pattern Double): A probability value (between 0.0 and 1.0) 
that determines the likelihood of each beat being played.

Output: A modified version of the input pattern with beats dropped probabilistically

Steps of implementation:
->Extract Events: The function first queries the input pattern to get a list of its events.
->Apply Probability Filtering: Each event is evaluated against the corresponding probability value.
->Random Decision Making: A random number is generated for each event, and if it is below the 
probability threshold, the event passes on to the final output, otherwise it is removed.
->Reconstruction of the Pattern: The remaining beats are reconstructed into a new TidalCycles pattern.