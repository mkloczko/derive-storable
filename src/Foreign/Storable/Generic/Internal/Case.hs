{-# OPTIONS_GHC -fenable-rewrite-rules #-}

module Foreign.Storable.Generic.Internal.Case where

{-# INLINE case1 #-}
case1 :: [a] -> Int -> a
case1 (a:rest) ix = case ix of
    0 -> a
    _ -> error ("case1 used outside it's scope " ++ show ix)

{-# INLINE case2 #-}
case2 :: [a] -> Int -> a
case2 (a:b:rest) ix = case ix of
    0 -> a
    1 -> b
    _ -> error ("case2 used outside it's scope " ++ show ix) undefined 

{-# INLINE case3 #-}
case3 :: [a] -> Int -> a
case3 (a:b:c:rest) ix = case ix of
    0 -> a
    1 -> b
    2 -> c
    _ -> error ("case3 used outside it's scope " ++ show ix) undefined 

{-# INLINE case4 #-}
case4 :: [a] -> Int -> a
case4 (a:b:c:d:rest) ix = case ix of
    0 -> a
    1 -> b
    2 -> c
    3 -> d
    _ -> error ("case4 used outside it's scope " ++ show ix) undefined 

{-# INLINE case5 #-}
case5 :: [a] -> Int -> a
case5 (a:b:c:d:e:rest) ix = case ix of
    0 -> a
    1 -> b
    2 -> c
    3 -> d
    4 -> e
    _ -> error ("case5 used outside it's scope " ++ show ix) undefined 

{-# NOINLINE caseN #-}
caseN :: Int -> [a] -> Int -> a
caseN _ ls ix = ls !! ix

{-# RULES
  "caseN/1" forall ls ix. caseN 1 ls ix = case1 ls ix;  
  "caseN/2" forall ls ix. caseN 2 ls ix = case2 ls ix;  
  "caseN/3" forall ls ix. caseN 3 ls ix = case3 ls ix;
  "caseN/4" forall ls ix. caseN 4 ls ix = case4 ls ix;
  "caseN/5" forall ls ix. caseN 5 ls ix = case5 ls ix;
  -- Let the hack begin!
  #-}
