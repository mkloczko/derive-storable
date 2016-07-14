{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE DefaultSignatures #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE UndecidableInstances #-}

module Foreign.Storable.Generic.Tools (
    Filling (..), 
    getFilling,
    sumFilling,
    calcAlignment, 
    calcOffsets,
    calcSize
) where


-- Calculation of offsets.

-- | The datatype representing the memory layout of a given struct.
data Filling = Size Int | Padding Int deriving(Show, Eq)

type Size      = Int
type Alignment = Int
type Offset    = Int


-- | Calculates the memory offset of type's/struct's fields.
-- The second argument is a list of sizes and aligments of the type's/struct's fields.
calcOffsets :: [(Size, Alignment)]  -- ^ List of sizes and aligments of the type's/struct's fields. [(Int,Int)]
            -> [Offset]             -- ^ List representing the offests of the type's/struct's fields. [Int]
calcOffsets []         = []
calcOffsets size_align = calcOffsets' align filling 0 [0]
    where filling = getFilling size_align
          align = maximum $ map snd size_align

calcOffsets' :: Alignment -> [Filling] -> Offset -> [Offset] -> [Offset]
calcOffsets' align []                     offset (_:acc) = reverse acc
calcOffsets' align (Size s: Padding p:fs) offset acc = calcOffsets' align fs new_offset (new_offset : acc)
    where new_offset = s + p + offset 
calcOffsets' align (Size s:fs)            offset acc = calcOffsets' align fs new_offset (new_offset : acc)
    where new_offset = s + offset


-- | Calculates the size of the type/struct.
calcSize :: [(Size, Alignment)] -- ^ List of sizes and aligments of the type's/struct's fields. [(Int,Int)].
         -> Size                -- ^ The returned size. Int
calcSize []          = 0
calcSize size_align  = (sumFilling.getFilling) size_align


{-#INLINE sumFilling#-}
sumFilling :: [Filling] -> Int
sumFilling = sumFilling_cps (\x -> unwrap x)
    where unwrap (Size    n) = n
          unwrap (Padding n) = n

sumFilling_cps :: (Filling -> Int) -> [Filling] -> Int
sumFilling_cps cont [f]      = cont f
sumFilling_cps cont (f:rest) = sumFilling_cps (\x -> unwrap x + cont f) rest 
    where unwrap (Size    n) = n
          unwrap (Padding n) = n


{-#INLINE getFilling #-}
getFilling :: [(Int, Int)] -> [Filling]
getFilling = reverse.filter_noise.getFilling_cps (\(s,a) -> ([Size s],s) )

getFilling_cps :: ((Int, Int) -> ([Filling], Int) ) -> [(Int,Int)] -> [Filling]
getFilling_cps cont []    = [Size 0]
getFilling_cps cont [val] = fst $ cont val
getFilling_cps cont (val:rest) = getFilling_cps next_cont rest
    where next_cont (s2,0 ) = error (" WTF?! " ++ show s2 ++ " " ++ show rest) 
          next_cont (s2,a2) = do
              let (fills, off) = cont val
                  padding      = (a2 - (off+s2))  `mod` a2
              (Size s2 : Padding padding : fills, off+s2+padding)

{-# INLINE filter_noise #-}
filter_noise :: [Filling] -> [Filling]
filter_noise = filter predicate
    where predicate (Size    n) = n > 0
          predicate (Padding n) = n > 0

{-#INLINE calcAlignment #-}
calcAlignment :: [Int] -> Int
calcAlignment als = calcAlignment_cps id als

calcAlignment_cps :: (Int -> Int) -> [Int] -> Int
-- calcAlignment_cps _    []     = 0
calcAlignment_cps cont [a]    = cont a
calcAlignment_cps cont (a:as) = calcAlignment_cps (\x -> max x (cont a)) as

-- Ignores the first alignment