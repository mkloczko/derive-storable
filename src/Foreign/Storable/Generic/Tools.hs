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
    calcOffsets,
    calcSize
) where


-- Calculation of offsets.

-- | The datatype representing the memory layout of a given struct.
data Filling = Size Int | Padding Int deriving(Show, Eq)

type Size      = Int
type Alignment = Int
type Offset    = Int

-- | Get the memory layout of a given type/struct. 
getFilling :: [(Size,Alignment)] -- ^ List of sizes and aligments of the type's/struct's fields. [Int,Int]
           -> [Filling]          -- ^ List representing the memory layout. [Filling]
getFilling []         = []
getFilling size_align = getFilling' g_alig size_align 0 [] 
    where g_alig = maximum $ map snd size_align

getFilling' :: Alignment -> [(Size,Alignment)] -> Offset -> [Filling] -> [Filling] 
getFilling' g_alig [] offset acc = acc 
getFilling' g_alig [(s1,al1)] offset acc = acc ++ to_add 
    where cos1       = (offset + s1) `mod` g_alig
          cos2       = cos1 /= 0 
          to_add     = if cos2 then [Size s1, Padding (g_alig - cos1)] else [Size s1]
getFilling' g_alig ((s1,al1):(s2,al2):sas) offset acc = getFilling' g_alig ((s2,al2):sas) new_offset (acc ++ to_add)
    where cos1       = (offset + s1) `mod` al2
          cos2       = cos1 /= 0 
          new_offset = if cos2 then offset + s1 + (al2 - cos1) else offset + s1
          to_add     = if cos2 then [Size s1, Padding (al2 - cos1)] else [Size s1]

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
calcSize size_align  = the_sum + the_padding
    where the_padding        = the_sum `mod` align 
          align              = maximum $ map snd size_align
          filling            = getFilling size_align
          the_sum            = sum $ map summer filling 
          summer (Size s)    = s
          summer (Padding p) = p

