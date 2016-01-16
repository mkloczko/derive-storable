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

import Generics.Deriving
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import Data.Int

import Debug.Trace



-- Calculation of offsets.

-- | The datatype representing the memory layout of a given struct.
data Filling = Size Int | Padding Int deriving(Show)

type Size      = Int
type Alignment = Int
type Offset    = Int

-- | Get the memory layout of a given type/struct. 
getFilling :: Alignment          -- ^ Global alignment of a type. Int
           -> [(Size,Alignment)] -- ^ List of sizes and aligments of the type's/struct's fields. [Int,Int]
           -> [Filling]          -- ^ List representing the memory layout. [Filling]
getFilling g_alig size_align = getFilling' g_alig size_align 0 [] 

getFilling' :: Alignment -> [(Size,Alignment)] -> Offset -> [Filling] -> [Filling] 
getFilling' g_alig [] offset acc = acc 
getFilling' g_alig [(s1,al1)] offset acc = acc ++ to_add 
    where cos1       = (offset + s1) `mod` g_alig
          cos2       = if cos1 /= 0 then True else False
          to_add     = if cos2 then [Size s1, Padding (g_alig - cos1)] else [Size s1]
getFilling' g_alig ((s1,al1):(s2,al2):sas) offset acc = getFilling' g_alig ((s2,al2):sas) new_offset (acc ++ to_add)
    where cos1       = (offset + s1) `mod` al2
          cos2       = if cos1 /= 0 then True else False
          new_offset = if cos2 then offset + s1 + (al2 - cos1) else offset + s1
          to_add     = if cos2 then [Size s1, Padding (al2 - cos1)] else [Size s1]

-- | Calculates the memory offset of type's/struct's fields.
-- The second argument is a list of sizes and aligments of the type's/struct's fields.
calcOffsets :: Alignment            -- ^ Global alignment of a type. Int
            -> [(Size, Alignment)]  -- ^ List of sizes and aligments of the type's/struct's fields. [(Int,Int)]
            -> [Offset]             -- ^ List representing the offests of the type's/struct's fields. [Int]
calcOffsets align size_align = calcOffsets' align filling 0 [0]
    where filling = getFilling align size_align

calcOffsets' :: Alignment -> [Filling] -> Offset -> [Offset] -> [Offset]
calcOffsets' align []                     offset (_:acc) = reverse acc
calcOffsets' align (Size s: Padding p:fs) offset acc = calcOffsets' align fs new_offset (new_offset : acc)
    where new_offset = s + p + offset 
calcOffsets' align (Size s:fs)            offset acc = calcOffsets' align fs new_offset (new_offset : acc)
    where new_offset = s + offset


-- | Calculates the size of the type/struct.
calcSize :: Alignment           -- ^ Global alignment of a type/struct. Int 
         -> [(Size, Alignment)] -- ^ List of sizes and aligments of the type's/struct's fields. [(Int,Int)].
         -> Size                -- ^ The returned size. Int
calcSize align size_align = the_sum + the_padding
    where the_padding        = if the_sum `mod` align == 0 then 0 else (the_sum `mod` align)
          filling            = getFilling align size_align
          the_sum            = sum $ map summer filling 
          summer (Size s)    = s
          summer (Padding p) = p

