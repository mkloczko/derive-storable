{-|
Module      : Foreign.Storable.Generic.Tools
Copyright   : (c) Mateusz Kłoczko, 2016
License     : MIT
Maintainer  : mateusz.p.kloczko@gmail.com
Stability   : experimental
Portability : portable


-}


{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE PolyKinds #-}

{-#LANGUAGE TypeInType #-}

module Foreign.Storable.Generic.Tools (
    Size,
    Alignment,
    Offset,
    Filling(..),
    calcOffsets,
    calcSize,
    calcAlignment,
    getFilling
) where

import Data.List
import GHC.TypeLits

-- | The datatype representing the memory layout of a given struct.
data Filling = Size Int | Padding Int deriving(Show, Eq)

data FillingNat = SizeNat Nat | PaddingNat Nat 

type family NotZero (x :: FillingNat) :: Bool where
    NotZero ('SizeNat    0) = False
    NotZero (PaddingNat 0) = False
    NotZero _               = True

-- | Checks whether the size or padding are zero.
not_zero :: Filling -> Bool
not_zero (Size    0) = False
not_zero (Padding 0) = False
not_zero _           = True


type Size      = Int
type Alignment = Int
type Offset    = Int

type SizeNat      = Nat 
type AlignmentNat = Nat 
type OffsetNat    = Nat 

type family FilterNonZeros (ls :: [FillingNat]) :: [FillingNat] where
    FilterNonZeros '[]            = '[]
    FilterNonZeros (('SizeNat    0):as) = FilterNonZeros as
    FilterNonZeros ((PaddingNat 0):as) = FilterNonZeros as
    FilterNonZeros (a:as)              = a : FilterNonZeros as

type family GetFsts (ls :: [(a,b)]) :: [b] where
    GetFsts '[]          = '[]
    GetFsts ('(a,b):abs) = a : GetFsts abs

type family GetSnds (ls :: [(a,b)]) :: [b] where
    GetSnds '[]          = '[]
    GetSnds ('(a,b):abs) = b : GetSnds abs

type family Fst (tup :: (a,b)) :: a where
    Fst '(el1,el2) = el1

type family Snd (tup :: (a,b)) :: b where
    Snd '(el1,el2) = el2

type family Max  (a :: Nat) (b :: Nat) :: Nat where
    Max a b = Max' a b (CmpNat a b) 

type family Max' (a :: Nat) (b :: Nat) (ord :: Ordering) :: Nat where
    Max' a b LT = b
    Max' a b _  = a

type family Reverse (ls :: [a]) :: [a] where
    Reverse as    = Reverse' as '[]

type family Reverse' (ls :: [a]) (acc :: [a]) :: [a] where
    Reverse' '[]    acc = acc
    Reverse' (a:as) acc = Reverse' as (a:acc)

type family Concat (lls :: [[a]]) :: [a] where
    Concat '[]           = '[]
    Concat ('[]:rst)     = Concat rst
    Concat ((a:as):rst) = a : Concat (as:rst)

type family Zip (as :: [a]) (bs :: [b]) :: [(a,b)] where
    Zip '[]    _      = '[]
    Zip _     '[]     = '[]
    Zip (a:as) (b:bs) = '(a,b): Zip as bs


type family GetFilling (sals :: [(SizeNat,AlignmentNat)]) :: [FillingNat] where
    GetFilling sals = Reverse (FilterNonZeros (GetFilling' (Concat '[OrderStep sals, '[ '(CalcSize sals, 0)]]) 0 0 '[] ))

type family P_ToOffsetSize (sals :: [(SizeNat,AlignmentNat)]) :: [(OffsetNat,SizeNat)] where
    P_ToOffsetSize sals = Zip (CalcOffsets sals) (GetFsts sals)

-- Insertion sort/selection sort (sort only when unsorted, otherwise O(n) ?)

type family P_Insert (el :: (OffsetNat, SizeNat)) (stuff :: [(OffsetNat,SizeNat)]) :: [(OffsetNat,SizeNat)] where
    P_Insert el        '[]           = '[el] 
    P_Insert '(oe, se) ('(o1,s1):as) = P_Insert' '(oe,se) ('(o1,s1):as) (CmpNat oe o1)

type family P_Insert' (el :: (OffsetNat, SizeNat)) (stuff :: [(OffsetNat,SizeNat)]) (cmp :: Ordering) :: [(OffsetNat, SizeNat)] where
    P_Insert' el (a:as) LT = el:a:as
    P_Insert' el (a:as) _  = a : P_Insert el as

type family P_OrderStep (os :: [(OffsetNat, SizeNat)]) (acc :: [(OffsetNat, SizeNat)]) :: [(OffsetNat, SizeNat)] where
    P_OrderStep '[] acc = acc 
    P_OrderStep (el:els) acc = P_OrderStep els (P_Insert el acc)

type family OrderStep (sals :: [(SizeNat,AlignmentNat)]) :: [(OffsetNat,SizeNat)] where
    OrderStep sals = (P_ToOffsetSize sals) 

-- | Get the memory layout of a given type/struct. Used mostly as debug information.
getFilling :: [(Size,Alignment)] -- ^ List of sizes and aligments of the type's/struct's fields. [(Int,Int)]
           -> [Filling]          -- ^ List representing the memory layout. [Filling]
getFilling size_align = reverse $ filter not_zero $ getFilling' (ordered ++ [(gl_size,0)] ) 0 0 []
    where offsets = calcOffsets size_align  :: [Offset]
          sizes   = map fst size_align      :: [Size]
          ordered = sortBy (\(o1,_) (o2,_) -> compare o1 o2) $ -- Needs to be sorted in case the fields are rearranged. 
                        zip offsets sizes   :: [(Offset,Size)] 
          gl_size  = calcSize size_align    :: Offset   -- This variable is used as offset

type family GetFilling' (sals::[(SizeNat,AlignmentNat)]) (s::SizeNat) (off::OffsetNat) (acc :: [FillingNat]) :: [FillingNat] where
    GetFilling' '[]              _  _ acc = acc
    GetFilling' ('(o2,s2):rest) s1 o1 acc = GetFilling' rest s2 o2 ('SizeNat s2 : PaddingNat ((o2-o1) - s1) : acc)

getFilling' :: [(Offset, Size)] -- ^ List of struct's fields' offsets and sizes  
            -> Size             -- ^ Size of the previous element
            -> Offset           -- ^ Offest of the previous element
            -> [Filling]        -- ^ Accumulator: List of filling
            -> [Filling]        -- ^ Returned list of fillings.
getFilling' []              _  _  acc = acc
getFilling' ((o2,s2):rest) s1 o1 acc = getFilling' rest s2 o2 (Size s2 : Padding ((o2-o1) - s1) : acc )


type family CalcOffsets (sals :: [(SizeNat,AlignmentNat)]) :: [OffsetNat] where
    CalcOffsets '[]        = '[]
    CalcOffsets size_align = Reverse  (Fst ( CalcOffsets' size_align 0 '[]))

type family CalcOffsets' (sals :: [(SizeNat, AlignmentNat)]) (itr :: Nat) (offst :: [OffsetNat]) :: ([OffsetNat],Nat) where
    CalcOffsets' '[]          inter acc = '(acc, inter)
    CalcOffsets' ('(s,a):rest) inter acc = CalcOffsets' rest (inter + s + (Mod (a-inter) a)) ((inter + (Mod (a-inter) a) ) : acc)

{-# NOINLINE calcOffsets #-}
-- | Calculates the memory offset of type's/struct's fields.
calcOffsets :: [(Size, Alignment)]  -- ^ List of sizes and aligments of the type's/struct's fields. [(Int,Int)]
            -> [Offset]             -- ^ List representing the offests of the type's/struct's fields. [Int]
calcOffsets []         = []
calcOffsets size_align = reverse $ fst $ calcOffsets' size_align 0 []

{-# NOINLINE calcOffsets' #-}
calcOffsets' :: [(Size, Alignment)] -- ^ List of struct's fields' sizes and alignments
             -> Int                 -- ^ The intermediate variable between the current and previous iteration.
             -> [Offset]            -- ^ Accumulator
             -> ([Offset], Int)     -- ^ List of offsets and the last intermediate value.
calcOffsets' []           inter acc = (acc, inter)  
calcOffsets' ((s,a):rest) inter acc = calcOffsets' rest (last_off + s) (last_off: acc)
    where p = (a - inter) `mod` a -- Padding
          last_off = inter + p    :: Offset



type family Aligns (sals :: [(SizeNat,AlignmentNat)]) :: [AlignmentNat] where
    Aligns sizes_aligns = GetSnds sizes_aligns

type family CalcSize (sals :: [(SizeNat,AlignmentNat)]) :: SizeNat where
    -- I really wish for local variables for type funs...
    -- Maybe CPP macros would be ok, but they come with their own trade offs
    -- inter = (Snd (CalcOffsets' sizes_aligns 0 '[]))
    -- glob_align = (CalcAlignment (GetSnds sizes_aligns)) 
    CalcSize sizes_aligns = (Snd (CalcOffsets' sizes_aligns 0 '[])) + (Mod (CalcAlignment ((GetSnds sizes_aligns)) - (Snd (CalcOffsets' sizes_aligns 0 '[]))) (CalcAlignment (GetSnds sizes_aligns) ) )

{-# NOINLINE calcSize #-}
-- | Calculates the size of the type/struct.
calcSize :: [(Size, Alignment)] -- ^ List of sizes and aligments of the type's/struct's fields. [(Int,Int)].
         -> Size                -- ^ The returned size. Int
calcSize size_align = inter + ((glob_align - inter) `mod` glob_align)
    where glob_align = calcAlignment $ map snd size_align
          inter      = snd $ calcOffsets' size_align 0 []

{-# NOINLINE calcAlignment #-}

type family CalcAlignment (as :: [AlignmentNat]) :: AlignmentNat where
    CalcAlignment aligns = CalcAlignment' aligns 1 

type family CalcAlignment' (as :: [AlignmentNat]) (acc :: AlignmentNat) :: AlignmentNat where
    CalcAlignment' '[]   glob = glob
    CalcAlignment' (a:as) glob = CalcAlignment' as  (Max glob a)

-- | Calculate the alignment of a struct.
calcAlignment :: [Alignment] -- ^ List of struct's fields' alignments.
              -> Alignment   -- ^ The resulting alignment.
calcAlignment aligns = calcAlignment' aligns 1


calcAlignment' :: [Alignment] -- ^ List of alignments
               -> Alignment   -- ^ Accumulator
               -> Alignment   -- ^ The resulting alignment.
calcAlignment' []          glob = glob
calcAlignment' (al:aligns) glob = calcAlignment' aligns (max glob al)
