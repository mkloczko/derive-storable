{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

module TestCases where

import GHC.Generics hiding (C1,S1)
import GHC.TypeLits 
import Foreign.C.Types
import Foreign.Storable
import Foreign.Storable.Generic
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr)
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Marshal.Array (mallocArray, pokeArray)

import Foreign.Storable.Generic.Tools
import Foreign.Storable.Generic.Internal
import Data.Int
import Control.Monad (sequence, liftM)
import System.Exit

import Test.QuickCheck
-- The module tests the memory alignment of Storable data-types 
-- derived with generic-storable package. 

-- Adding parametric polimorphism to reduce the boilerplate.
class (Storable a) => Checkable a where
  -- | Checks whether the fields are the same
  checkFields  :: Ptr a -> Ptr a -> IO Bool 
  -- | Checks whether the offsets are the same
  checkOffsets :: a -> Ptr Int16 -> IO Bool 
  -- | Checks whether the size is the same
  getSize    :: a -> IO Int              
  -- | Checks whether the alignment is the same
  getAlignment :: a-> IO Int              
  
  new          :: a -> IO (Ptr a)


newStorable :: Storable a => a -> IO (Ptr a)
newStorable val = do
  ptr <- malloc
  poke ptr val
  return ptr


class SumOffsets' f where
    sumOffsets' :: f p -> [Offset]

instance (SumOffsets' f) => SumOffsets' (M1 D t f) where
    sumOffsets' (M1 v) = sumOffsets' v

instance (GStorable' f, SumOffsets' f) => SumOffsets' (M1 C t f) where
    sumOffsets' (M1 v) = internalOffsets v

instance (SumOffsets' f, SumOffsets' g) => SumOffsets' (f :+: g) where
    sumOffsets' (L1 v) = sumOffsets' v
    sumOffsets' (R1 v) = sumOffsets' v

instance SumOffsets' (M1 S t f) where
    sumOffsets' _ = undefined
instance SumOffsets' (f :*: g) where
    sumOffsets' _ = undefined
instance SumOffsets' (K1 i a) where
    sumOffsets' _ = undefined
instance SumOffsets' (U1) where
    sumOffsets' _ = undefined
instance SumOffsets' (V1) where
    sumOffsets' _ = undefined


goffsets :: (SumOffsets' (Rep a), GStorable a, Generic a) => a -> [Int16]
goffsets v = map fromIntegral $ sumOffsets' (from v)

data C0 = C0
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C0 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC0 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC0 offs
    getSize        a          = fromIntegral <$> getSizeC0
    getAlignment   a          = fromIntegral <$> getAlignmentC0
    new (C0 ) = do
        ptr <- newC0 
        return ptr

instance Arbitrary C0 where 
    arbitrary = return C0

foreign import ccall newC0 :: IO (Ptr C0)
foreign import ccall checkFieldsC0 :: Ptr C0 -> Ptr C0 -> IO Int8
foreign import ccall checkOffsetsC0 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC0 :: IO Int16
foreign import ccall getAlignmentC0 :: IO Int16

data C1 = C1 Int32 Int32
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C1 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC1 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC1 offs
    getSize        a          = fromIntegral <$> getSizeC1
    getAlignment   a          = fromIntegral <$> getAlignmentC1
    new (C1 a b) = do
        ptr <- newC1 a b
        return ptr

instance Arbitrary C1 where 
    arbitrary = C1 <$> arbitrary <*> arbitrary

foreign import ccall newC1 :: Int32 -> Int32 -> IO (Ptr C1)
foreign import ccall checkFieldsC1 :: Ptr C1 -> Ptr C1 -> IO Int8
foreign import ccall checkOffsetsC1 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC1 :: IO Int16
foreign import ccall getAlignmentC1 :: IO Int16

data C2 = C2 Int32 C0 Int16 Int8
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C2 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC2 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC2 offs
    getSize        a          = fromIntegral <$> getSizeC2
    getAlignment   a          = fromIntegral <$> getAlignmentC2
    new (C2 a b c d) = do
        ptr_b <- newStorable b
        ptr <- newC2 a ptr_b c d
        free ptr_b
        return ptr

instance Arbitrary C2 where 
    arbitrary = C2 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC2 :: Int32 -> Ptr C0 -> Int16 -> Int8 -> IO (Ptr C2)
foreign import ccall checkFieldsC2 :: Ptr C2 -> Ptr C2 -> IO Int8
foreign import ccall checkOffsetsC2 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC2 :: IO Int16
foreign import ccall getAlignmentC2 :: IO Int16

data C3 = C3 Int32 Int8 Int16
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C3 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC3 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC3 offs
    getSize        a          = fromIntegral <$> getSizeC3
    getAlignment   a          = fromIntegral <$> getAlignmentC3
    new (C3 a b c) = do
        ptr <- newC3 a b c
        return ptr

instance Arbitrary C3 where 
    arbitrary = C3 <$> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC3 :: Int32 -> Int8 -> Int16 -> IO (Ptr C3)
foreign import ccall checkFieldsC3 :: Ptr C3 -> Ptr C3 -> IO Int8
foreign import ccall checkOffsetsC3 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC3 :: IO Int16
foreign import ccall getAlignmentC3 :: IO Int16

data C4 = C4 Int32 Int8 Int8
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C4 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC4 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC4 offs
    getSize        a          = fromIntegral <$> getSizeC4
    getAlignment   a          = fromIntegral <$> getAlignmentC4
    new (C4 a b c) = do
        ptr <- newC4 a b c
        return ptr

instance Arbitrary C4 where 
    arbitrary = C4 <$> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC4 :: Int32 -> Int8 -> Int8 -> IO (Ptr C4)
foreign import ccall checkFieldsC4 :: Ptr C4 -> Ptr C4 -> IO Int8
foreign import ccall checkOffsetsC4 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC4 :: IO Int16
foreign import ccall getAlignmentC4 :: IO Int16

data C5 = C5 C0 C0 Int32 Int16 Int8 Int8 Int8
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C5 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC5 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC5 offs
    getSize        a          = fromIntegral <$> getSizeC5
    getAlignment   a          = fromIntegral <$> getAlignmentC5
    new (C5 a b c d e f g) = do
        ptr_a <- newStorable a
        ptr_b <- newStorable b
        ptr <- newC5 ptr_a ptr_b c d e f g
        free ptr_a
        free ptr_b
        return ptr

instance Arbitrary C5 where 
    arbitrary = C5 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC5 :: Ptr C0 -> Ptr C0 -> Int32 -> Int16 -> Int8 -> Int8 -> Int8 -> IO (Ptr C5)
foreign import ccall checkFieldsC5 :: Ptr C5 -> Ptr C5 -> IO Int8
foreign import ccall checkOffsetsC5 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC5 :: IO Int16
foreign import ccall getAlignmentC5 :: IO Int16

data C6 = C6 Int64 Int8 Int64
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C6 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC6 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC6 offs
    getSize        a          = fromIntegral <$> getSizeC6
    getAlignment   a          = fromIntegral <$> getAlignmentC6
    new (C6 a b c) = do
        ptr <- newC6 a b c
        return ptr

instance Arbitrary C6 where 
    arbitrary = C6 <$> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC6 :: Int64 -> Int8 -> Int64 -> IO (Ptr C6)
foreign import ccall checkFieldsC6 :: Ptr C6 -> Ptr C6 -> IO Int8
foreign import ccall checkOffsetsC6 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC6 :: IO Int16
foreign import ccall getAlignmentC6 :: IO Int16

data C7 = C7 C1 Int32
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C7 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC7 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC7 offs
    getSize        a          = fromIntegral <$> getSizeC7
    getAlignment   a          = fromIntegral <$> getAlignmentC7
    new (C7 a b) = do
        ptr_a <- newStorable a
        ptr <- newC7 ptr_a b
        free ptr_a
        return ptr

instance Arbitrary C7 where 
    arbitrary = C7 <$> arbitrary <*> arbitrary

foreign import ccall newC7 :: Ptr C1 -> Int32 -> IO (Ptr C7)
foreign import ccall checkFieldsC7 :: Ptr C7 -> Ptr C7 -> IO Int8
foreign import ccall checkOffsetsC7 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC7 :: IO Int16
foreign import ccall getAlignmentC7 :: IO Int16

data C8 = C8 C2 Int8 C4
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C8 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC8 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC8 offs
    getSize        a          = fromIntegral <$> getSizeC8
    getAlignment   a          = fromIntegral <$> getAlignmentC8
    new (C8 a b c) = do
        ptr_a <- newStorable a
        ptr_c <- newStorable c
        ptr <- newC8 ptr_a b ptr_c
        free ptr_a
        free ptr_c
        return ptr

instance Arbitrary C8 where 
    arbitrary = C8 <$> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC8 :: Ptr C2 -> Int8 -> Ptr C4 -> IO (Ptr C8)
foreign import ccall checkFieldsC8 :: Ptr C8 -> Ptr C8 -> IO Int8
foreign import ccall checkOffsetsC8 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC8 :: IO Int16
foreign import ccall getAlignmentC8 :: IO Int16

data C9 = C9 C5 Int8 Int8 Int8
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C9 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC9 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC9 offs
    getSize        a          = fromIntegral <$> getSizeC9
    getAlignment   a          = fromIntegral <$> getAlignmentC9
    new (C9 a b c d) = do
        ptr_a <- newStorable a
        ptr <- newC9 ptr_a b c d
        free ptr_a
        return ptr

instance Arbitrary C9 where 
    arbitrary = C9 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC9 :: Ptr C5 -> Int8 -> Int8 -> Int8 -> IO (Ptr C9)
foreign import ccall checkFieldsC9 :: Ptr C9 -> Ptr C9 -> IO Int8
foreign import ccall checkOffsetsC9 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC9 :: IO Int16
foreign import ccall getAlignmentC9 :: IO Int16

data C10 = C10 C8 Int64 C1
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C10 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC10 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC10 offs
    getSize        a          = fromIntegral <$> getSizeC10
    getAlignment   a          = fromIntegral <$> getAlignmentC10
    new (C10 a b c) = do
        ptr_a <- newStorable a
        ptr_c <- newStorable c
        ptr <- newC10 ptr_a b ptr_c
        free ptr_a
        free ptr_c
        return ptr

instance Arbitrary C10 where 
    arbitrary = C10 <$> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC10 :: Ptr C8 -> Int64 -> Ptr C1 -> IO (Ptr C10)
foreign import ccall checkFieldsC10 :: Ptr C10 -> Ptr C10 -> IO Int8
foreign import ccall checkOffsetsC10 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC10 :: IO Int16
foreign import ccall getAlignmentC10 :: IO Int16

data C11 = C11 C10 C10
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C11 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC11 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC11 offs
    getSize        a          = fromIntegral <$> getSizeC11
    getAlignment   a          = fromIntegral <$> getAlignmentC11
    new (C11 a b) = do
        ptr_a <- newStorable a
        ptr_b <- newStorable b
        ptr <- newC11 ptr_a ptr_b
        free ptr_a
        free ptr_b
        return ptr

instance Arbitrary C11 where 
    arbitrary = C11 <$> arbitrary <*> arbitrary

foreign import ccall newC11 :: Ptr C10 -> Ptr C10 -> IO (Ptr C11)
foreign import ccall checkFieldsC11 :: Ptr C11 -> Ptr C11 -> IO Int8
foreign import ccall checkOffsetsC11 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC11 :: IO Int16
foreign import ccall getAlignmentC11 :: IO Int16

data C12 = C12 Int64 Int64 Int64 Int64
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C12 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC12 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC12 offs
    getSize        a          = fromIntegral <$> getSizeC12
    getAlignment   a          = fromIntegral <$> getAlignmentC12
    new (C12 a b c d) = do
        ptr <- newC12 a b c d
        return ptr

instance Arbitrary C12 where 
    arbitrary = C12 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC12 :: Int64 -> Int64 -> Int64 -> Int64 -> IO (Ptr C12)
foreign import ccall checkFieldsC12 :: Ptr C12 -> Ptr C12 -> IO Int8
foreign import ccall checkOffsetsC12 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC12 :: IO Int16
foreign import ccall getAlignmentC12 :: IO Int16

data C13 = C13 C12 C12 C0 C12 C12
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C13 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC13 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC13 offs
    getSize        a          = fromIntegral <$> getSizeC13
    getAlignment   a          = fromIntegral <$> getAlignmentC13
    new (C13 a b c d e) = do
        ptr_a <- newStorable a
        ptr_b <- newStorable b
        ptr_c <- newStorable c
        ptr_d <- newStorable d
        ptr_e <- newStorable e
        ptr <- newC13 ptr_a ptr_b ptr_c ptr_d ptr_e
        free ptr_a
        free ptr_b
        free ptr_c
        free ptr_d
        free ptr_e
        return ptr

instance Arbitrary C13 where 
    arbitrary = C13 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC13 :: Ptr C12 -> Ptr C12 -> Ptr C0 -> Ptr C12 -> Ptr C12 -> IO (Ptr C13)
foreign import ccall checkFieldsC13 :: Ptr C13 -> Ptr C13 -> IO Int8
foreign import ccall checkOffsetsC13 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC13 :: IO Int16
foreign import ccall getAlignmentC13 :: IO Int16

data C14 = C14 C13 C13 C13 C13 Int8
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C14 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC14 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC14 offs
    getSize        a          = fromIntegral <$> getSizeC14
    getAlignment   a          = fromIntegral <$> getAlignmentC14
    new (C14 a b c d e) = do
        ptr_a <- newStorable a
        ptr_b <- newStorable b
        ptr_c <- newStorable c
        ptr_d <- newStorable d
        ptr <- newC14 ptr_a ptr_b ptr_c ptr_d e
        free ptr_a
        free ptr_b
        free ptr_c
        free ptr_d
        return ptr

instance Arbitrary C14 where 
    arbitrary = C14 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC14 :: Ptr C13 -> Ptr C13 -> Ptr C13 -> Ptr C13 -> Int8 -> IO (Ptr C14)
foreign import ccall checkFieldsC14 :: Ptr C14 -> Ptr C14 -> IO Int8
foreign import ccall checkOffsetsC14 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC14 :: IO Int16
foreign import ccall getAlignmentC14 :: IO Int16

data C15 = C15 C12 C14
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C15 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC15 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC15 offs
    getSize        a          = fromIntegral <$> getSizeC15
    getAlignment   a          = fromIntegral <$> getAlignmentC15
    new (C15 a b) = do
        ptr_a <- newStorable a
        ptr_b <- newStorable b
        ptr <- newC15 ptr_a ptr_b
        free ptr_a
        free ptr_b
        return ptr

instance Arbitrary C15 where 
    arbitrary = C15 <$> arbitrary <*> arbitrary

foreign import ccall newC15 :: Ptr C12 -> Ptr C14 -> IO (Ptr C15)
foreign import ccall checkFieldsC15 :: Ptr C15 -> Ptr C15 -> IO Int8
foreign import ccall checkOffsetsC15 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC15 :: IO Int16
foreign import ccall getAlignmentC15 :: IO Int16

data C16 = C16 C10 C0 C15 C7
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C16 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC16 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC16 offs
    getSize        a          = fromIntegral <$> getSizeC16
    getAlignment   a          = fromIntegral <$> getAlignmentC16
    new (C16 a b c d) = do
        ptr_a <- newStorable a
        ptr_b <- newStorable b
        ptr_c <- newStorable c
        ptr_d <- newStorable d
        ptr <- newC16 ptr_a ptr_b ptr_c ptr_d
        free ptr_a
        free ptr_b
        free ptr_c
        free ptr_d
        return ptr

instance Arbitrary C16 where 
    arbitrary = C16 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC16 :: Ptr C10 -> Ptr C0 -> Ptr C15 -> Ptr C7 -> IO (Ptr C16)
foreign import ccall checkFieldsC16 :: Ptr C16 -> Ptr C16 -> IO Int8
foreign import ccall checkOffsetsC16 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC16 :: IO Int16
foreign import ccall getAlignmentC16 :: IO Int16

data C17 = C17 Float Double Float
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C17 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC17 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC17 offs
    getSize        a          = fromIntegral <$> getSizeC17
    getAlignment   a          = fromIntegral <$> getAlignmentC17
    new (C17 a b c) = do
        ptr <- newC17 a b c
        return ptr

instance Arbitrary C17 where 
    arbitrary = C17 <$> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC17 :: Float -> Double -> Float -> IO (Ptr C17)
foreign import ccall checkFieldsC17 :: Ptr C17 -> Ptr C17 -> IO Int8
foreign import ccall checkOffsetsC17 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC17 :: IO Int16
foreign import ccall getAlignmentC17 :: IO Int16

data C18 = C18 Float Float Double Float
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C18 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC18 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC18 offs
    getSize        a          = fromIntegral <$> getSizeC18
    getAlignment   a          = fromIntegral <$> getAlignmentC18
    new (C18 a b c d) = do
        ptr <- newC18 a b c d
        return ptr

instance Arbitrary C18 where 
    arbitrary = C18 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC18 :: Float -> Float -> Double -> Float -> IO (Ptr C18)
foreign import ccall checkFieldsC18 :: Ptr C18 -> Ptr C18 -> IO Int8
foreign import ccall checkOffsetsC18 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC18 :: IO Int16
foreign import ccall getAlignmentC18 :: IO Int16

data C19 = C19 C17 Float Double
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C19 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC19 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC19 offs
    getSize        a          = fromIntegral <$> getSizeC19
    getAlignment   a          = fromIntegral <$> getAlignmentC19
    new (C19 a b c) = do
        ptr_a <- newStorable a
        ptr <- newC19 ptr_a b c
        free ptr_a
        return ptr

instance Arbitrary C19 where 
    arbitrary = C19 <$> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC19 :: Ptr C17 -> Float -> Double -> IO (Ptr C19)
foreign import ccall checkFieldsC19 :: Ptr C19 -> Ptr C19 -> IO Int8
foreign import ccall checkOffsetsC19 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC19 :: IO Int16
foreign import ccall getAlignmentC19 :: IO Int16

data C20 = C20 Double C18 Double C19
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C20 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC20 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC20 offs
    getSize        a          = fromIntegral <$> getSizeC20
    getAlignment   a          = fromIntegral <$> getAlignmentC20
    new (C20 a b c d) = do
        ptr_b <- newStorable b
        ptr_d <- newStorable d
        ptr <- newC20 a ptr_b c ptr_d
        free ptr_b
        free ptr_d
        return ptr

instance Arbitrary C20 where 
    arbitrary = C20 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC20 :: Double -> Ptr C18 -> Double -> Ptr C19 -> IO (Ptr C20)
foreign import ccall checkFieldsC20 :: Ptr C20 -> Ptr C20 -> IO Int8
foreign import ccall checkOffsetsC20 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeC20 :: IO Int16
foreign import ccall getAlignmentC20 :: IO Int16

#ifdef GSTORABLE_SUMTYPES
data S0 = S0_1 Int8
        | S0_2 Int16
    deriving (Show, Eq, Generic, GStorable)

instance Checkable S0 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsS0 ptr1 ptr2
    checkOffsets (S0_1 a) offs = (==1) <$> checkOffsetsS0_1 offs
    checkOffsets (S0_2 a) offs = (==1) <$> checkOffsetsS0_2 offs
    getSize        a          = fromIntegral <$> getSizeS0
    getAlignment   a          = fromIntegral <$> getAlignmentS0
    new (S0_1 a) = do
        ptr <- newS0_1 a
        return ptr
    new (S0_2 a) = do
        ptr <- newS0_2 a
        return ptr

instance Arbitrary S0 where 
    arbitrary = oneof [ S0_1 <$> arbitrary
                      , S0_2 <$> arbitrary
                      ]
foreign import ccall newS0_1 :: Int8 -> IO (Ptr S0)
foreign import ccall newS0_2 :: Int16 -> IO (Ptr S0)
foreign import ccall checkFieldsS0 :: Ptr S0 -> Ptr S0 -> IO Int8
foreign import ccall checkOffsetsS0 :: Ptr Int16 -> IO Int8
foreign import ccall checkOffsetsS0_1 :: Ptr Int16 -> IO Int8
foreign import ccall checkOffsetsS0_2 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeS0 :: IO Int16
foreign import ccall getAlignmentS0 :: IO Int16

data S1 = S1_1 Int32 Int8
        | S1_2 Double Int32 Int8 Int8
        | S1_3 Float
    deriving (Show, Eq, Generic, GStorable)

instance Checkable S1 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsS1 ptr1 ptr2
    checkOffsets (S1_1 a b) offs = (==1) <$> checkOffsetsS1_1 offs
    checkOffsets (S1_2 a b c d) offs = (==1) <$> checkOffsetsS1_2 offs
    checkOffsets (S1_3 a) offs = (==1) <$> checkOffsetsS1_3 offs
    getSize        a          = fromIntegral <$> getSizeS1
    getAlignment   a          = fromIntegral <$> getAlignmentS1
    new (S1_1 a b) = do
        ptr <- newS1_1 a b
        return ptr
    new (S1_2 a b c d) = do
        ptr <- newS1_2 a b c d
        return ptr
    new (S1_3 a) = do
        ptr <- newS1_3 a
        return ptr

instance Arbitrary S1 where 
    arbitrary = oneof [ S1_1 <$> arbitrary <*> arbitrary
                      , S1_2 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      , S1_3 <$> arbitrary
                      ]
foreign import ccall newS1_1 :: Int32 -> Int8 -> IO (Ptr S1)
foreign import ccall newS1_2 :: Double -> Int32 -> Int8 -> Int8 -> IO (Ptr S1)
foreign import ccall newS1_3 :: Float -> IO (Ptr S1)
foreign import ccall checkFieldsS1 :: Ptr S1 -> Ptr S1 -> IO Int8
foreign import ccall checkOffsetsS1 :: Ptr Int16 -> IO Int8
foreign import ccall checkOffsetsS1_1 :: Ptr Int16 -> IO Int8
foreign import ccall checkOffsetsS1_2 :: Ptr Int16 -> IO Int8
foreign import ccall checkOffsetsS1_3 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeS1 :: IO Int16
foreign import ccall getAlignmentS1 :: IO Int16

data S2 = S2_1 C13 Int8
        | S2_2 C2 C1
        | S2_3 C0
    deriving (Show, Eq, Generic, GStorable)

instance Checkable S2 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsS2 ptr1 ptr2
    checkOffsets (S2_1 a b) offs = (==1) <$> checkOffsetsS2_1 offs
    checkOffsets (S2_2 a b) offs = (==1) <$> checkOffsetsS2_2 offs
    checkOffsets (S2_3 a) offs = (==1) <$> checkOffsetsS2_3 offs
    getSize        a          = fromIntegral <$> getSizeS2
    getAlignment   a          = fromIntegral <$> getAlignmentS2
    new (S2_1 a b) = do
        ptr_a <- newStorable a
        ptr <- newS2_1 ptr_a b
        free ptr_a
        return ptr
    new (S2_2 a b) = do
        ptr_a <- newStorable a
        ptr_b <- newStorable b
        ptr <- newS2_2 ptr_a ptr_b
        free ptr_a
        free ptr_b
        return ptr
    new (S2_3 a) = do
        ptr_a <- newStorable a
        ptr <- newS2_3 ptr_a
        free ptr_a
        return ptr

instance Arbitrary S2 where 
    arbitrary = oneof [ S2_1 <$> arbitrary <*> arbitrary
                      , S2_2 <$> arbitrary <*> arbitrary
                      , S2_3 <$> arbitrary
                      ]
foreign import ccall newS2_1 :: Ptr C13 -> Int8 -> IO (Ptr S2)
foreign import ccall newS2_2 :: Ptr C2 -> Ptr C1 -> IO (Ptr S2)
foreign import ccall newS2_3 :: Ptr C0 -> IO (Ptr S2)
foreign import ccall checkFieldsS2 :: Ptr S2 -> Ptr S2 -> IO Int8
foreign import ccall checkOffsetsS2 :: Ptr Int16 -> IO Int8
foreign import ccall checkOffsetsS2_1 :: Ptr Int16 -> IO Int8
foreign import ccall checkOffsetsS2_2 :: Ptr Int16 -> IO Int8
foreign import ccall checkOffsetsS2_3 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeS2 :: IO Int16
foreign import ccall getAlignmentS2 :: IO Int16

data S3 = S3_1 S1
        | S3_2 S2
        | S3_3 Int8
    deriving (Show, Eq, Generic, GStorable)

instance Checkable S3 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsS3 ptr1 ptr2
    checkOffsets (S3_1 a) offs = (==1) <$> checkOffsetsS3_1 offs
    checkOffsets (S3_2 a) offs = (==1) <$> checkOffsetsS3_2 offs
    checkOffsets (S3_3 a) offs = (==1) <$> checkOffsetsS3_3 offs
    getSize        a          = fromIntegral <$> getSizeS3
    getAlignment   a          = fromIntegral <$> getAlignmentS3
    new (S3_1 a) = do
        ptr_a <- newStorable a
        ptr <- newS3_1 ptr_a
        free ptr_a
        return ptr
    new (S3_2 a) = do
        ptr_a <- newStorable a
        ptr <- newS3_2 ptr_a
        free ptr_a
        return ptr
    new (S3_3 a) = do
        ptr <- newS3_3 a
        return ptr

instance Arbitrary S3 where 
    arbitrary = oneof [ S3_1 <$> arbitrary
                      , S3_2 <$> arbitrary
                      , S3_3 <$> arbitrary
                      ]
foreign import ccall newS3_1 :: Ptr S1 -> IO (Ptr S3)
foreign import ccall newS3_2 :: Ptr S2 -> IO (Ptr S3)
foreign import ccall newS3_3 :: Int8 -> IO (Ptr S3)
foreign import ccall checkFieldsS3 :: Ptr S3 -> Ptr S3 -> IO Int8
foreign import ccall checkOffsetsS3 :: Ptr Int16 -> IO Int8
foreign import ccall checkOffsetsS3_1 :: Ptr Int16 -> IO Int8
foreign import ccall checkOffsetsS3_2 :: Ptr Int16 -> IO Int8
foreign import ccall checkOffsetsS3_3 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeS3 :: IO Int16
foreign import ccall getAlignmentS3 :: IO Int16

data S4 = S4_1 C0
        | S4_2 C0
        | S4_3 C0
        | S4_4 C1
    deriving (Show, Eq, Generic, GStorable)

instance Checkable S4 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsS4 ptr1 ptr2
    checkOffsets (S4_1 a) offs = (==1) <$> checkOffsetsS4_1 offs
    checkOffsets (S4_2 a) offs = (==1) <$> checkOffsetsS4_2 offs
    checkOffsets (S4_3 a) offs = (==1) <$> checkOffsetsS4_3 offs
    checkOffsets (S4_4 a) offs = (==1) <$> checkOffsetsS4_4 offs
    getSize        a          = fromIntegral <$> getSizeS4
    getAlignment   a          = fromIntegral <$> getAlignmentS4
    new (S4_1 a) = do
        ptr_a <- newStorable a
        ptr <- newS4_1 ptr_a
        free ptr_a
        return ptr
    new (S4_2 a) = do
        ptr_a <- newStorable a
        ptr <- newS4_2 ptr_a
        free ptr_a
        return ptr
    new (S4_3 a) = do
        ptr_a <- newStorable a
        ptr <- newS4_3 ptr_a
        free ptr_a
        return ptr
    new (S4_4 a) = do
        ptr_a <- newStorable a
        ptr <- newS4_4 ptr_a
        free ptr_a
        return ptr

instance Arbitrary S4 where 
    arbitrary = oneof [ S4_1 <$> arbitrary
                      , S4_2 <$> arbitrary
                      , S4_3 <$> arbitrary
                      , S4_4 <$> arbitrary
                      ]
foreign import ccall newS4_1 :: Ptr C0 -> IO (Ptr S4)
foreign import ccall newS4_2 :: Ptr C0 -> IO (Ptr S4)
foreign import ccall newS4_3 :: Ptr C0 -> IO (Ptr S4)
foreign import ccall newS4_4 :: Ptr C1 -> IO (Ptr S4)
foreign import ccall checkFieldsS4 :: Ptr S4 -> Ptr S4 -> IO Int8
foreign import ccall checkOffsetsS4 :: Ptr Int16 -> IO Int8
foreign import ccall checkOffsetsS4_1 :: Ptr Int16 -> IO Int8
foreign import ccall checkOffsetsS4_2 :: Ptr Int16 -> IO Int8
foreign import ccall checkOffsetsS4_3 :: Ptr Int16 -> IO Int8
foreign import ccall checkOffsetsS4_4 :: Ptr Int16 -> IO Int8
foreign import ccall getSizeS4 :: IO Int16
foreign import ccall getAlignmentS4 :: IO Int16

#endif
