{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestCases where

import GHC.Generics (Generic, Rep, from)
import Foreign.C.Types
import Foreign.Storable
import Foreign.Storable.Generic
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr)
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Marshal.Array (mallocArray, pokeArray)

import Foreign.Storable.Generic.Tools
import Foreign.Storable.Generic.Internal
import Foreign.Storable.Generic.Instances
import Data.Int
import Control.Monad (sequence, liftM)
import System.Exit

import Test.QuickCheck
-- The module tests the memory alignment of Storable data-types 
-- derived with generic-storable package. 
--
-- The module will define data-types and the corresponding C structs

-- If fields ok and offsets not - the value of the fields before could use only some of the bits

-- If the offsets are ok and fields are not -- different types ? (Int vs Unsigned int).

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

goffsets :: (GStorable' (Rep a), GStorable a, Generic a) => a -> [Int16]
goffsets v = map fromIntegral $ calcOffsets $ zip sizes alignments 
    where sizes = glistSizeOf' (from v)
          alignments = glistAlignment' (from v)
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

data C2 = C2 Int32 Int16 Int8
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C2 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC2 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC2 offs
    getSize        a          = fromIntegral <$> getSizeC2
    getAlignment   a          = fromIntegral <$> getAlignmentC2
    new (C2 a b c) = do
        ptr <- newC2 a b c
        return ptr

instance Arbitrary C2 where 
    arbitrary = C2 <$> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC2 :: Int32 -> Int16 -> Int8 -> IO (Ptr C2)
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

data C5 = C5 Int32 Int16 Int8 Int8 Int8
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C5 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC5 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC5 offs
    getSize        a          = fromIntegral <$> getSizeC5
    getAlignment   a          = fromIntegral <$> getAlignmentC5
    new (C5 a b c d e) = do
        ptr <- newC5 a b c d e
        return ptr

instance Arbitrary C5 where 
    arbitrary = C5 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC5 :: Int32 -> Int16 -> Int8 -> Int8 -> Int8 -> IO (Ptr C5)
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

data C13 = C13 C12 C12 C12 C12
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C13 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC13 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC13 offs
    getSize        a          = fromIntegral <$> getSizeC13
    getAlignment   a          = fromIntegral <$> getAlignmentC13
    new (C13 a b c d) = do
        ptr_a <- newStorable a
        ptr_b <- newStorable b
        ptr_c <- newStorable c
        ptr_d <- newStorable d
        ptr <- newC13 ptr_a ptr_b ptr_c ptr_d
        free ptr_a
        free ptr_b
        free ptr_c
        free ptr_d
        return ptr

instance Arbitrary C13 where 
    arbitrary = C13 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC13 :: Ptr C12 -> Ptr C12 -> Ptr C12 -> Ptr C12 -> IO (Ptr C13)
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

data C16 = C16 C10 C15 C7
    deriving (Show, Eq, Generic, GStorable)

instance Checkable C16 where
    checkFields    ptr1 ptr2 = (==1) <$> checkFieldsC16 ptr1 ptr2
    checkOffsets   _    offs = (==1) <$> checkOffsetsC16 offs
    getSize        a          = fromIntegral <$> getSizeC16
    getAlignment   a          = fromIntegral <$> getAlignmentC16
    new (C16 a b c) = do
        ptr_a <- newStorable a
        ptr_b <- newStorable b
        ptr_c <- newStorable c
        ptr <- newC16 ptr_a ptr_b ptr_c
        free ptr_a
        free ptr_b
        free ptr_c
        return ptr

instance Arbitrary C16 where 
    arbitrary = C16 <$> arbitrary <*> arbitrary <*> arbitrary

foreign import ccall newC16 :: Ptr C10 -> Ptr C15 -> Ptr C7 -> IO (Ptr C16)
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

