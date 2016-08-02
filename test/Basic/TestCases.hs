{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestCases where

import GHC.Generics (Generic, Rep, from)
import Foreign.C.Types
import Foreign.Storable
import Foreign.Storable.TH
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
goffsets v = map fromIntegral $ internalOffsets (from v)

data C1 = C1 Int32 Int32
    deriving (Show, Eq, Generic)

$(deriveGStorable @C1 ''C1)

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
    deriving (Show, Eq, Generic)

$(deriveGStorable @C2 ''C2)

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
    deriving (Show, Eq, Generic)

$(deriveGStorable @C3 ''C3)

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
    deriving (Show, Eq, Generic)

$(deriveGStorable @C4 ''C4)

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
    deriving (Show, Eq, Generic)

$(deriveGStorable @C5 ''C5)

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
    deriving (Show, Eq, Generic)

$(deriveGStorable @C6 ''C6)

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

data C17 = C17 Float Double Float
    deriving (Show, Eq, Generic)

$(deriveGStorable @C17 ''C17)

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
    deriving (Show, Eq, Generic)

$(deriveGStorable @C18 ''C18)

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

