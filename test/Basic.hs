{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Generics.Deriving (Generic)
import Foreign.C.Types
import Foreign.Storable
import Foreign.Storable.Generic
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (malloc)
import Foreign.Marshal.Array (mallocArray, pokeArray)


import Data.Int
import Control.Monad (sequence, liftM)
import System.Exit

-- The module tests the memory alignment of Storable data-types 
-- derived with generic-storable package. 
--
-- The module will define data-types and the corresponding C structs
-- The corresponding C structs will be presented with their
-- size, alignments and offsets.

-- struct C1 {
-- //TYPE:    FIELD:  SIZE:  ALIGNMENT:  OFFSET: 
--   HsInt32  a;      4      4           0
--   HsInt8   b;      1      1           4
--   HsInt8   c;      1      1           5
-- //padding - 2 bytes
-- };
-- Expected size:      8
-- Expected alignment: 4

data C1 = C1 Int32 Int8 Int8 deriving (Show, Generic)

instance GStorable C1

-- struct C2 {
-- //TYPE:    FIELD:  SIZE:  ALIGNMENT:  OFFSET:
--   HsInt64  a;      8      8           0
--   HsInt8   b;      1      1           8
-- //padding - 3 bytes
--   HsInt32  c;      4      4           12
-- };
-- Expected size:      16
-- Expected alignment: 8

data C2 = C2 Int64 Int8 Int32 deriving (Show, Generic)

instance GStorable C2


-- struct C3 {
-- //TYPE:  FIELD:  SIZE:  ALIGNMENT: OFFSET:
--   HsInt16  a;      2      2          0
--   HsInt8   b;      1      1          2
--   HsInt8   c;      1      1          3
--   HsInt8   d;      1      1          4
-- //padding - 2 bytes  
--   HsInt16  e;      2      2          6
-- //padding - 1 byte
-- };
-- Expected size:      8
-- Expected alignment: 2

data C3 = C3 Int16 Int8 Int8 Int8 Int16 deriving (Show, Generic)
instance GStorable C3

foreign import ccall newC1          :: Int32 -> Int8 -> Int8 -> IO (Ptr C1)
foreign import ccall checkFieldsC1  :: Ptr C1 -> Ptr C1 -> IO CInt
foreign import ccall checkOffsetsC1 :: Ptr CUInt -> IO CInt 
foreign import ccall getSizeC1      :: IO CInt
foreign import ccall getAlignmentC1 :: IO CInt

-- If fields ok and offsets not - the value of the fields before could use only some of the bits

-- If the offsets are ok and fields are not -- different types ? (Int vs Unsigned int).

-- Adding parametric polimorphism to reduce the boilerplate.
class (Storable a) => Checkable a where
  -- | Checks whether the fields are the same
  checkFields  :: Ptr a -> Ptr a -> IO Bool 
  -- | Checks whether the offsets are the same
  checkOffsets :: a -> Ptr CUInt -> IO Bool 
  -- | Checks whether the size is the same
  checkSize    :: a -> IO Bool              
  -- | Checks whether the alignment is the same
  checkAlignment :: a-> IO Bool              
  
  new          :: a -> IO (Ptr a)

newStorable :: Storable a => a -> IO (Ptr a)
newStorable val = do
  ptr <- malloc
  poke ptr val
  return ptr


data Check = Check {
  fields_ok :: Bool,
  offsets_ok :: Bool,
  size_ok :: Bool,
  alignment_ok :: Bool
} deriving (Show)

checkOk :: Check -> Bool
checkOk (Check f o s a) = and [f,o,s,a]

checkType :: (GStorable a, Checkable a) => a -> IO Check
checkType val = do
  -- Offsets
  let offsets = map fromIntegral $ goffsets val
  offsets_ptr <- mallocArray $ length offsets
  pokeArray offsets_ptr offsets 
 
  -- Fields
  c_ptr  <- new val
  hs_ptr <- newStorable val
  
  -- Tests
  field_test  <- checkFields  c_ptr hs_ptr
  offset_test <- checkOffsets val  offsets_ptr
  size_test   <- checkSize val   
  alignment_test <- checkAlignment val
  return $ Check field_test offset_test size_test alignment_test


instance Checkable C1 where
  checkFields  ptr1 ptr2 = (==1) <$> checkFieldsC1 ptr1 ptr2
  checkOffsets _ offs    = (==1) <$> checkOffsetsC1 offs
  checkSize         a    = (==) (fromIntegral $ sizeOf    a) <$> getSizeC1
  checkAlignment    a    = (==) (fromIntegral $ alignment a) <$> getAlignmentC1
  new (C1 a b c)         = newC1 a b c




checkC1 :: C1 -> IO Bool
checkC1 c1 = do
  checked <- checkType c1
  let is_ok = checkOk checked
      ok_text = case is_ok of
          True -> "Ok"
          False ->"Fail!"
  putStrLn $ concat ["Checking C1... ", ok_text]
  putStrLn $ show checked
  putStrLn ""
  return is_ok

main :: IO ()
main = do
  let c1 = C1 10 3 4
  
  putStrLn ""
  passed <- (liftM and) $ sequence [checkC1 c1]
  case passed of
    True -> exitSuccess
    False -> exitFailure
