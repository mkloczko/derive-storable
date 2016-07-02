{-# LANGUAGE CPP #-}
module Foreign.Storable.Generic.Instances where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Storable
import Foreign.Storable.Generic.Internal
import GHC.Fingerprint.Type
#define MakeGStorable(Type)     \
instance GStorable Type where   \
    gsizeOf      = sizeOf       \
;   galignment   = alignment    \
;   gpeekByteOff = peekByteOff  \
;   gpokeByteOff = pokeByteOff  \

-- Haskell primitives
MakeGStorable(Bool)
MakeGStorable(Char)
MakeGStorable(Double)
MakeGStorable(Float)

MakeGStorable(Int)
MakeGStorable(Int8)
MakeGStorable(Int16)
MakeGStorable(Int32)
MakeGStorable(Int64)

MakeGStorable(Word)
MakeGStorable(Word8)
MakeGStorable(Word16)
MakeGStorable(Word32)
MakeGStorable(Word64)

MakeGStorable (Fingerprint)

-- C primitives
MakeGStorable (CUIntMax)
MakeGStorable (CIntMax)
MakeGStorable (CSUSeconds)
MakeGStorable (CUSeconds)
MakeGStorable (CTime)
MakeGStorable (CClock)

-- and so on..
