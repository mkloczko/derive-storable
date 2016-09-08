{-# LANGUAGE CPP #-}
module Foreign.Storable.Generic.Instances () where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Storable.Generic.Internal
import GHC.Fingerprint.Type
import System.Posix.Types
import Data.Ratio (Ratio)

#define MakeGStorable(Type)     \
instance GStorable Type where   \
    {-#INLINE gsizeOf #-}       \
;   gsizeOf      = sizeOf       \
;   {-#INLINE galignment #-}    \
;   galignment   = alignment    \
;   {-#INLINE gpeekByteOff #-}  \
;   gpeekByteOff = peekByteOff  \
;   {-#INLINE gpokeByteOff #-}  \
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
MakeGStorable (CSigAtomic)
MakeGStorable (CPtrdiff)
MakeGStorable (CDouble)
MakeGStorable (CFloat)
MakeGStorable (CULLong)
MakeGStorable (CLLong)
MakeGStorable (CULong)
MakeGStorable (CLong)
MakeGStorable (CUInt)
MakeGStorable (CInt)
MakeGStorable (CUShort)
MakeGStorable (CShort)
MakeGStorable (CUChar)
MakeGStorable (CSChar)
MakeGStorable (CChar)

-- Ptr
MakeGStorable (IntPtr)
MakeGStorable (WordPtr)

MakeGStorable ((StablePtr a))
MakeGStorable ((Ptr a)) 
MakeGStorable ((FunPtr a))

-- Posix
MakeGStorable (Fd)
MakeGStorable (CRLim)
MakeGStorable (CTcflag)
MakeGStorable (CSpeed)
MakeGStorable (CCc)
MakeGStorable (CUid)
MakeGStorable (CNlink)
MakeGStorable (CGid)
MakeGStorable (CSsize)
MakeGStorable (CPid)
MakeGStorable (COff)
MakeGStorable (CMode)
MakeGStorable (CIno)
MakeGStorable (CDev)
 


