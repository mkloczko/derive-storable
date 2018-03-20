{-|
Module      : Foreign.Storable.Generic.Instances
Copyright   : (c) Mateusz Kłoczko, 2016
License     : MIT
Maintainer  : mateusz.p.kloczko@gmail.com
Stability   : experimental
Portability : portable


-}


{-#LANGUAGE CPP #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE TypeFamilies #-}
#include "MachDeps.h"
#include "HsBaseConfig.h"
module Foreign.Storable.Generic.Instances () where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Storable.Generic.Internal
import GHC.Fingerprint.Type
import GHC.Storable

import System.Posix.Types
import Data.Ratio (Ratio)


#define Testy(T,val) \
test_T = val

Testy(Int32, SIZEOF_INT32)

test_val = SIZEOF_INT32

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

#define MakeGStorableTypeFun(T, size, align, read, write) \
instance GStorable T where                                \
;   type GSize T = size                                   \
;   type GAlignment T = align                             \
;   gsizeOf    _ = size                                   \
;   galignment _ = align                                  \
;   gpeekByteOff ptr off = read  (ptr `plusPtr` off) 0    \
;   gpokeByteOff ptr off = write (ptr `plusPtr` off) 0    \

MakeGStorableTypeFun(Char,SIZEOF_INT32,ALIGNMENT_INT32, readWideCharOffPtr,writeWideCharOffPtr)

--MakeGStorableTypeFun(Float,SIZEOF_HSFLOAT,ALIGNMENT_HSFLOAT,
--         readFloatOffPtr,writeFloatOffPtr)
--
--MakeGStorableTypeFun(Double,SIZEOF_HSDOUBLE,ALIGNMENT_HSDOUBLE,
--         readDoubleOffPtr,writeDoubleOffPtr)

-- Haskell primitives
MakeGStorable(Bool)
--MakeGStorable(Char)
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

MakeGStorable(Fingerprint)

-- C primitives
MakeGStorable(CUIntMax)
MakeGStorable(CIntMax)
MakeGStorable(CSUSeconds)
MakeGStorable(CUSeconds)
MakeGStorable(CTime)
MakeGStorable(CClock)
MakeGStorable(CSigAtomic)
MakeGStorable(CPtrdiff)
MakeGStorable(CDouble)
MakeGStorable(CFloat)
MakeGStorable(CULLong)
MakeGStorable(CLLong)
MakeGStorable(CULong)
MakeGStorable(CLong)
MakeGStorable(CUInt)
MakeGStorable(CInt)
MakeGStorable(CUShort)
MakeGStorable(CShort)
MakeGStorable(CUChar)
MakeGStorable(CSChar)
MakeGStorable(CChar)

-- Ptr
MakeGStorable(IntPtr)
MakeGStorable(WordPtr)

MakeGStorable((StablePtr a))
MakeGStorable((Ptr a)) 
MakeGStorable((FunPtr a))

-- Posix
MakeGStorable(Fd)
#if defined(HTYPE_RLIM_T)
MakeGStorable(CRLim)
#endif
#if defined(HTYPE_TCFLAG_T)
MakeGStorable(CTcflag)
#endif
#if defined(HTYPE_SPEED_T)
MakeGStorable(CSpeed)
#endif
#if defined(HTYPE_CC_T)
MakeGStorable(CCc)
#endif
#if defined(HTYPE_UID_T)
MakeGStorable(CUid)
#endif
#if defined(HTYPE_NLINK_T)
MakeGStorable(CNlink)
#endif
#if defined(HTYPE_GID_T)
MakeGStorable(CGid)
#endif
#if defined(HTYPE_SSIZE_T)
MakeGStorable(CSsize)
#endif
#if defined(HTYPE_PID_T)
MakeGStorable(CPid)
#endif
#if defined(HTYPE_OFF_T)
MakeGStorable(COff)
#endif
#if defined(HTYPE_MODE_T)
MakeGStorable(CMode)
#endif
#if defined(HTYPE_INO_T)
MakeGStorable(CIno)
#endif
#if defined(HTYPE_DEV_T)
MakeGStorable(CDev)
#endif
 


