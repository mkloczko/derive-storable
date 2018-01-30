{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module TestCases where 

import GHC.Generics (Generic)
import Foreign.Storable.Generic
import Data.Int
import Control.DeepSeq


data C1 = C1 Int32                 deriving (Show, Generic, GStorable, NFData)
data C2 = C2 Int8 Int32 Int16      deriving (Show, Generic, GStorable, NFData)
data C3 = C3 C2 Int64 C1           deriving (Show, Generic, GStorable, NFData)
data C4 = C4 Double Int8 C3        deriving (Show, Generic, GStorable, NFData)
data C5 = C5 Int32 C2 C4           deriving (Show, Generic, GStorable, NFData)

c1_def = C1 3
c2_def = C2 3 10 8
c3_def = C3 c2_def 11000 c1_def
c4_def = C4 0.312 3 c3_def 
c5_def = C5 100 c2_def c4_def 


data C1hw = C1hw Int32                 deriving (Show,Generic, NFData)
data C2hw = C2hw Int8 Int32 Int16      deriving (Show,Generic, NFData)
data C3hw = C3hw C2hw Int64 C1hw       deriving (Show,Generic, NFData)
data C4hw = C4hw Double Int8 C3hw      deriving (Show,Generic, NFData)
data C5hw = C5hw Int32 C2hw C4hw       deriving (Show,Generic, NFData)

c1hw_def = C1hw 3
c2hw_def = C2hw 3 10 8
c3hw_def = C3hw c2hw_def 11000 c1hw_def
c4hw_def = C4hw 0.312 3 c3hw_def 
c5hw_def = C5hw 100 c2hw_def c4hw_def 

instance Storable C1hw where
    sizeOf                         _ = 4
    alignment                      _ = 4
    peekByteOff ptr off              = C1hw <$> (peekByteOff ptr off :: IO Int32) 
    pokeByteOff ptr off (C1hw v) = pokeByteOff ptr off v

instance Storable C2hw where
    sizeOf                         _ = 12
    alignment                      _ = 4
    peekByteOff ptr off              = C2hw 
        <$> (peekByteOff ptr off        :: IO Int8 ) 
        <*> (peekByteOff ptr (off + 4)  :: IO Int32) 
        <*> (peekByteOff ptr (off + 8)  :: IO Int16) 
                                       
    pokeByteOff ptr off (C2hw i8a i32 i16) = do
        pokeByteOff ptr  off       i8a
        pokeByteOff ptr (off + 4)  i32
        pokeByteOff ptr (off + 8)  i16

instance Storable C3hw where
    sizeOf              _ = 32
    alignment           _ = 8
    peekByteOff ptr off = C3hw 
        <$> (peekByteOff ptr off        :: IO C2hw ) 
        <*> (peekByteOff ptr (off + 16) :: IO Int64) 
        <*> (peekByteOff ptr (off + 24) :: IO C1hw) 
                                       
    pokeByteOff ptr off (C3hw c2hw i64 c1hw) = do
        pokeByteOff ptr  off        c2hw
        pokeByteOff ptr (off + 16)  i64
        pokeByteOff ptr (off + 24)  c1hw

instance Storable C4hw where
    sizeOf              _ = 48
    alignment           _ = 8
    peekByteOff ptr off   = C4hw 
        <$> (peekByteOff ptr  off       :: IO Double)
        <*> (peekByteOff ptr (off + 8)  :: IO Int8  )
        <*> (peekByteOff ptr (off + 16) :: IO C3hw  )
    pokeByteOff ptr off (C4hw dbl i8 c3hw) = do
        pokeByteOff ptr  off       dbl
        pokeByteOff ptr (off + 8)  i8
        pokeByteOff ptr (off + 16) c3hw

instance Storable C5hw where 
    sizeOf              _ = 64
    alignment           _ = 8
    peekByteOff ptr off   = C5hw 
        <$> (peekByteOff ptr  off       :: IO Int32)
        <*> (peekByteOff ptr (off + 4 ) :: IO C2hw )
        <*> (peekByteOff ptr (off + 16) :: IO C4hw )
    pokeByteOff ptr off (C5hw i32 c2hw c4hw) = do
        pokeByteOff ptr  off       i32
        pokeByteOff ptr (off + 4)  c2hw
        pokeByteOff ptr (off + 16) c4hw
