{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TypeApplications #-}

module Main where 

import Foreign.Storable.TH

import GHC.Generics

import Foreign.Marshal.Alloc
import Foreign.Ptr

data TestMe = TestMe Int Int deriving (Show, Generic)
data FailMe = FailMe Int Int deriving (Show)


$(deriveGStorable @TestMe ''TestMe )



main = do 
    print $ gsizeOf    (undefined :: TestMe)
    print $ galignment (undefined :: TestMe)
    a <- malloc :: IO (Ptr TestMe)
    print =<< peek a
    poke a (TestMe 100 300)
    print =<< peek a
