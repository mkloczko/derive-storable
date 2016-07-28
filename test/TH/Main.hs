{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}

module Main where 

import Foreign.Storable.Generic.Internal.TH
import Foreign.Storable.Generic
import GHC.Generics
import Foreign.Marshal.Alloc
import Foreign.Ptr

data A = A Int Int deriving (Show, Generic)

$(deriveGStorable ''A (undefined :: A) )



main = do 
    print $ gsizeOf    (undefined :: A)
    print $ galignment (undefined :: A)
    a <- malloc :: IO (Ptr A)
    print =<< peek a
    poke a (A 100 300)
    print =<< peek a
