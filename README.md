# Introduction

The `derive-storable` package allows you to automatically generate Storable instances for your datatypes. It uses GHC.Generics, which allows the coders to derive certain instances automatically. To derive a (G)Storable instance, the data-type has to:

* have only one constructor.
* all fields of the constructor need to be GStorable.
* implement a Generic instance (`derive (Generic)`)

In order to achieve the same performance as for hand-written instances, take a look at [derive-storable-plugin](https://hackage.haskell.org/package/derive-storable-plugin).

# Usage

Here's an example:


```haskell
{-# LANGUAGE DeriveGeneric #-}

import Foreign.Storable
import Foreign.Storable.Generic
import Foreign.Ptr
import Foreign.Marshal.Alloc

import Generics.Deriving

data Position = Position {
   x :: Double, 
   y :: Double
} deriving (Show,Read, Generic)

instance GStorable Position

updatePosition :: Ptr Position -> Position -> IO ()
updatePosition ptr pos = poke ptr pos


main = do
    let val = Position 0.0 10.0
    ptr <- malloc :: IO (Ptr Position)      
    putStrLn "Created a ptr with value of"
    putStrLn =<< show <$> peek ptr
    updatePosition ptr val
    putStrLn "And now the value of ptr is:"   
    putStrLn =<< show <$> peek ptr

```


