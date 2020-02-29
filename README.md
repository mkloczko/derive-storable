# Introduction

[![Build Status](https://travis-ci.org/mkloczko/derive-storable.svg?branch=master)](https://travis-ci.org/mkloczko/derive-storable)

The `derive-storable` package allows you to automatically generate Storable instances for your datatypes. It uses GHC.Generics, which allows the coders to derive certain instances automatically. To derive a (G)Storable instance, the data-type has to:

* ~~have only one constructor~~ There is now a `sumtypes` option for data-types with multiple constructors. See **Sum types** section for more.
* all fields of the constructor need to be GStorable.
* implement a Generic instance (`derive (Generic)`)

### Sum types

To enable support for sum types, add a `-f sumtypes` option to `cabal new-build` or `cabal new-configure`. The library discerns between sum and non-sum types. Non-sum types have the same memory layout as C structs, while sum types correspond to tagged unions: 

```c
struct datatype {
    unsigned char tag;
    union { 
        constructor1 a;
        constructor2 b;
        ...
    } val;
};

```

Note - while it is possible to have an instance for a self/mutually recursive data-type, using methods for the data-type will result in infinite loop. So there is no support for recursion in data-types.

### Note on performance

There are some problems with performance of derived Storable instances. For now there exists a solution in form of GHC Core plugin - [derive-storable-plugin](https://hackage.haskell.org/package/derive-storable-plugin).

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

