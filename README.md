# Introduction

The `generic-storable` package allows you to automatically generate Storable instances for your datatypes. It uses `generic-deriving`, which allows the coders to derive certain instances at compile time (unless the code is compiled with `-O0` flag). To derive a Storable instance, the data-type has to:

* have only one constructor.
* all fields of the constructor need to be Storable.
* implement a Generic instance (`derive (Show, Read ,..,Generic)`)

# Usage

Here's an example:


```
{-#LANGUAGE DeriveGenerics#-}

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
    let val = Position (0.0,10.0)
    ptr <- malloc :: IO (Ptr Position)	    
    putStrLn "Created a ptr with value of"
    putStrLn =<< show <$> peek ptr
    updatePosition ptr val
    putStrLn "And now the value of ptr is:"   
    putStrLn =<< show <$> peek ptr

```



