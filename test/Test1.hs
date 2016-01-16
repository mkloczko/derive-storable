module Main where


--import Foreign.Storable.Generic
import Foreign.Storable.Tuple
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc

import Data.Int

import Control.Monad


isEqual :: (Eq a, Storable a) => a -> IO Bool
isEqual val = do
        ptr <- malloc 
        poke ptr val
        val2 <- peek ptr
        return $ val == val2    

main = do
    let v1 = (126,2^31-2,2^15-1,-126) :: (Int8, Int32, Int16, Int8)
        v2 = (2^31-2,2^31-2,2^31-1)   :: (Int32, Int32, Int32)
        v3 = (10,20,30)               :: (Int8, Int8, Int16)
        v4 = (10 :: Int8, v1, 20 :: Int32)
    are_ok <- sequence [isEqual v1,
                        isEqual v2,
                        isEqual v3,
                        isEqual v4]
    putStrLn $ show $ and are_ok
