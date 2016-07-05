{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DeriveGeneric       #-}
{-#LANGUAGE DataKinds           #-}
module Foreign.Storable.Generic.Internal.InternalSpec where


-- Test tools
import Test.Hspec
import Test.QuickCheck
import GenericType 

-- Tested modules
import Foreign.Storable.Generic.Internal 

-- Additional data
import Foreign.Storable.Generic.Instances
import GHC.Generics
import Foreign.Marshal.Alloc (malloc, mallocBytes, free)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Data.Int



spec :: Spec
spec = do 
    describe "internalSizeOf" $ do
        it "is equal to calcSize $ zip (glistSizeOf' a) (glistAlignment' a)" $ do
            property (\(GenericType val) -> internalSizeOf val `shouldBe` (calcSize $ zip (glistSizeOf' val) (glistAlignment' val) ) )
    -- describe "internalAlignment" $ do
    -- describe "internalPeekByteOff" $ do
    -- describe "internalPeekByteOff" $ do
    -- describe "internalPeekByteOff" $ do
    -- describe "internalOffsets" $ do

--     describe "gpokeByteOff' " $ do
--         it "Error when length of offsets > number of fields" $ do
--             property (\(gen_types :: NonEmptyList GenericType) -> do
--                 let gen_type  = toGenericType $ getNonEmpty gen_types
--                     offsets   = listOffsets gen_type ++ [4]
--                     bytes     = getSize gen_type
--                 ptr <- mallocBytes bytes
--                 testPokeByteOff offsets ptr 0 gen_type `shouldThrow` anyException
--                 )
--         it "Error when length of offsets < number of fields" $ do
--             property (\(gen_types :: NonEmptyList GenericType) -> do
--                 let gen_type  = toGenericType $ getNonEmpty gen_types
--                     offsets   = drop 1 $ listOffsets gen_type
--                     bytes     = getSize gen_type
--                 ptr <- mallocBytes bytes
--                 testPokeByteOff offsets ptr 0 gen_type `shouldThrow` anyException
--                 )
--         it "No error when length of offsets == number of fields" $ do
--             property (\(gen_types :: NonEmptyList GenericType) -> do
--                 let gen_type  = toGenericType $ getNonEmpty gen_types
--                     offsets   = listOffsets gen_type
--                     bytes     = getSize gen_type
--                 ptr <- mallocBytes bytes
--                 testPokeByteOff offsets ptr 0 gen_type
--                 )
