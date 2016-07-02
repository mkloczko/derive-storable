{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DeriveGeneric       #-}
module GStorable'Spec where


-- Test tools
import Test.Hspec
import Test.QuickCheck
import GenericType 

-- Tested modules
import Foreign.Storable.Generic.Internal 

-- Additional data
import Foreign.Storable.Generic.Instances
import GHC.Generics
import Foreign.Marshal.Alloc (malloc, mallocBytes)

spec :: Spec
spec = do 
    describe "glistSizeOf'" $ do
        it "Length == 1 for primitives" $ do
            property (\(basic_type :: BasicType) -> do
                let test1 val = length (listOfSizes val) `shouldSatisfy` (==1)
                test1 $ wrapType basic_type
                )
        it "Length >= 1 for product types" $ do
            property (\(basic_types :: NonEmptyList BasicType) -> do
                let test1 val = length (listOfSizes val) `shouldSatisfy` (>=1)
                test1 $ wrapType $ toGenericType $ getNonEmpty basic_types
                )
        it "glistSizeOf' a == [gsizeOf a_field1, gsizeOf a_field2, ...]" $ do
            property (\(basic_types :: NonEmptyList BasicType) -> do
                let gen_type = toGenericType $ getNonEmpty basic_types
                listOfSizes gen_type `shouldBe` map gsizeOf (getNonEmpty basic_types) 
                )
    describe "glistAlignment'" $ do
        it "Length == 1 for primitives" $ do
            property (\(basic_type :: BasicType) -> do
                let test1 val = length (listOfAlignments val) `shouldSatisfy` (==1)
                test1 $ wrapType basic_type
                )
        it "Length >= 1 for product types" $ do
            property (\(basic_types :: NonEmptyList BasicType) -> do
                let test1 val = length (listOfAlignments val) `shouldSatisfy` (>=1)
                test1 $ wrapType $ toGenericType $ getNonEmpty basic_types
                )
        it "glistAlignment' a == [galignment a_field1, galignment a_field2, ...]" $ do
            property (\(basic_types :: NonEmptyList BasicType) -> do
                let gen_type = toGenericType $ getNonEmpty basic_types
                listOfAlignments gen_type `shouldBe` map galignment (getNonEmpty basic_types) 
                )
    describe "gnumberOf' " $ do
        it "Is equal to number of fields" $ do 
            property (\(basic_types :: NonEmptyList BasicType) -> do
                let gen_type          = toGenericType $ getNonEmpty basic_types
                    getNo (Product p) = gnumberOf' p
                    getNo (Part    p) = gnumberOf' p
                getNo gen_type `shouldBe` length (getNonEmpty basic_types) 
                )
    describe "gpeekByteOff' " $ do
        it "Error when length of offsets > number of fields" $ do
            property (\(gen_types :: NonEmptyList GenericType) -> do
                let gen_type  = toGenericType $ getNonEmpty gen_types
                    offsets   = listOffsets gen_type ++ [4]
                    bytes     = getSize gen_type
                ptr <- mallocBytes bytes
                testPeekByteOff gen_type offsets ptr 0 `shouldThrow` anyException
                )
        it "Error when length of offsets < number of fields" $ do
            property (\(gen_types :: NonEmptyList GenericType) -> do
                let gen_type  = toGenericType $ getNonEmpty gen_types
                    offsets   = drop 1 $ listOffsets gen_type
                    bytes     = getSize gen_type
                ptr <- mallocBytes bytes
                testPeekByteOff gen_type offsets ptr 0 `shouldThrow` anyException
                )
        it "No error when length of offsets == number of fields" $ do
            property (\(gen_types :: NonEmptyList GenericType) -> do
                let gen_type  = toGenericType $ getNonEmpty gen_types
                    offsets   = listOffsets gen_type
                    bytes     = getSize gen_type
                ptr <- mallocBytes bytes
                testPeekByteOff gen_type offsets ptr 0
                return ()
                )
    describe "gpokeByteOff' " $ do
        it "Error when length of offsets > number of fields" $ do
            property (\(gen_types :: NonEmptyList GenericType) -> do
                let gen_type  = toGenericType $ getNonEmpty gen_types
                    offsets   = listOffsets gen_type ++ [4]
                    bytes     = getSize gen_type
                ptr <- mallocBytes bytes
                testPokeByteOff offsets ptr 0 gen_type `shouldThrow` anyException
                )
        it "Error when length of offsets < number of fields" $ do
            property (\(gen_types :: NonEmptyList GenericType) -> do
                let gen_type  = toGenericType $ getNonEmpty gen_types
                    offsets   = drop 1 $ listOffsets gen_type
                    bytes     = getSize gen_type
                ptr <- mallocBytes bytes
                testPokeByteOff offsets ptr 0 gen_type `shouldThrow` anyException
                )
        it "No error when length of offsets == number of fields" $ do
            property (\(gen_types :: NonEmptyList GenericType) -> do
                let gen_type  = toGenericType $ getNonEmpty gen_types
                    offsets   = listOffsets gen_type
                    bytes     = getSize gen_type
                ptr <- mallocBytes bytes
                testPokeByteOff offsets ptr 0 gen_type
                )
