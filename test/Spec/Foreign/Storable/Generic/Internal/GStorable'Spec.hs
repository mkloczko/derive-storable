{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DeriveGeneric       #-}
{-#LANGUAGE DataKinds           #-}
module Foreign.Storable.Generic.Internal.GStorable'Spec where


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
    describe "glistSizeOf'" $ do
        it "glistSizeOf' (M1 a) == glistSizeOf' a" $ do
            property (\((NestedToType (GenericType val)) :: NestedToType 2) -> do
                glistSizeOf' (M1 $ val) `shouldBe` glistSizeOf' val 
                )
        it "glistSizeOf' (K1 a) == [gsizeOf a]" $ do
            property (\((NestedToType (GenericType val)) :: NestedToType 1) -> do
                glistSizeOf' (K1 $ val) `shouldBe` [internalSizeOf val] 
                )
        it "glistSizeOf' (a :*: b) == glistSizeOf' a ++ glistSizeOf' b" $ do
            property (\((NestedToType (GenericType val1)) :: NestedToType 1) ((NestedToType (GenericType val2)) :: NestedToType 1) -> do 
                glistSizeOf' (val1 :*: val2 ) `shouldBe` (glistSizeOf' val1 ++ glistSizeOf' val2) 
                )
--         it "glistSizeOf' a == [gsizeOf a_field1, gsizeOf a_field2, ...]" $ do
--             property (\(nested_types :: NonEmptyList (NestedToType 4)) -> do
--                 let list_types =  map (\(NestedToType v) -> v) $ getNonEmpty nested_types
--                     gen_type = toGenericType list_types
--                     test (GenericType v) = glistSizeOf' v `shouldBe` map gsizeOf list_types 
--                 test gen_type 
--                 )
--     describe "glistAlignment'" $ do
--         it "Length == 1 for primitives" $ do
--             property (\(basic_type :: BasicType) -> do
--                 let test1 val = length (listOfAlignments val) `shouldSatisfy` (==1)
--                 test1 $ wrapType basic_type
--                 )
--         it "glistAlignment' a == [galignment a_field1, galignment a_field2, ...]" $ do
--             property (\(basic_types :: NonEmptyList BasicType) -> do
--                 let gen_type = toGenericType $ getNonEmpty basic_types
--                 listOfAlignments gen_type `shouldBe` map galignment (getNonEmpty basic_types) 
--                 )
--     describe "gnumberOf' " $ do
--         it "Is equal to number of fields" $ do 
--             property (\(basic_types :: NonEmptyList BasicType) -> do
--                 let gen_type          = toGenericType $ getNonEmpty basic_types
--                     getNo (GenericType p) = gnumberOf' p
--                 getNo gen_type `shouldBe` length (getNonEmpty basic_types) 
--                 )
    describe "gpeekByteOff' " $ do
        it "gpeekByteOff' [f_off] ptr off == K1 <$> gpeekByteOff ptr (f_off + off) val " $ do
            property (\((NestedToType (GenericType (val :: f p))) :: NestedToType 1) -> do
                let size = testSizeOf val
                -- Get the offsets to test
                f_off <- generate $ suchThat arbitrary (>=0) 
                off   <- generate $ suchThat arbitrary (>=0)

                -- Reserve some memory and write some data to it.
                ptr    <- mallocBytes (f_off + off + size)
                values <- generate $ vector (f_off + off + size) :: IO [Int8]
                pokeArray ptr values
               
                -- Check:
                -- Left side
                v1 <- gpeekByteOff' [f_off] ptr off     :: IO (K1 i (f p) p)
                -- Right side
                v2 <- internalPeekByteOff ptr (f_off + off) :: IO (f p)
                free ptr

                v1 `shouldBe` K1 v2

                )
--         it "Error when length of offsets > number of fields" $ do
--             property (\(gen_types :: NonEmptyList GenericType) -> do
--                 let gen_type  = toGenericType $ getNonEmpty gen_types
--                     offsets   = listOffsets gen_type ++ [4]
--                     bytes     = getSize gen_type
--                 ptr <- mallocBytes bytes
--                 testPeekByteOff gen_type offsets ptr 0 `shouldThrow` anyException
--                 )
--         it "Error when length of offsets < number of fields" $ do
--             property (\(gen_types :: NonEmptyList GenericType) -> do
--                 let gen_type  = toGenericType $ getNonEmpty gen_types
--                     offsets   = drop 1 $ listOffsets gen_type
--                     bytes     = getSize gen_type
--                 ptr <- mallocBytes bytes
--                 testPeekByteOff gen_type offsets ptr 0 `shouldThrow` anyException
--                 )
--         it "No error when length of offsets == number of fields" $ do
--             property (\(gen_types :: NonEmptyList GenericType) -> do
--                 let gen_type  = toGenericType $ getNonEmpty gen_types
--                     offsets   = listOffsets gen_type
--                     bytes     = getSize gen_type
--                 ptr <- mallocBytes bytes
--                 testPeekByteOff gen_type offsets ptr 0
--                 return ()
--                 )
    describe "gpokeByteOff' " $ do
        it "gpokeByteOff' [f_off] ptr off (K1 val) == gpokeByteOff ptr (f_off + off) val " $ do
            property (\((NestedToType (GenericType (val :: f p))) :: NestedToType 1) -> do
                let size = testSizeOf val
                -- Get the offsets to test
                f_off <- generate $ suchThat arbitrary (>=0) 
                off   <- generate $ suchThat arbitrary (>=0)

                -- Reserve some memory to read from
                ptr    <- mallocBytes (f_off + off + size)
                
                -- First test
                gpokeByteOff' [f_off] ptr off (K1 val)
                bytes1 <- peekArray (f_off + off + size) ptr :: IO [Int8]
              
                --Second test
                internalPokeByteOff ptr (f_off + off) val
                bytes2 <- peekArray (f_off + off + size) ptr :: IO [Int8]
         
                free ptr
                -- Check:
                bytes1 `shouldBe` bytes2
                )


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
