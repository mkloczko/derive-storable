{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DeriveGeneric       #-}
{-#LANGUAGE DataKinds           #-}
module Foreign.Storable.Generic.InternalSpec where


-- Test tools
import Test.Hspec
import Test.QuickCheck
import GenericType 

-- Tested modules
import Foreign.Storable.Generic.Internal 

-- Additional data
import Foreign.Storable.Generic.Tools
import Foreign.Storable.Generic.Instances
import GHC.Generics
import Foreign.Marshal.Alloc (malloc, mallocBytes, free)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (Ptr, plusPtr)
import Data.Word



spec :: Spec
spec = do 
    describe "internalSizeOf" $ do
        it "is equal to: calcSize $ zip (glistSizeOf' a) (glistAlignment' a)" $ do
            property (\((NestedToType (GenericType val)) :: NestedToType 4) -> 
                internalSizeOf val `shouldBe` (calcSize $ zip (glistSizeOf' val) (glistAlignment' val) ) )
    describe "internalAlignment" $ do
        it "is equal to: maximum (glistAlignment' a)" $ do
            property (\((NestedToType (GenericType val)) :: NestedToType 4) -> 
                internalAlignment val `shouldBe` (maximum $ glistAlignment' val) )
    describe "internalOffsets" $ do
        it "is equal to: calcOffsets $ zip (glistSizeOf' a) (glistAlignment' a)" $ do
            property (\((NestedToType (GenericType val)) :: NestedToType 4) -> 
                internalOffsets val `shouldBe` (calcOffsets $ zip (glistSizeOf' val) (glistAlignment' val) ) )
    describe "internalPeekByteOff" $ do
        it "is equal to: gpeekByteOff' (internalOffsets a) ptr off" $ do
            property (\((NestedToType (GenericType (val :: f p))) :: NestedToType 4) -> do
                let size      = internalSizeOf val
                    no_fields = gnumberOf' (undefined :: f p)
                off <- generate $ suchThat arbitrary (\x -> x>=0 && x < 100)
                
                -- Area in memory to peek
                bytes <- generate $ ok_vector (off + size)
                ptr <- mallocBytes (off + size)
                pokeArray ptr bytes
                
                -- first peek
                v1 <- internalPeekByteOff ptr off :: IO (f p)
                v2 <- gpeekByteOff' (internalOffsets val) (no_fields - 1) ptr off :: IO (f p)
                
                free ptr
                v1 `shouldBe` v2
                )
        it "it reads only specified area of the memory" $ do
            property $ (\((NestedToType (GenericType (test_type1 :: f p))) :: NestedToType 4) -> do    
                let size      = internalSizeOf test_type1 
                    pokeBytes = pokeArray :: (Ptr Word8 -> [Word8] -> IO ())
                -- The memory area
                ptr <- mallocBytes (size+16)
                
                -- Beginning state.
                bytes1_beginning <- generate $ ok_vector 8
                bytes1_middle    <- generate $ ok_vector size
                bytes1_end       <- generate $ ok_vector 8
                
                pokeBytes  ptr                   bytes1_beginning
                pokeBytes (plusPtr ptr 8)        bytes1_middle
                pokeBytes (plusPtr ptr (size+8)) bytes1_end
            
                v1 <- internalPeekByteOff ptr 8 :: IO (f p)
                
                -- Changed state 
                bytes2_beginning <- generate $ suchThat (ok_vector 8) (/=bytes1_beginning)
                bytes2_end       <- generate $ suchThat (ok_vector 8) (/=bytes1_end)
                
                pokeBytes  ptr               bytes2_beginning
                pokeBytes (plusPtr ptr (size + 8)) bytes2_end
                
                v2 <- internalPeekByteOff ptr 8 :: IO (f p) 
 
                v1 `shouldBe` v2
                )
    describe "internalPokeByteOff" $ do
        it "is equal to: gpokeByteOff' (internalOffsets a) ptr off v" $ do
            property (\((NestedToType (GenericType (val :: f p))) :: NestedToType 4)  -> do
                let size = internalSizeOf val
                    no_fields = gnumberOf' (undefined :: f p)
                off <- generate $ suchThat arbitrary (\x -> x>=0 && x < 100)
                
                -- Area in memory to poke
                ptr <- mallocBytes (off + size)
                
                -- first poke
                internalPokeByteOff ptr off val
                bytes1 <- peekArray (off + size) ptr :: IO [Word8]
                
                -- second poke
                gpokeByteOff' (internalOffsets val) (no_fields - 1) ptr off val
                bytes2 <- peekArray (off + size) ptr :: IO [Word8]
 
                free ptr
                
                bytes1 `shouldBe` bytes2
                )
        it "it modifies only specified area of the memory" $ do
            property $ (\((NestedToType (GenericType test_type1)) ::NestedToType 4)-> do    
                test_type2 <- generate $ suchThat arbitrary (/=test_type1)
                -- if test_type1 is different from test_type2, then 
                -- the memory state has to change when poking both of them
                let size = internalSizeOf test_type1 
                    peekBytes = peekArray :: (Int -> Ptr Word8 -> IO [Word8])
                -- The memory area
                ptr <- mallocBytes (size+16)
               
                -- Beginning state.
                mem_state1_beginning <- peekBytes 8     ptr                   
                mem_state1_middle    <- peekBytes size (plusPtr ptr 8)              
                mem_state1_end       <- peekBytes 8    (plusPtr ptr (size+8))
            
                internalPokeByteOff ptr 8 test_type1
                -- Poked first variable
                mem_state2_beginning <- peekBytes 8     ptr
                mem_state2_middle    <- peekBytes size (plusPtr ptr 8)
                mem_state2_end       <- peekBytes 8    (plusPtr ptr (size+8))
            
                internalPokeByteOff ptr 8 test_type2
                -- Poked second state
                mem_state3_beginning <- peekBytes 8     ptr
                mem_state3_middle    <- peekBytes size (plusPtr ptr 8)
                mem_state3_end       <- peekBytes 8    (plusPtr ptr (size+8))
                
  
                -- Beginnings and ends should stay the same. The middle one should be different from
                -- the one at the beginning.
                sequence_ [mem_state1_beginning `shouldBe` mem_state2_beginning
                          ,mem_state2_beginning `shouldBe` mem_state3_beginning
                          ,mem_state1_end       `shouldBe` mem_state2_end                  
                          ,mem_state2_end       `shouldBe` mem_state3_end
                          ,(mem_state1_middle /= mem_state2_middle) || (mem_state1_middle /= mem_state3_middle) `shouldBe` True]
                )
    describe "other" $ do
        it "poke, then peek: receive the poked value" $ do
            property $ (\((NestedToType (GenericType (val :: f p) )) :: NestedToType 4) -> do
                let size = internalSizeOf val
                off <- generate $ suchThat arbitrary (\x -> x>=0 && x < 100)
                
                ptr <- mallocBytes (size + off)

                internalPokeByteOff ptr off val
                p_val <- internalPeekByteOff ptr off :: IO (f p) 
                val `shouldBe` p_val
                ) 
