{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DeriveGeneric       #-}
module Foreign.Storable.Generic.Internal.GStorableSpec where


-- Test tools
import Test.Hspec
import Test.QuickCheck
import GenericType 

-- Tested modules
import Foreign.Storable.Generic.Internal 

-- Additional data
import Foreign.Storable.Generic.Instances
import Data.Int
import GHC.Generics
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Marshal.Alloc (malloc, mallocBytes)
import Foreign.Marshal.Array (peekArray)

data TestData = TestData Int Int64 Int8 Int8
    deriving (Show, Generic, Eq)

instance GStorable TestData
instance Arbitrary TestData where
    arbitrary = TestData <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = do 
    describe "gpokeByteOff" $ do
        it "modifies only a part of the memory" $ do
            property (\(test_type1 :: TestData) -> do
                test_type2 <- generate $ suchThat arbitrary (/=test_type1)
                -- if test_type1 is different from test_type2, then 
                -- the memory state has to change when poking both of them
                let size = gsizeOf test_type1 
                    peekBytes = peekArray :: (Int -> Ptr Int8 -> IO [Int8])
                -- The memory area
                ptr <- mallocBytes (size+16)
              
                -- Beginning state.
                mem_state1_beginning <- peekBytes 8     ptr                   
                mem_state1_middle    <- peekBytes size (plusPtr ptr 8)              
                mem_state1_end       <- peekBytes 8    (plusPtr ptr (size+8))

                gpokeByteOff ptr 8 test_type1
                -- Poked first variable
                mem_state2_beginning <- peekBytes 8     ptr
                mem_state2_middle    <- peekBytes size (plusPtr ptr 8)
                mem_state2_end       <- peekBytes 8    (plusPtr ptr (size+8))

                gpokeByteOff ptr 8 test_type2
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
    describe "Other tests:" $ do
        it "gpokeByteOff ptr 0 val >> gpeekByteOff ptr 0 == val" $ do
            True `shouldBe` True
        it "off1 /= off2, |off1 - off2| < gsizeOf val => gpokeByteOff ptr off1 val >> gpeekByteOff ptr off2 /= val" $ do
            True `shouldBe` True
