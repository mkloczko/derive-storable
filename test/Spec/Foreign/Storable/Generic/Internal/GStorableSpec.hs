{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGe DataKinds           #-}
{-#LANGUAGE DeriveGeneric       #-}
{-#LANGUAGE DeriveAnyClass      #-}
{-#LANGUAGE FlexibleContexts    #-}
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
import Foreign.Marshal.Alloc (malloc, mallocBytes, free)
import Foreign.Marshal.Array (peekArray,pokeArray)

data TestData  = TestData Int Int64 Int8 Int8
    deriving (Show, Generic, GStorable, Eq)
instance Arbitrary TestData where
    arbitrary = TestData <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


data TestData2 = TestData2 Int8 TestData Int32 Int64
    deriving (Show, Generic, GStorable, Eq)
instance Arbitrary TestData2 where
    arbitrary = TestData2 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data TestData3 = TestData3 Int64 TestData2 Int16 TestData Int8
    deriving (Show, Generic, GStorable, Eq)
instance Arbitrary TestData3 where
    arbitrary = TestData3 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


-- | Check whether gpokeByteOff modifies the specified area 
memory_bounds :: (Eq a,Arbitrary a, GStorable a) => a -> Expectation
memory_bounds test_type1 = do
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
     

sizeEquality a = do
    gsizeOf a `shouldBe` internalSizeOf (from a)


alignmentEquality a = do
    gsizeOf a `shouldBe` internalSizeOf (from a)

pokeEquality a = do
    let size = gsizeOf a
    off <- generate $ suchThat arbitrary (>=0)

    ptr <- mallocBytes (off + size)
    -- First poke
    gpokeByteOff ptr off a
    bytes1 <- peekArray (off+size) ptr :: IO [Int8]

    internalPokeByteOff ptr off (from a)
    bytes2 <- peekArray (off+size) ptr :: IO [Int8]

    free ptr
    bytes1 `shouldBe` bytes2            

peekEquality (a :: t) = do
    let size = gsizeOf a
    off   <- generate $ suchThat arbitrary (>=0)
    ptr   <- mallocBytes (off + size)
    bytes <- generate $ vector (off+size) :: IO [Int8]
   
    -- Save random stuff to memory
    pokeArray ptr bytes
   
    -- Take a peek
    v1 <- gpeekByteOff        ptr off :: IO t
    v2 <- internalPeekByteOff ptr off :: IO (Rep t p)

    free ptr
    
    v1 `shouldBe` to v2           


peekAndPoke (a :: t)= do
    ptr   <- malloc :: IO (Ptr t)
    gpokeByteOff ptr 0 a
    (gpeekByteOff ptr 0) `shouldReturn` a


spec :: Spec
spec = do
    describe "gsizeOf" $ do
        it "gsizeOf a == internalSizeOf (from a)" $ property $ do 
            test1 <- generate $ arbitrary :: IO TestData
            test2 <- generate $ arbitrary :: IO TestData2
            test3 <- generate $ arbitrary :: IO TestData3
            sizeEquality test1
            sizeEquality test2
            sizeEquality test3
    describe "galignment" $ do
        it "galignment a == internalAlignment (from a)" $ property $ do 
            test1 <- generate $ arbitrary :: IO TestData
            test2 <- generate $ arbitrary :: IO TestData2
            test3 <- generate $ arbitrary :: IO TestData3
            alignmentEquality test1
            alignmentEquality test2
            alignmentEquality test3
    describe "gpokeByteOff" $ do
        it "gpokeByteOff ptr off a = internalPokeByteOff ptr off (from a)" $ property $ do 
            test1 <- generate $ arbitrary :: IO TestData
            test2 <- generate $ arbitrary :: IO TestData2
            test3 <- generate $ arbitrary :: IO TestData3
            pokeEquality test1
            pokeEquality test2
            pokeEquality test3
        it "modifies only specified area of the memory" $ property $ do
            test1 <- generate $ arbitrary :: IO TestData
            test2 <- generate $ arbitrary :: IO TestData2
            test3 <- generate $ arbitrary :: IO TestData3
            memory_bounds test1
            memory_bounds test2
            memory_bounds test3
    describe "gpeekByteOff" $ do
        it "gpeekByteOff ptr off = to <$> internalPeekByteOff ptr off" $ property $ do 
            test1 <- generate $ arbitrary :: IO TestData
            test2 <- generate $ arbitrary :: IO TestData2
            test3 <- generate $ arbitrary :: IO TestData3
            peekEquality test1
            peekEquality test2
            peekEquality test3
    describe "Other tests:" $ do
        it "gpokeByteOff ptr 0 val >> gpeekByteOff ptr 0 == val" $ property $ do        
            test1 <- generate $ arbitrary :: IO TestData
            test2 <- generate $ arbitrary :: IO TestData2
            test3 <- generate $ arbitrary :: IO TestData3
            peekAndPoke test1
            peekAndPoke test2
            peekAndPoke test3 
