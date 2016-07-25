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
import Data.Word



spec :: Spec
spec = do 
    describe "glistSizeOf'" $ do
        it "instance M1    is equal to: glistSizeOf' a" $ do
            property (\(GenericType val) -> do
                glistSizeOf' (M1 $ val) `shouldBe` glistSizeOf' val 
                )
        it "instance K1    is equal to: [internalSizeOf a]" $ do
            property (\(GenericType val) -> do
                glistSizeOf' (K1 $ val) `shouldBe` [internalSizeOf val] 
                )
        it "instance (:*:) is equal to: glistSizeOf' a ++ glistSizeOf' b" $ do
            property (\(GenericType val1) (GenericType val2) -> do 
                glistSizeOf' (val1 :*: val2 ) `shouldBe` (glistSizeOf' val1 ++ glistSizeOf' val2) 
                )
    describe "glistAlignment'" $ do
        it "instance M1    is equal to: glistAlignment' a" $ do
            property (\(GenericType val) -> do
                glistAlignment' (M1 $ val) `shouldBe` glistAlignment' val 
                )
        it "instance K1    is equal to: [internalAlignment a]" $ do
            property (\(GenericType val) -> do
                glistAlignment' (K1 $ val) `shouldBe` [internalAlignment val] 
                )
        it "instance (:*:) is equal to: glistAlignment' a ++ glistAlignment' b" $ do
            property (\(GenericType val1) (GenericType val2) -> do 
                glistAlignment' (val1 :*: val2 ) `shouldBe` (glistAlignment' val1 ++ glistAlignment' val2) 
                )
    describe "gnumberOf' " $ do
        it "instance M1    is equal to: gnumberOf' a" $ do
            property (\(GenericType val) -> do
                gnumberOf' (M1 $ val) `shouldBe` gnumberOf' val 
                )
        it "instance K1    is equal to: 1" $ do
            property (\(GenericType val) -> do
                gnumberOf' (K1 $ val) `shouldBe` 1
                )
        it "instance (:*:) is equal to: gnumberOf' a + gnumberOf' b" $ do
            property (\(GenericType val1) (GenericType val2) -> do 
                gnumberOf' (val1 :*: val2 ) `shouldBe` (gnumberOf' val1 + gnumberOf' val2) 
                )
    describe "gpeekByteOff' " $ do
        it "instance M1    is equal to: M1 <$> gpeekByteOff' offs ix ptr off" $ do
            property (\(GenericType (val :: f p)) -> do                
                let size      = internalSizeOf  val
                    offs      = internalOffsets val
                    no_fields = gnumberOf' (undefined :: f p)
                -- Random global offset
                off   <- generate $ suchThat arbitrary (\x -> x>=0 && x < 100)

                -- Reserve some memory and write some data to it.
                ptr    <- mallocBytes (off + size)
                values <- generate $ ok_vector (off + size) :: IO [Word8]
                pokeArray ptr values

                -- Check:
                -- With M1
                v1 <- gpeekByteOff' offs (no_fields - 1) ptr off     :: IO (M1 i c f p)
                -- Without M1
                v2 <- gpeekByteOff' offs (no_fields - 1) ptr off     :: IO (f p)
                free ptr

                v1 `shouldBe` M1 v2
                )
        it "instance K1    is equal to: K1 <$> internalPeekByteOff ptr (f_off + off) val" $ do
            property (\(GenericType (val :: f p)) -> do
                let size = internalSizeOf val
                -- Random global offsets and field offset
                f_off <- generate $ suchThat arbitrary (>=0) 
                off   <- generate $ suchThat arbitrary (>=0)

                -- Reserve some memory and write some data to it.
                ptr    <- mallocBytes (f_off + off + size)
                values <- generate $ ok_vector (f_off + off + size) :: IO [Word8]
                pokeArray ptr values
               
                -- Check:
                -- With K1
                v1 <- gpeekByteOff' [f_off] 0 ptr off     :: IO (K1 i (f p) p)
                -- Without K1
                v2 <- internalPeekByteOff ptr (f_off + off) :: IO (f p)
                free ptr

                v1 `shouldBe` K1 v2

                )
        it "instance (:*:) is equal to: (:*:) <$> peeker (ix - n2) <*> peeker ix  \n\
            \                                where peeker n_ix   = gpeekByteOff' offsets n_ix ptr off \n" $ do
            property (\(GenericType (val1 :: f p)) (GenericType (val2 :: g p)) -> do                
                let offsets   = internalOffsets (undefined :: (:*:) f g p)
                    size      = internalSizeOf (undefined :: (:*:) f g p)
                    no_fields = gnumberOf' (undefined :: (:*:) f g p)
                    n2        = gnumberOf' (undefined :: g p)


                -- Random global offset
                off   <- generate $ suchThat arbitrary (>=0)

                -- Reserve some memory and write some data to it.
                ptr    <- mallocBytes (off + size)
                values <- generate $ ok_vector (off + size) :: IO [Word8]
                pokeArray ptr values

                -- Check:
                -- Left side
                v1   <- gpeekByteOff' offsets (no_fields - 1)      ptr off :: IO ((:*:) f g p)
                -- Right side
                v2_a <- gpeekByteOff' offsets (no_fields - 1 - n2) ptr off :: IO (f p)
                v2_b <- gpeekByteOff' offsets (no_fields - 1)      ptr off :: IO (g p)
                free ptr

                v1 `shouldBe` (v2_a :*: v2_b)
                )
        it "crashes when ix /= number of fields" $ do 
            property (\(GenericType (val1 :: f p)) -> do
                let offsets   = internalOffsets val1
                    no_fields = gnumberOf' val1 
                -- The bad index    
                bad_ix <- generate $ suchThat arbitrary (/=no_fields)
                -- Poked area
                ptr <- mallocBytes $ internalSizeOf val1
                -- The test
                (gpeekByteOff' offsets bad_ix ptr 0 :: IO (f p)) `shouldThrow` anyException
                -- Freeing the pointer
                free ptr
                )
    describe "gpokeByteOff' " $ do
        it "instance M1    is equal to: gpokeByteOff' offs ix ptr off val" $ do
            property (\(GenericType (val :: f p)) -> do
                let size    = internalSizeOf val
                    offsets = internalOffsets val 
                    no_fields= gnumberOf' (undefined :: f p)
                -- Get the offsets to test
                off   <- generate $ suchThat arbitrary (>=0)

                -- Reserve some memory to read from
                ptr    <- mallocBytes (off + size)
                
                -- First test
                -- With M1
                gpokeByteOff' offsets (no_fields - 1) ptr off (M1 val)
                bytes1 <- peekArray (off + size) ptr :: IO [Word8]
              
                -- Second test
                -- Without M1
                gpokeByteOff' offsets (no_fields - 1) ptr off val
                bytes2 <- peekArray (off + size) ptr :: IO [Word8]
         
                free ptr
                -- Check:
                bytes1 `shouldBe` bytes2
                )
        it "instance K1    is equal to: internalPokeByteOff ptr (f_off + off) val" $ do
            property (\(GenericType (val :: f p)) -> do
                let size = internalSizeOf val
                -- Get the offsets to test
                f_off <- generate $ suchThat arbitrary (>=0) 
                off   <- generate $ suchThat arbitrary (>=0)

                -- Reserve some memory to read from
                ptr    <- mallocBytes (f_off + off + size)
                
                -- First test
                -- With K1
                gpokeByteOff' [f_off] 0 ptr off (K1 val)
                bytes1 <- peekArray (f_off + off + size) ptr :: IO [Word8]
              
                -- Second test
                -- Without K1
                internalPokeByteOff ptr (f_off + off) val
                bytes2 <- peekArray (f_off + off + size) ptr :: IO [Word8]
         
                free ptr
                -- Check:
                bytes1 `shouldBe` bytes2
                )
        it "instance (:*:) is equal to: (:*:) <$> poker (ix - n2) a <*> poker ix b \n\
            \                                where poker n_ix v  = gpokeByteOff' offsets n_ix ptr off v" $ do
            property (\(GenericType (val1 :: f p)) (GenericType (val2 :: g p)) -> do
                let offsets   = internalOffsets (undefined :: (:*:) f g p)
                    no_fields = gnumberOf'      (undefined :: (:*:) f g p)
                    n2        = gnumberOf'      (undefined :: g p        )
                    size      = internalSizeOf  (undefined :: (:*:) f g p)
                
                -- Get the offset to test
                off   <- generate $ suchThat arbitrary (>=0)

                -- Reserve some memory to read from
                ptr    <- mallocBytes (off + size)
                
                -- First poke
                -- Left part of the tree
                gpokeByteOff' offsets (no_fields - 1) ptr off (val1 :*: val2)
                bytes1 <- peekArray (off + size) ptr :: IO [Word8]
              
                -- Second pokes
                -- Right part of the tree
                gpokeByteOff' offsets (no_fields - 1 - n2) ptr off val1
                gpokeByteOff' offsets (no_fields -1)       ptr off val2
                
                bytes2 <- peekArray (off + size) ptr :: IO [Word8]
         
                free ptr
                -- Check:
                bytes1 `shouldBe` bytes2
                )
        it "crashes when ix /= number of fields" $ do 
            property (\(GenericType (val1 :: f p)) -> do
                let offsets   = internalOffsets val1
                    no_fields = gnumberOf' val1 
                -- The bad index    
                bad_ix <- generate $ suchThat arbitrary (/=no_fields)
                -- Poked area
                ptr <- mallocBytes $ internalSizeOf val1
                -- The test
                gpokeByteOff' offsets bad_ix ptr 0 val1 `shouldThrow` anyException
                -- Freeing the pointer
                free ptr
                )
