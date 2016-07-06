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
        it "instance M1    is equal to: M1 <$> gpeekByteOff' offs ptr off" $ do
            property (\(GenericType (val :: f p)) -> do                
                let size = internalSizeOf  val
                    offs = internalOffsets val
                -- Get the offsets to test
                off   <- generate $ suchThat arbitrary (>=0)

                -- Reserve some memory and write some data to it.
                ptr    <- mallocBytes (off + size)
                values <- generate $ vector (off + size) :: IO [Int8]
                pokeArray ptr values

                -- Check:
                -- Left side
                v1 <- gpeekByteOff' offs ptr off     :: IO (M1 i c f p)
                -- Right side
                v2 <- gpeekByteOff' offs ptr off     :: IO (f p)
                free ptr

                v1 `shouldBe` M1 v2
                )
        it "instance K1    is equal to: K1 <$> internalPeekByteOff ptr (f_off + off) val" $ do
            property (\(GenericType (val :: f p)) -> do
                let size = internalSizeOf val
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
        it "instance (:*:) is equal to: (:*:) <$> peeker offs_l <*> peeker offs_r  \n\
            \                                where peeker my_offs   = gpeekByteOff' my_offs ptr off \n\
            \                                      (offs_l, offs_r) = splitAt (gnumberOf' (undefined :: f p)) offs" $ do
            property (\(GenericType (val1 :: f p)) (GenericType (val2 :: g p)) -> do                
                let offsets = internalOffsets (undefined :: (:*:) f g p)
                    (offs_l, offs_r) = splitAt (gnumberOf' val1) offsets
                    size    = internalSizeOf (undefined :: (:*:) f g p)

                -- Get the offsets to test
                off   <- generate $ suchThat arbitrary (>=0)

                -- Reserve some memory and write some data to it.
                ptr    <- mallocBytes (off + size)
                values <- generate $ vector (off + size) :: IO [Int8]
                pokeArray ptr values

                -- Check:
                -- Left side
                v1 <- gpeekByteOff' offsets ptr off      :: IO ((:*:) f g p)
                -- Right side
                v2_a <- gpeekByteOff' offs_l ptr off   :: IO (f p)
                v2_b <- gpeekByteOff' offs_r ptr off  :: IO (g p)
                free ptr

                v1 `shouldBe` (v2_a :*: v2_b)
                )
        it "instance K1    crashes when length of supplied offsets /= 1" $ do 
            property (\(GenericType (val1 :: f p)) -> do
                to_gen <- generate $ suchThat arbitrary (\v -> (v/=1)  && (v>=0) )
                offs   <- generate $ vector to_gen

                ptr <- mallocBytes $ internalSizeOf (undefined :: K1 i (f p) p)
                (gpeekByteOff' offs ptr 0 :: IO (K1 i (f p) p)) `shouldThrow` anyException
                free ptr
                )
        it "instance (:*:) crashes when length of supplied offsets /= no. of fields" $ do 
            property (\(GenericType (_ :: f p)) (GenericType (_ :: g p)) -> do
                let org_offs = internalOffsets (undefined :: (:*:) f g p)
                    len      = length org_offs
                to_gen <- generate $ suchThat arbitrary (\v -> (v/=len)  && (v>=0) )
                offs   <- generate $ vector to_gen

                ptr <- mallocBytes $ internalSizeOf (undefined :: f p)
                (gpeekByteOff' offs ptr 0 :: IO ((:*:) f g p)) `shouldThrow` anyException
                free ptr
                )
    describe "gpokeByteOff' " $ do
        it "instance M1    is equal to: gpokeByteOff' offs ptr off val" $ do
            property (\(GenericType (val :: f p)) -> do
                let size    = internalSizeOf val
                    offsets = internalOffsets val 
                -- Get the offsets to test
                off   <- generate $ suchThat arbitrary (>=0)

                -- Reserve some memory to read from
                ptr    <- mallocBytes (off + size)
                
                -- First test
                gpokeByteOff' offsets ptr off (M1 val)
                bytes1 <- peekArray (off + size) ptr :: IO [Int8]
              
                --Second test
                gpokeByteOff' offsets ptr off val
                bytes2 <- peekArray (off + size) ptr :: IO [Int8]
         
                free ptr
                -- Check:
                bytes1 `shouldBe` bytes2
                )
        it "instance K1    is equal to: internalByteOff ptr (f_off + off) val" $ do
            property (\(GenericType (val :: f p)) -> do
                let size = internalSizeOf val
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
        it "instance (:*:) is equal to: (:*:) <$> poker offs_l a <*> poker offs_r b \n\
            \                                where poker my_offs v  = gpokeByteOff' my_offs ptr off v \n\
            \                                      (offs_l, offs_r) = splitAt (gnumberOf' a) offs" $ do
            property (\(GenericType (val1 :: f p)) (GenericType (val2 :: g p)) -> do
                let offsets = internalOffsets (undefined :: (:*:) f g p)
                    (offs_l, offs_r) = splitAt (gnumberOf' val1) offsets
                    size    = internalSizeOf (undefined :: (:*:) f g p)
                
                -- Get the offset to test
                off   <- generate $ suchThat arbitrary (>=0)

                -- Reserve some memory to read from
                ptr    <- mallocBytes (off + size)
                
                -- First poke
                gpokeByteOff' offsets ptr off (val1 :*: val2)
                bytes1 <- peekArray (off + size) ptr :: IO [Int8]
              
                --Second pokes
                gpokeByteOff' offs_l ptr off val1
                gpokeByteOff' offs_r ptr off val2
                
                bytes2 <- peekArray (off + size) ptr :: IO [Int8]
         
                free ptr
                -- Check:
                bytes1 `shouldBe` bytes2
                )
        it "instance K1    crashes when length of supplied offsets /= 1" $ do 
            property (\(GenericType (val1 :: f p)) -> do
                to_gen <- generate $ suchThat arbitrary (\v -> (v/=1)  && (v>=0) )
                offs   <- generate $ vector to_gen

                ptr <- mallocBytes $ internalSizeOf (undefined :: K1 i (f p) p)
                gpokeByteOff' offs ptr 0 (K1 val1) `shouldThrow` anyException
                free ptr
                )
        it "instance (:*:) crashes when length of supplied offsets /= no. of fields" $ do 
            property (\(GenericType (val1 :: f p)) (GenericType (val2 :: g p)) -> do
                let org_offs = internalOffsets (undefined :: (:*:) f g p)
                    len      = length org_offs
                to_gen <- generate $ suchThat arbitrary (\v -> (v/=len)  && (v>=0) )
                offs   <- generate $ vector to_gen

                ptr <- mallocBytes $ internalSizeOf (undefined :: f p)
                gpokeByteOff' offs ptr 0 (val1 :*: val2) `shouldThrow` anyException
                free ptr
                )
