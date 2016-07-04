{-#LANGUAGE GADTs         #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-} 
{-#LANGUAGE InstanceSigs #-}
{-#LANGUAGE PartialTypeSignatures #-}

{-#LANGUAGE DataKinds #-}
module GenericType where
-- Test modules
import Test.QuickCheck
import Test.QuickCheck.Modifiers (NonEmptyList(..))
-- Tested modules
import Foreign.Storable.Generic.Internal

-- Test data
import Foreign.Storable.Generic.Tools
import Foreign.Storable.Generic.Instances
import Foreign.Ptr (Ptr)
import Foreign.Storable
import GHC.Generics  
import Data.Int
import Data.Proxy
import Debug.Trace
import GHC.TypeLits

import Unsafe.Coerce
-- | TestType - the basic building blocks from which
-- GStorable instances are built.
class (Arbitrary a,Eq a,GStorable a, Show a) => TestType a

instance TestType Int
instance TestType Char


-- | The wrappable type class. Wraps the type in generics.
class (Show a) => Wrappable a where
    wrapType :: a -> GenericType


-- This actually reimplements the GStorable behaviour.
-- instance (GStorable' f, GStorable' g) => GStorable ((:*:) f g p) where
--     gsizeOf _ = trace "prod" $ calcSize $ zip sizes alignments
--         where sizes      = glistSizeOf' (undefined :: f p) ++ glistSizeOf' (undefined :: g p)
--               alignments = glistAlignment' (undefined :: f p) ++ glistAlignment' (undefined :: g p)
--     
--     galignment _ = g_align 
--         where alignments = glistAlignment' (undefined :: f p) ++ glistAlignment' (undefined :: g p)
--               g_align    = maximum alignments  
-- 
--     gpeekByteOff :: Ptr b -> Int -> IO ((:*:) f g p) 
--     gpeekByteOff ptr offset = gpeekByteOff' offsets ptr offset
--         where sizes      = glistSizeOf'    (undefined :: f p) ++ glistSizeOf' (undefined :: g p)
--               alignments = glistAlignment' (undefined :: f p) ++ glistAlignment' (undefined :: g p)
--               offsets    = calcOffsets $ zip sizes alignments
-- 
--     gpokeByteOff ptr offset val = gpokeByteOff' offsets ptr offset val
--         where sizes      = glistSizeOf'    (undefined :: f p) ++ glistSizeOf' (undefined :: g p)
--               alignments = glistAlignment' (undefined :: f p) ++ glistAlignment' (undefined :: g p)
--               offsets    = calcOffsets $ zip sizes alignments



-------------------
-------------------
-- | Contains the basic building blocks that generate GStorable type classes.
data BasicType where
   BasicType :: (TestType a) => a -> BasicType

instance Show BasicType where
    show (BasicType val) = show val

instance Arbitrary BasicType where
    arbitrary = do
        valInt  <- choose (minBound :: Int, maxBound :: Int)
        valChar <- choose (minBound :: Char, maxBound :: Char)
        elements [BasicType valInt, BasicType valChar]

-- instance GStorable BasicType where
--    gsizeOf              (BasicType (_ :: a)) = gsizeOf (undefined :: a)
--    galignment           (BasicType (_ :: a)) = galignment (undefined :: a)
--    gpeekByteOff ptr off               = gpeekByteOff ptr off
--    gpokeByteOff ptr off (BasicType v) = gpokeByteOff ptr off v

-- | Wraps the basic type with 'M1' and 'K1' type constructors. 
-- The result is usable by the testing algorithms.
instance Wrappable BasicType where
    wrapType (BasicType val) = GenericType $ M1 $ K1 val

-- Some tricks for generics:
instance Arbitrary c     => Arbitrary (K1 i c p) where
    arbitrary = K1 <$> arbitrary
instance Arbitrary (f p) => Arbitrary (M1 i c f p) where
    arbitrary = M1 <$> arbitrary
instance (Arbitrary (f p), Arbitrary (g p)) => Arbitrary ((:*:) f g p) where
    arbitrary = (:*:) <$> (arbitrary :: Gen (f p)) <*> (arbitrary :: Gen (g p))


data MyPhantom

-- | Constains generic representations of arbitrary data-types.
-- The Show constraint is used so we can print out the badly working cases.
-- The Eq and Arbitrary one are for generating different values for the same types.
data GenericType where
   GenericType  :: (p ~ MyPhantom, Eq (f p), Arbitrary (f p), GStorable' f, Show (f p)) => f p -> GenericType

testSizeOf :: forall f p. (GStorable' f) => f p -> Int
testSizeOf _  = calcSize $ zip sizes aligns
    where sizes  = glistSizeOf'    (undefined :: f p)
          aligns = glistAlignment' (undefined :: f p)

testAlignment :: forall f p. (GStorable' f) => f p -> Int
testAlignment  _  = maximum aligns
    where aligns = glistAlignment' (undefined :: f p)

testPeekByteOff :: forall f p b. (GStorable' f) => Ptr b -> Int -> IO (f p)
testPeekByteOff ptr off  = gpeekByteOff' offsets ptr off
    where sizes   = glistSizeOf'    (undefined :: f p)
          aligns  = glistAlignment' (undefined :: f p)
          offsets = calcOffsets $ zip sizes aligns

testPokeByteOff :: forall f p b. (GStorable' f) => Ptr b -> Int -> f p -> IO ()
testPokeByteOff ptr off rep = gpokeByteOff' offsets ptr off rep
    where sizes   = glistSizeOf'    (undefined :: f p)
          aligns  = glistAlignment' (undefined :: f p)
          offsets = calcOffsets $ zip sizes aligns

instance {-#OVERLAPS#-} (GStorable' f) => GStorable' (K1 i (f p)) where
    glistSizeOf'    _ = [testSizeOf (undefined :: f p)]
    glistAlignment' _ = [testAlignment (undefined :: f p)]
    gpeekByteOff' [f_off] ptr off   = K1 <$> testPeekByteOff ptr (off + f_off)
    gpeekByteOff' offs ptr off   = error "Mismatch between number of offsets and fields"
    gpokeByteOff' [f_off] ptr off (K1 v) = testPokeByteOff ptr (off + f_off) v
    gpokeByteOff' offs ptr off v  = error "Mismatch between number of offsets and fields"
    gnumberOf'      _  = 1
-- instance (GStorable' f) => GStorable (M1 i c f p) where
--     gsizeOf _ = trace "m1" $ calcSize $ zip sizes alignments
--         where sizes      = glistSizeOf' (undefined :: f p)
--               alignments = glistAlignment' (undefined :: f p)
--     
--     galignment _ = g_align 
--         where alignments = glistAlignment' (undefined :: f p)
--               g_align    = maximum alignments  
-- 
--     gpeekByteOff ptr offset = gpeekByteOff' offsets ptr offset
--         where sizes      = glistSizeOf'    (undefined :: f p) 
--               alignments = glistAlignment' (undefined :: f p) 
--               offsets    = calcOffsets $ zip sizes alignments
-- 
--     gpokeByteOff ptr offset val = gpokeByteOff' offsets ptr offset val
--         where sizes      = glistSizeOf'    (undefined :: f p) 
--               alignments = glistAlignment' (undefined :: f p) 
--               offsets    = calcOffsets $ zip sizes alignments

-- instance (GStorable (f p)) => GStorable (M1 i c f p) where
--    gsizeOf  = gsizeOf 
--    galignment = galignment
--    gpeekByteOff = gpeekByteOff
--    gpokeByteOff = gpokeByteOff
-- 
-- 
-- instance (GStorable' f, GStorable (f p)) => GStorable (K1 i (f p) p) where
--    gsizeOf  = gsizeOf 
--    galignment = galignment
--    gpeekByteOff = gpeekByteOff
--    gpokeByteOff = gpokeByteOff

-- instance GStorable GenericType where
--    gsizeOf              (GenericType v) = gsizeOf v
--    galignment           (GenericType v) = galignment v
--    gpeekByteOff ptr off                 = gpeekByteOff ptr off
--    gpokeByteOff ptr off (GenericType v) = gpokeByteOff ptr off v

instance Arbitrary GenericType where
    arbitrary = nestedType 1

instance Show GenericType where
    show (GenericType    val) = show val

instance Wrappable GenericType where
    wrapType    (GenericType  val) = GenericType $ M1 $ M1 $ val

data NestedType (n :: Nat) = NestedType GenericType

instance (KnownNat n) => Show (NestedType n) where
    show (NestedType (GenericType val)) = type_info ++ show val
        where type_info = "NestedType " ++ (show $ natVal (Proxy :: Proxy n))

instance (KnownNat n) => Arbitrary (NestedType n) where
    arbitrary = NestedType <$> nestedType (fromIntegral $ natVal (Proxy :: Proxy n))

data NestedToType (n :: Nat) = NestedToType GenericType

instance (KnownNat n) => Show (NestedToType n) where
    show (NestedToType (GenericType val)) = type_info ++ show val
        where type_info = "NestedType " ++ (show $ natVal (Proxy :: Proxy n))

instance (KnownNat n) => Arbitrary (NestedToType n) where
    arbitrary = NestedToType <$> nestedToType (fromIntegral $ natVal (Proxy :: Proxy n))

-- | For generating nested types
nestedType :: Int -> Gen (GenericType)
nestedType n 
    | n <  0    = error "GenericType.nestedType: n is less than 0"
    | n == 0    = wrapType <$> (arbitrary :: Gen BasicType)
    | otherwise = wrapType <$> toGenericType <$> (listOf1 $ nestedType (n-1))

-- | For generating nested types with components from levels below.
nestedToType :: Int -> Gen (GenericType)
nestedToType n = wrapType <$> toGenericType <$> mapM nestedType [0..n] 

-- | Uses the :*: operator to construct a representation of a product type.
typeProduct :: GenericType -> GenericType -> GenericType
typeProduct (GenericType val1) (GenericType val2) = GenericType $ val1 :*: val2

-- | Wraps the ['BasicType'] and ['GenericType'] lists as needed.
toGenericType :: [GenericType] -> GenericType
toGenericType []  = error "toGenericType requires at least one type"
toGenericType [v] = v
toGenericType types = foldl1 typeProduct types

