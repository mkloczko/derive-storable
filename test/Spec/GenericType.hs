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
instance (GStorable' f, GStorable' g) => GStorable ((:*:) f g p) where
    gsizeOf _ = calcSize $ zip sizes alignments
        where sizes      = glistSizeOf' (undefined :: f p) ++ glistSizeOf' (undefined :: g p)
              alignments = glistAlignment' (undefined :: f p) ++ glistAlignment' (undefined :: g p)
    
    galignment _ = g_align 
        where alignments = glistAlignment' (undefined :: f p) ++ glistAlignment' (undefined :: g p)
              g_align    = maximum alignments  

    gpeekByteOff :: Ptr b -> Int -> IO ((:*:) f g p) 
    gpeekByteOff ptr offset = gpeekByteOff' offsets ptr offset
        where sizes      = glistSizeOf'    (undefined :: f p) ++ glistSizeOf' (undefined :: g p)
              alignments = glistAlignment' (undefined :: f p) ++ glistAlignment' (undefined :: g p)
              offsets    = calcOffsets $ zip sizes alignments

    gpokeByteOff ptr offset val = gpokeByteOff' offsets ptr offset val
        where sizes      = glistSizeOf'    (undefined :: f p) ++ glistSizeOf' (undefined :: g p)
              alignments = glistAlignment' (undefined :: f p) ++ glistAlignment' (undefined :: g p)
              offsets    = calcOffsets $ zip sizes alignments



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

instance GStorable BasicType where
   gsizeOf              (BasicType v) = gsizeOf v
   galignment           (BasicType v) = galignment v
   gpeekByteOff ptr off               = gpeekByteOff ptr off
   gpokeByteOff ptr off (BasicType v) = gpokeByteOff ptr off v

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


-- | Constains generic representations of arbitrary data-types.
-- The Show constraint is used so we can print out the badly working cases.
-- The Eq and Arbitrary one are for generating different values for the same types.
data GenericType p where
   GenericType  :: (Eq (f p), Arbitrary (f p), GStorable' f, Show (f p)) => f p -> GenericType p

instance Arbitrary (GenericType p) where
    arbitrary = nestedType 1

instance Show (GenericType p) where
    show (GenericType    val) = show val

instance Wrappable (GenericType p) where
    wrapType    (GenericType  val) = GenericType $ M1 $ M1 $ val

-- | For generating nested types
nestedType :: Int -> Gen (GenericType p)
nestedType n 
    | n <  0    = error "GenericType.nestedType: n is less than 0"
    | n == 0    = wrapType <$> (arbitrary :: Gen (BasicType p))
    | otherwise = wrapType <$> toGenericType <$> (listOf1 $ nestedType (n-1))

-- | For generating nested types with components from levels below.
nestedToType :: Int -> Gen (GenericType p)
nestedToType n = wrapType <$> toGenericType <$> mapM nestedType [0..n] 

-- | Uses the :*: operator to construct a representation of a product type.
typeProduct :: GenericType p -> GenericType p -> GenericType p
typeProduct (GenericType val1) (GenericType val2) = GenericType $ val1 :*: val2

-- | Wraps the ['BasicType'] and ['GenericType'] lists as needed.
toGenericType :: [GenericType] -> GenericType
toGenericType []  = error "toGenericType requires at least one type"
toGenericType [v] = v
toGenericType types = foldl1 typeProduct types

