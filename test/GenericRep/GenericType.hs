{-#LANGUAGE GADTs         #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-} 
{-#LANGUAGE InstanceSigs #-}
{-#LANGUAGE PartialTypeSignatures #-}

{-#LANGUAGE DataKinds #-}
module GenericType (
    BasicType(..),
    GenericType(..),
    NestedType(..),
    NestedToType(..),
    ok_vector
    ) where
-- Test modules
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Modifiers (NonEmptyList(..))
-- Tested modules
import Foreign.Storable.Generic.Internal

-- Test data
import Foreign.Storable.Generic.Tools
import Foreign.Storable.Generic.Instances
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.C.Types
import Foreign.Storable
import GHC.Generics  
import Data.Int
import Data.Word
import Data.Bits
import Data.Proxy
import Debug.Trace
import GHC.TypeLits

import Unsafe.Coerce

-- | TestType - the basic building blocks from which
-- GStorable instances are built.
class (Arbitrary a,Eq a,GStorable a, Show a) => TestType a

-- Generating random pointers.
instance Arbitrary (Ptr a) where
    arbitrary = do
        plus <- choose (0, 10000)
        return $ plusPtr nullPtr plus

instance TestType Int
instance TestType Int8
instance TestType Int16
instance TestType Int32
instance TestType Int64

instance TestType Word
instance TestType Word8
instance TestType Word16
instance TestType Word32
instance TestType Word64

instance TestType Double
instance TestType Float
instance TestType (Ptr a)
instance TestType Char

instance TestType CFloat

instance TestType CDouble

-- | The wrappable type class. Wraps the type in generics
-- and then into GenericType data type.
class (Show a) => Wrappable a where
    wrapType :: a -> GenericType


-------------------
-- | Contains the basic building blocks that generate GStorable type classes.
data BasicType where
   BasicType :: (TestType a) => a -> BasicType

instance Show BasicType where
    show (BasicType val) = show val

instance Arbitrary BasicType where
    arbitrary = do
        valInt     <- arbitrary  :: Gen Int 
        valInt8    <- arbitrary  :: Gen Int8
        valInt16   <- arbitrary  :: Gen Int16 
        valInt32   <- arbitrary  :: Gen Int32 
        valInt64   <- arbitrary  :: Gen Int64
        valWord     <- arbitrary :: Gen Word 
        valWord8    <- arbitrary :: Gen Word8
        valWord16   <- arbitrary :: Gen Word16 
        valWord32   <- arbitrary :: Gen Word32 
        valWord64   <- arbitrary :: Gen Word64
        valDouble  <- arbitrary  :: Gen Double
        valFloat   <- arbitrary  :: Gen Float
        valPtr     <- arbitrary  :: Gen (Ptr a)
        valChar    <- arbitrary  :: Gen Char

        valCDouble  <- arbitrary :: Gen CDouble
        valCFloat   <- arbitrary :: Gen CFloat

        elements [BasicType valInt,    BasicType valInt8, BasicType valInt16
                 ,BasicType valInt32 , BasicType valInt64, BasicType valCDouble
                 ,BasicType valCFloat, BasicType valPtr, BasicType valChar
                 ,BasicType valWord,   BasicType valWord8, BasicType valWord16
                 ,BasicType valWord32, BasicType valWord64]

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


-- | Used withing GenericType to enforce that the types can be joined with :*:.
data MyPhantom

-- | Constains generic representations of arbitrary data-types.
-- The Show constraint is used so we can print out the badly working cases.
-- The Eq and Arbitrary one are for generating different values for the same types.
data GenericType where
   GenericType  :: (p ~ MyPhantom, Eq (f p), Arbitrary (f p), GStorable' f, Show (f p)) => f p -> GenericType

instance Arbitrary GenericType where
    arbitrary = do
        n <- choose (0,100) :: Gen Int
        genType n

-- | Generates a random generic representation.
-- Does not resemble the generic representation from real types.
genType :: Int  -- ^ Number of randomness. 
        -> Gen GenericType
genType 0 = wrapType <$> (arbitrary :: Gen BasicType)
genType n = do
    -- Choose what kind of element this layer is.
    step <- arbitrary :: Gen GenTree
    case step of
        -- A product - a tree
        Producted -> do
            -- Generate two subtrees with the same function
            div <- choose  (0,n) 
            gt1 <- genType  div
            gt2 <- genType (n-div)
            return $ (\(GenericType t1) (GenericType t2) -> GenericType $ t1 :*: t2) gt1 gt2 
        -- Wrap in meta-data anything that's generated later
        M1ed      -> (\(GenericType t) -> GenericType $ M1 t) <$> genType (n-1) 
        -- Wrap in K1 anything that's generated later.
        K1ed      -> (\(GenericType t) -> GenericType $ K1 t) <$> genType (n-1) 

-- | Enums for generating the generic representations.
data GenTree = Producted | M1ed | K1ed

instance Arbitrary GenTree where
    arbitrary = elements [Producted, M1ed , K1ed]

-- | Show the generic metadata. Could use some pretty printing later. 
instance Show GenericType where
    show (GenericType    val) = show val

-- | Wrap the generic representation as it were derived from a proper type.
instance Wrappable GenericType where
    wrapType    (GenericType  val) = GenericType $ M1 $ K1 $ val

-- | Wrapper for creating nested types.
data NestedType (n :: Nat) = NestedType GenericType

instance (KnownNat n) => Show (NestedType n) where
    show (NestedType (GenericType val)) = type_info ++ show val
        where type_info = "NestedType " ++ (show $ natVal (Proxy :: Proxy n)) ++ " "

instance (KnownNat n) => Arbitrary (NestedType n) where
    arbitrary = NestedType <$> nestedType (fromIntegral $ natVal (Proxy :: Proxy n))

-- | Wrapper for creating nested types up to a certain level.
data NestedToType (n :: Nat) = NestedToType GenericType

instance (KnownNat n) => Show (NestedToType n) where
    show (NestedToType (GenericType val)) = type_info ++ show val
        where type_info = "NestedToType " ++ (show $ natVal (Proxy :: Proxy n)) ++ " "

instance (KnownNat n) => Arbitrary (NestedToType n) where
    arbitrary = NestedToType <$> nestedToType (fromIntegral $ natVal (Proxy :: Proxy n))

-- | Generate a nested type.
nestedType :: Int             -- ^ Depth of the generated type. 
           -> Gen GenericType -- ^ Resulting generator.
nestedType n  = nestedType' n gen
    where gen = wrapType <$> (arbitrary :: Gen BasicType) 

nestedType' :: Int             -- ^ Depth of the generated type.
            -> Gen GenericType -- ^ Accumulator
            -> Gen GenericType -- ^ Resulting generator
nestedType' n gen 
    | n <  0 = error "GenericType.nestedType': n is less than 0"
    | n == 0 = gen
    | n > 0  = do 
        fields <- choose (1, 4*n)
        nestedType' (n-1) (wrapType <$> toGenericType <$> vectorOf fields gen) 

-- | For generating nested types with components from levels below.
nestedToType :: Int -> Gen (GenericType)
nestedToType n =do
    sublist <- suchThat (sublistOf [0..n]) (\x -> length x > 0)
    wrapType <$> toGenericType <$> mapM nestedType sublist

-- | Uses the :*: operator to construct a representation of a product type.
typeProduct :: GenericType -> GenericType -> GenericType
typeProduct (GenericType val1) (GenericType val2) = GenericType $ val1 :*: val2

-- | Creates a tree for type product.
toGenericType :: [GenericType] -> GenericType
toGenericType []  = error "toGenericType requires at least one type"
toGenericType [v] = v
toGenericType types = foldl1 typeProduct types

-- | Simulates the K1 step for generic representations. 
instance {-#OVERLAPS#-} (GStorable' f) => GStorable' (K1 i (f p)) where
    glistSizeOf'    _ = [internalSizeOf (undefined :: f p)]
    glistAlignment' _ = [internalAlignment (undefined :: f p)]
    gpeekByteOff' offs n ptr off   = K1 <$> internalPeekByteOff ptr (off + f_off)
        where f_off = offs !! n
    gpokeByteOff' offs n ptr off (K1 v) = internalPokeByteOff ptr (off + f_off) v
        where f_off = offs !! n
    gnumberOf'      _  = 1

-- | Helps with avoiding NaN problem. 
ok_vector :: Int -> Gen [Word8]
ok_vector n = vectorOf n (suchThat arbitrary $ (\x-> (x .&. 127) /= 127) )

