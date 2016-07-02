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


-- | Temporary inside wrappers
-- | The wrappable type class. Replicates the Generic
-- behaviour.
class (Show a) => Wrappable a where
    wrapType :: a -> GenericType


-- Now this is funny... I'm basicly reimplementing the GStorable behaviour.
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
    wrapType (BasicType val) = Part $ M1 $ K1 val

data MyPhantom
-- Some tricks for generics:
instance Arbitrary c     => Arbitrary (K1 i c p) where
    arbitrary = K1 <$> arbitrary
instance Arbitrary (f p) => Arbitrary (M1 i c f p) where
    arbitrary = M1 <$> arbitrary
instance (Arbitrary (f p), Arbitrary (g p)) => Arbitrary ((:*:) f g p) where
    arbitrary = (:*:) <$> (arbitrary :: Gen (f p)) <*> (arbitrary :: Gen (g p))


-- | Constains generic representations of arbitrary data-types.
-- The Show constraint is used so we can print out the badly working cases.
-- The last type variable of K1, M1 and :*: had to be constant so that :*: operator will work.
data GenericType where
   -- | Represents parts of a type.
   Part    :: (Eq (f MyPhantom), Arbitrary (f MyPhantom), GStorable' f, Show (f MyPhantom)) => M1 i c f MyPhantom -> GenericType
   -- | Represents product of types. 
   Product :: (GStorable' f, GStorable' g, Show ((:*:) f g MyPhantom),Arbitrary ((:*:) f g MyPhantom), Eq ((:*:) f g MyPhantom)  ) => (:*:) f g MyPhantom -> GenericType
   -- | For expressions that were constructed using 'M1'.
   Full    :: (Eq (f MyPhantom), Arbitrary (f MyPhantom), GStorable' f, Show (f MyPhantom)) => M1 i c f MyPhantom -> GenericType

instance Arbitrary GenericType where
    arbitrary = do
       non_empty <- arbitrary :: (Gen (NonEmptyList BasicType)) 
       return $ wrapType $ toGenericType $ getNonEmpty non_empty

instance Show GenericType where
    show (Full    val) = show val
    show (Part    val) = show val
    show (Product val) = show val

-- | Wraps the generic type with 'M1' and 'K1' type constructors
instance Wrappable GenericType where
    -- | Full gets lift to Part.
    wrapType  f@(Full    val) = f
    -- | Part stays the same.
    wrapType    (Part    val) = Full $ M1 $ M1 $ val
    -- | Products obtained throught the :*: type constructor are
    -- wrapped using 'K1' and 'M1'
    wrapType    (Product val) = Part $ M1 $ K1 val

data NestedType (from :: Nat) (to :: Nat) = NestedType GenericType

-- | Uses the :*: operator to construct a representation of a product type.
typeProduct :: GenericType -> GenericType -> GenericType
typeProduct (Part    val1) (Part    val2) = Product $ val1 :*: val2
typeProduct (Product val1) (Product val2) = Product $ val1 :*: val2
typeProduct (Product val1) (Part    val2) = Product $ val1 :*: val2
typeProduct (Part    val1) (Product val2) = Product $ val1 :*: val2 
typeProduct (Full    val1) (Full    val2) = Product $ val1 :*: val2
typeProduct (Full    val1) (Product val2) = Product $ val1 :*: val2
typeProduct (Product val1) (Full    val2) = Product $ val1 :*: val2
typeProduct (Full    val1) (Part    val2) = Product $ val1 :*: val2
typeProduct (Part    val1) (Full    val2) = Product $ val1 :*: val2
-- typeProduc  (Final   val1) (Final   val2) = Product $ val1 :*: val2
-- | Primitive test case.
testTypes :: [BasicType]
testTypes = [BasicType (3 :: Int), BasicType (14 :: Int), BasicType ('a' :: Char), BasicType (5 :: Int), BasicType ('b' :: Char)] 

-- | Wraps the ['BasicType'] and ['GenericType'] lists as needed.
toGenericType :: (Wrappable a) => [a] -> GenericType
toGenericType []  = error "toGenericType requires at least one type"
toGenericType [v] = wrapType v
toGenericType types = foldl1 typeProduct $ map wrapType types

-- | Obtains the list of sizes from the generic representation
listOfSizes :: GenericType -> [Int]
listOfSizes (Full   m1) = glistSizeOf' m1
listOfSizes (Part   m1) = glistSizeOf' m1
listOfSizes (Product p) = glistSizeOf' p

-- | Obtains the list of alignments from the generic representation.
listOfAlignments :: GenericType -> [Int]
listOfAlignments (Full   m1) = glistAlignment' m1
listOfAlignments (Part   m1) = glistAlignment' m1
listOfAlignments (Product p) = glistAlignment' p

listOffsets :: GenericType -> [Int]
listOffsets gen_type  = calcOffsets $ zip sizes alignments 
    where sizes       = listOfSizes gen_type
          alignments  = listOfAlignments gen_type 

getSize :: GenericType -> Int
getSize gen_type = calcSize $ zip sizes alignments
   where sizes      = listOfSizes gen_type
         alignments = listOfAlignments gen_type
        
testPeekByteOff :: GenericType -> [Int] -> Ptr b -> Int -> IO GenericType
testPeekByteOff gen_type offsets ptr offset = case gen_type of
        Part    (v :: M1    i c f  MyPhantom) -> Part    <$> (gpeekByteOff' offsets ptr offset :: IO (M1    i c f MyPhantom)) 
        Full    (v :: M1    i c f  MyPhantom) -> Full    <$> (gpeekByteOff' offsets ptr offset :: IO (M1    i c f MyPhantom)) 
        Product (v :: (:*:) f g    MyPhantom) -> Product <$> (gpeekByteOff' offsets ptr offset :: IO ((:*:) f g   MyPhantom))

testPeekByteOffEqual :: GenericType -> [Int] -> Ptr b -> Int -> IO Bool
testPeekByteOffEqual gen_type offsets ptr offset = case gen_type of
        Part    (v :: M1    i c f  MyPhantom) -> (v==) <$> (gpeekByteOff' offsets ptr offset :: IO (M1    i c f MyPhantom)) 
        Full    (v :: M1    i c f  MyPhantom) -> (v==) <$> (gpeekByteOff' offsets ptr offset :: IO (M1    i c f MyPhantom)) 
        Product (v :: (:*:) f g    MyPhantom) -> (v==) <$> (gpeekByteOff' offsets ptr offset :: IO ((:*:) f g   MyPhantom))

testPokeByteOff :: [Int] -> Ptr b -> Int -> GenericType -> IO () 
testPokeByteOff offsets ptr offset (Part    val) = gpokeByteOff' offsets ptr offset val
testPokeByteOff offsets ptr offset (Full    val) = gpokeByteOff' offsets ptr offset val
testPokeByteOff offsets ptr offset (Product val) = gpokeByteOff' offsets ptr offset val

