{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE DefaultSignatures #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE UndecidableInstances #-}

module Foreign.Storable.Generic.Internal where

import Generics.Deriving
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import Data.Int

import Debug.Trace

import Foreign.Storable.Generic.Tools

------ List of Sizes -------

class GListSizeOf' f where
    -- | Calculates the sizes of type's/struct's fields.
    glistSizeOf' :: f a   -- ^ GHC.Generic information about a given type/struct. 
                 -> [Int] -- ^ List of sizes.

instance (GListSizeOf' f) => GListSizeOf' ( M1 i t f) where
    glistSizeOf' _ = glistSizeOf' (undefined :: f a)

instance (GListSizeOf' f, GListSizeOf' g) => GListSizeOf' (f :*: g) where
    -- Concatenate the lists. 
    glistSizeOf' _ = glistSizeOf' (undefined :: f a) ++ glistSizeOf' (undefined :: g a)

instance (GStorable a) => GListSizeOf' (K1 i a) where
    -- When the constructor is used, return the size of 
    -- the constructed type in a list.
    glistSizeOf' _ = [gsizeOf (undefined :: a)]

------ Number of elements -------

class GNumberOf' f where
    -- | Calculates the number of type's/struct's fields.
    gnumberOf' :: f a -- ^ GHC.Generic information about a given type/struct.
               -> Int -- ^ Size.

instance (GNumberOf' f) => GNumberOf' ( M1 i t f) where
    gnumberOf' _ = gnumberOf' (undefined :: f a)

instance (GNumberOf' f, GNumberOf' g) => GNumberOf' (f :*: g) where
    gnumberOf' _ = gnumberOf' (undefined :: f a) + gnumberOf' (undefined :: g a)

instance GNumberOf' (K1 i c) where
    -- When we use the contructor, just return one.
    gnumberOf' _ = 1

------ List of alignemnts -------

class GListAlignment' f where
    -- | Calculates the alignments of type'struct's fields.
    glistAlignment' :: f a   -- ^ GHC.Generic information about a given type/struct.
                    -> [Int] -- ^ List of alignments.

instance (GListAlignment' f) => GListAlignment' (M1 i t f) where
    glistAlignment' _ = glistAlignment' (undefined :: f a)

instance (GListAlignment' f, GListAlignment' g) => GListAlignment' (f :*: g) where
    -- Concatenate the lists.
    glistAlignment' _ = glistAlignment' (undefined :: f a) ++ glistAlignment' (undefined :: g a)

instance (GStorable a) => GListAlignment' (K1 i a) where
    -- When the constructor is used, return the alignment of 
    -- the constructed type in a list.
    glistAlignment' _ = [galignment (undefined :: a)]  

------ Peek and Poke --------

class GStorable' f where
    -- | Read the element at a given offset. Additional information about the offests 
    -- of the subfields are needed.
    gpeekByteOff' :: [Int]    -- ^ List of fields' offsets for the type/struct. 
                  -> Ptr a    -- ^ The pointer to the type/struct.
                  -> Int      -- ^ Global offset.
                  -> IO (f a) -- ^ The result, wrapped in GHC.Generic metadata.
    -- | Write the element at a given offset. Additional information about the offests 
    -- of the subfields are needed.
    gpokeByteOff' :: [Int]  -- ^ List of fields' offsets for the type/struct.
                  -> Ptr a  -- ^ The pointer to the type/struct.
                  -> Int    -- ^ Global offset.
                  -> (f a)  -- ^ The element to write, wrapped in GHC.Generic metadata.
                  -> IO ()

instance (GStorable' f) => GStorable' (M1 i t f) where
    -- Wrap the peeked value in metadata.
    gpeekByteOff' offsets ptr offset = M1 <$> gpeekByteOff' offsets ptr offset
    -- Discard the metadata and go further.
    gpokeByteOff' offsets ptr offset (M1 x) = gpokeByteOff' offsets ptr offset x 

------------------------------------------
--   The important part of the code!    --
------------------------------------------

instance (GNumberOf' f, GStorable' f, GNumberOf' g, GStorable' g) => GStorable' (f :*: g) where
    -- Tree-like traversal for reading the type.
    gpeekByteOff' offsets ptr offset = if is_ok then (:*:) <$> peeker offs1 <*>  peeker offs2 else error_action
        where n1 = gnumberOf' (undefined :: f a)               -- Number of elements for the left part of the tree.
              n2 = gnumberOf' (undefined :: g a)               -- Number of elements for the right part of the tree
              is_ok = n1+n2 == length offsets                  -- Check if offset number is the same as the number of subelements.
              error_action = error "Foreign.Storable.Generic.Internal.gpeekByteOff': Mismatch between number of fields and number of offsets"
              (offs1,offs2) = splitAt n1 offsets               -- Offsets for the left and right part of the tree.
              peeker offs = gpeekByteOff' offs ptr offset      -- gpeekByteOff' wrapped to peek into subtrees.
    -- Tree like travelsar for writing the type.
    gpokeByteOff' offsets ptr offset (x :*: y) = if is_ok then peeker offs1 x >> peeker offs2 y else error_action
        where n1 = gnumberOf' (undefined :: f a)               -- Number of elements for the left part of the tree.
              n2 = gnumberOf' (undefined :: g a)               -- Number of elements for the right part of the tree.
              is_ok = n1+n2 == length offsets                  -- Check if offset number is the same as the number of subelements.
              error_action = error "Foreign.Storable.Generic.Internal.gpokeByteOff': Mismatch between number of fields and number of offsets"
              (offs1,offs2) = splitAt n1 offsets               -- Offsets for the left and right part of the tree.
              peeker offs z = gpokeByteOff' offs ptr offset z  -- gpokeByteOff' wrapped to peek into the subtree



instance (GStorable a) => GStorable' (K1 i a) where
    gpeekByteOff' [off1]  ptr offset = K1 <$> gpeekByteOff ptr (off1 + offset) 
    gpeekByteOff' offsets ptr offset = error "Foreign.Storable.Generic.Internal.gpeekByteOff': Incorrect number of field offsets for K1"    
    gpokeByteOff' [off1]  ptr offset (K1 x) = gpokeByteOff ptr (off1 + offset) x
    gpokeByteOff' offsets ptr offset (K1 x) = error "Foreign.Storable.Generic.Internal.gpokeByteOff': Incorrect number of field offsets for K1"


-- | The class uses the default Generic based implementations to 
-- provide Storable instances for types made from primitive types.
-- Does not work on Algebraic Data Types with more than one constructor.
class GStorable a where
    -- | Calculate the size of the type.
    gsizeOf :: a   -- ^ Element of a given type. Can be undefined.
            -> Int -- ^ Size.
    default gsizeOf :: (Generic a, GListSizeOf' (Rep a), GListAlignment' (Rep a))
                    => a -> Int
    gsizeOf _ = calcSize g_align $ zip sizes alignments 
        where sizes      = glistSizeOf'    (from (undefined :: a))  
              alignments = glistAlignment' (from (undefined :: a))
              g_align    = maximum alignments --Using galignemnt here generated bugs.
    -- | Calculate the alignment of the type.
    galignment :: a   -- ^ Element of a given type. Can be undefined  
               -> Int -- ^ Alignment.
    default galignment :: (Generic a, GListAlignment' (Rep a))
                         => a -> Int
    galignment _ = maximum alignments
        where alignments = glistAlignment' (from (undefined :: a))

    -- | Read the variable from a given pointer.
    gpeekByteOff :: Ptr b -- ^ Pointer to the variable
                 -> Int   -- ^ Offset
                 -> IO a  -- ^ Returned variable.
    default gpeekByteOff :: (Generic a, GStorable' (Rep a), GListSizeOf' (Rep a), GListAlignment' (Rep a))
                         => Ptr b -> Int -> IO a
    gpeekByteOff ptr offset = to <$> gpeekByteOff' offsets ptr offset
        where sizes      = glistSizeOf'    (from (undefined :: a))
              alignments = glistAlignment' (from (undefined :: a))
              g_align    = maximum alignments -- Using galigment here generated bugs.
              offsets    = calcOffsets g_align $ zip sizes alignments
    -- | Write the variable to a pointer. 
    gpokeByteOff :: Ptr b -- ^ Pointer to the variable. 
                 -> Int   -- ^ Offset.
                 -> a     -- ^ The variable
                 -> IO ()
    default gpokeByteOff :: (Generic a, GStorable' (Rep a), GListSizeOf' (Rep a), GListAlignment' (Rep a))
                         => Ptr b -> Int -> a -> IO ()
    gpokeByteOff ptr offset x = gpokeByteOff' offsets ptr offset (from x)
        where sizes      = glistSizeOf'    (from (undefined :: a))
              alignments = glistAlignment' (from (undefined :: a))
              g_align    = maximum alignments -- Using galigment here generated bugs.
              offsets    = calcOffsets g_align $ zip sizes alignments
    -- | Get the offsets for the elements in the data type.
    goffsets :: a     -- ^ The data type
             -> [Int] -- ^ The offsets
    default goffsets :: (Generic a, GStorable' (Rep a), GListSizeOf' (Rep a), GListAlignment' (Rep a))
                     => a -> [Int]
    goffsets _ = offsets 		     
        where sizes      = glistSizeOf'    (from (undefined :: a))
              alignments = glistAlignment' (from (undefined :: a))
              g_align    = maximum alignments -- Using galigment here generated bugs.
              offsets    = calcOffsets g_align $ zip sizes alignments

------Association to Storable class-------

instance {-# OVERLAPS #-} (Storable a) => (GStorable a) where
    gsizeOf      = sizeOf
    galignment   = alignment
    gpeekByteOff = peekByteOff
    gpokeByteOff = pokeByteOff
    goffsets   _ = [0] 
instance {-# OVERLAPS #-} (GStorable a) => (Storable a) where
    sizeOf      = gsizeOf
    alignment   = galignment
    peekByteOff = gpeekByteOff
    pokeByteOff = gpokeByteOff


