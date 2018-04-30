{-|
Module      : Foreign.Storable.Generic.Internal
Copyright   : (c) Mateusz Kłoczko, 2016
License     : MIT
Maintainer  : mateusz.p.kloczko@gmail.com
Stability   : experimental
Portability : portable


-}

{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE DefaultSignatures #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE UndecidableInstances #-}

module Foreign.Storable.Generic.Internal (
     GStorable'(..),
     GStorable (..),
     internalSizeOf,
     internalAlignment,
     internalPeekByteOff,
     internalPokeByteOff,
     internalOffsets
  ) where

import GHC.Generics
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.Types

import Data.Int

import Debug.Trace

import Foreign.Storable.Generic.Tools

import GHC.Exts

-- Defining the generics ---

class GStorable' f where
    -- | Read the element at a given offset. Additional information about the offests 
    -- of the subfields are needed.
    gpeekByteOff' :: [Int]    -- ^ List of fields' offsets for the type/struct. 
                  -> Int      -- ^ The index. Used to obtain the correct offset
                  -> Ptr b    -- ^ The pointer to the type/struct.
                  -> Int      -- ^ Global offset.
                  -> IO (f a) -- ^ The result, wrapped in GHC.Generic metadata.
    -- | Write the element at a given offset. Additional information about the offests 
    -- of the subfields are needed.
    gpokeByteOff' :: [Int]  -- ^ List of fields' offsets for the type/struct.
                  -> Int    -- ^ The index. Used to obtain the correct offset.
                  -> Ptr b  -- ^ The pointer to the type/struct.
                  -> Int    -- ^ Global offset.
                  -> (f a)  -- ^ The element to write, wrapped in GHC.Generic metadata.
                  -> IO ()

    -- | Calculates the number of type's/struct's fields.
    gnumberOf' :: f a -- ^ GHC.Generic information about a given type/struct.
               -> Int -- ^ Size.

    -- | Calculates the sizes of type's/struct's fields.
    glistSizeOf' :: f a    -- ^ GHC.Generic information about a given type/struct. 
                 -> [Size] -- ^ List of sizes.

    -- | Calculates the alignments of type's/struct's fields.
    glistAlignment' :: f a         -- ^ GHC.Generic information about a given type/struct.
                    -> [Alignment] -- ^ List of alignments.


instance (GStorable' f) => GStorable' (M1 i t f) where
    -- Wrap the peeked value in metadata.
    {-# INLINE gpeekByteOff' #-}
    gpeekByteOff' offsets ix ptr offset = M1 <$> gpeekByteOff' offsets ix ptr offset
    -- Discard the metadata and go further.
    {-# INLINE gpokeByteOff' #-}
    gpokeByteOff' offsets ix ptr offset (M1 x) = gpokeByteOff' offsets ix ptr offset x 
    

    gnumberOf' (M1 v) = gnumberOf' v
    glistSizeOf' _ = glistSizeOf' (undefined :: f p)
    glistAlignment' _ = glistAlignment' (undefined :: f p)

instance GStorable' U1 where
    -- Wrap the peeked value in metadata.
    {-# INLINE gpeekByteOff' #-}
    gpeekByteOff' offsets ix ptr offset = return U1
    -- Discard the metadata and go further.
    {-# INLINE gpokeByteOff' #-}
    gpokeByteOff' offsets ix ptr offset (U1) = return ()
    

    gnumberOf' (U1)   = 0
    glistSizeOf'    _ = []
    glistAlignment' _ = []

instance (GStorable' f, GStorable' g) => GStorable' (f :*: g) where
    -- Tree-like traversal for reading the type.
    {-# INLINE gpeekByteOff' #-}
    gpeekByteOff' offsets ix ptr offset = (:*:) <$> peeker new_ix <*>  peeker ix
        where new_ix =  ix - n2                                        -- The new index for the left part of the tree.
              n2 = gnumberOf' (undefined :: g a)                       -- Number of elements for the right part of the tree
              peeker n_ix = gpeekByteOff' offsets n_ix ptr offset      -- gpeekByteOff' wrapped to peek into subtrees.
    -- Tree like traversal for writing the type.
    {-# INLINE gpokeByteOff' #-}
    gpokeByteOff' offsets ix ptr offset (x :*: y) = peeker new_ix x >> peeker ix y
        where new_ix = ix - n2                                 
              n2 = gnumberOf' (undefined :: g a)               -- Number of elements for the right part of the tree.
              peeker n_ix z = gpokeByteOff' offsets n_ix ptr offset z  -- gpokeByteOff' wrapped to peek into the subtree




    gnumberOf' _ = gnumberOf' (undefined :: f a) + gnumberOf' (undefined :: g a)
    -- Concatenate the lists. 
    glistSizeOf' _ = glistSizeOf' (undefined :: f a) ++ glistSizeOf' (undefined :: g a)
    -- Concatenate the lists.
    glistAlignment' _ = glistAlignment' (undefined :: f a) ++ glistAlignment' (undefined :: g a)

instance (GStorable a) => GStorable' (K1 i a) where
    {-# INLINE gpeekByteOff' #-}
    gpeekByteOff' offsets ix ptr offset = K1 <$> gpeekByteOff ptr (off1 + offset)
        where off1 = inline (offsets !! ix)
    {-# INLINE gpokeByteOff' #-}
    gpokeByteOff' offsets ix ptr offset (K1 x) = gpokeByteOff ptr (off1 + offset) x
        where off1 = inline (offsets !! ix) 


    -- When we use the contructor, just return one.
    gnumberOf' _ = 1
    -- When the constructor is used, return the size of 
    -- the constructed type in a list.
    glistSizeOf' _ = [gsizeOf (undefined :: a)]
    -- When the constructor is used, return the alignment of 
    -- the constructed type in a list.
    glistAlignment' _ = [galignment (undefined :: a)]  


-- These functions were moved outside GStorable type class.
-- They take generic representations as input.

{-# INLINE internalSizeOf #-}
-- | Calculates the size of generic data-type.
internalSizeOf :: forall f p. (GStorable' f)
               => f p  -- ^ Generic representation 
               -> Int  -- ^ Resulting size
internalSizeOf _  = calcSize $ zip sizes aligns
    where sizes  = glistSizeOf'    (undefined :: f p)
          aligns = glistAlignment' (undefined :: f p)

{-# INLINE internalAlignment #-}
-- | Calculates the alignment of generic data-type.
internalAlignment :: forall f p. (GStorable' f) 
                  => f p       -- ^ Generic representation
                  -> Alignment -- ^ Resulting alignment
internalAlignment  _  = calcAlignment aligns
    where aligns = glistAlignment' (undefined :: f p)

{-# INLINE internalPeekByteOff #-}
-- | View the variable under a pointer, with offset.
internalPeekByteOff :: forall f p b. (GStorable' f) 
                    => Ptr b    -- ^ Pointer to peek 
                    -> Offset   -- ^ Offset 
                    -> IO (f p) -- ^ Resulting generic representation
internalPeekByteOff ptr off  = gpeekByteOff' offsets ix ptr off
    where offsets = internalOffsets (undefined :: f p)
          ix      = gnumberOf' (undefined :: f p) - 1

{-# INLINE internalPokeByteOff #-}
-- | Write the variable under the pointer, with offset.
internalPokeByteOff :: forall f p b. (GStorable' f) 
                    => Ptr b  -- ^ Pointer to write to
                    -> Offset -- ^ Offset 
                    -> f p    -- ^ Written generic representation 
                    -> IO () 
internalPokeByteOff ptr off rep = gpokeByteOff' offsets ix ptr off rep
    where offsets = internalOffsets (undefined :: f p)
          ix      = gnumberOf' (undefined :: f p) - 1

{-# INLINE internalOffsets #-}
-- | Obtain the list of offsets
internalOffsets :: forall f p. (GStorable' f)
                => f p      -- Generic representation
                -> [Offset] -- List of offsets
internalOffsets _ = calcOffsets $ zip sizes aligns
    where sizes = glistSizeOf'    (undefined :: f p)
          aligns= glistAlignment' (undefined :: f p)

-- | The class uses the default Generic based implementations to 
-- provide Storable instances for types made from primitive types.
-- Does not work on Algebraic Data Types with more than one constructor.
class GStorable a where
    -- | Calculate the size of the type.
    {-# INLINE gsizeOf #-}
    gsizeOf :: a   -- ^ Element of a given type. Can be undefined.
            -> Int -- ^ Size.
    default gsizeOf :: (Generic a, GStorable' (Rep a))
                    => a -> Int
    gsizeOf _ = internalSizeOf (undefined :: Rep a p) 
    
    -- | Calculate the alignment of the type.
    {-# INLINE galignment #-}
    galignment :: a   -- ^ Element of a given type. Can be undefined  
               -> Int -- ^ Alignment.
    default galignment :: (Generic a, GStorable' (Rep a))
                         => a -> Int
    galignment _ = internalAlignment (undefined :: Rep a p) 

    -- | Read the variable from a given pointer.
    gpeekByteOff :: Ptr b -- ^ Pointer to the variable
                 -> Int   -- ^ Offset
                 -> IO a  -- ^ Returned variable.
    default gpeekByteOff :: (Generic a, GStorable' (Rep a))
                         => Ptr b -> Int -> IO a
    {-# INLINE gpeekByteOff #-}
    gpeekByteOff ptr offset = to <$> internalPeekByteOff ptr offset

    -- | Write the variable to a pointer. 
    gpokeByteOff :: Ptr b -- ^ Pointer to the variable. 
                 -> Int   -- ^ Offset.
                 -> a     -- ^ The variable
                 -> IO ()
    default gpokeByteOff :: (Generic a, GStorable' (Rep a))
                         => Ptr b -> Int -> a -> IO ()
    {-# INLINE gpokeByteOff #-}
    gpokeByteOff ptr offset x = internalPokeByteOff ptr offset (from x)


