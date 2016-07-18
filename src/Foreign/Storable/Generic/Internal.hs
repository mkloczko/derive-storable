{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE DefaultSignatures #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE UndecidableInstances #-}

{-#LANGUAGE ScopedTypeVariables #-}

{-#LANGUAGE InstanceSigs #-}
{-#LANGUAGE AllowAmbiguousTypes #-}

{-#LANGUAGE BangPatterns #-}

module Foreign.Storable.Generic.Internal where

import GHC.Generics
import GHC.TypeLits (KnownNat, natVal)
import Data.Proxy (Proxy(..))
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.Types

import Foreign.Storable.Generic.Internal.TypeFuns hiding (getNoFields)
import Foreign.Storable.Generic.Internal.Case

import Data.Int

import Debug.Trace
import Foreign.Storable.Generic.Tools
import GHC.Exts (inline)


import Control.DeepSeq
import System.IO.Unsafe

-- Defining the generics ---

class GStorable' f where
    -- | Read the element at a given offset. Additional information about the offests 
    -- of the subfields are needed.
    gpeekByteOff' :: (GStorable e)
                  => Int      -- ^ List of fields' offsets for the type/struct. 
                  -> e
                  -> Ptr b    -- ^ The pointer to the type/struct.
                  -> Int      -- ^ Global offset.
                  -> IO (f a) -- ^ The result, wrapped in GHC.Generic metadata.
    -- | Write the element at a given offset. Additional information about the offests 
    -- of the subfields are needed.
    gpokeByteOff' :: (GStorable e)
                  => Int    -- ^ List of fields' offsets for the type/struct.
                  -> e
                  -> Ptr b  -- ^ The pointer to the type/struct.
                  -> Int    -- ^ Global offset.
                  -> (f a)  -- ^ The element to write, wrapped in GHC.Generic metadata.
                  -> IO ()

    -- | Calculates the number of type's/struct's fields.
    gnumberOf' :: f a -- ^ GHC.Generic information about a given type/struct.
               -> Int -- ^ Size.

    -- | Calculates the sizes of type's/struct's fields.
    glistSizeOf' :: f a   -- ^ GHC.Generic information about a given type/struct. 
                 -> [Int] -- ^ List of sizes.

    -- | Calculates the alignments of type's/struct's fields.
    glistAlignment' :: f a   -- ^ GHC.Generic information about a given type/struct.
                    -> [Int] -- ^ List of alignments.


instance (GStorable' f) => GStorable' (M1 i t f) where
    -- Wrap the peeked value in metadata.
    -- {-# INLINE gpeekByteOff' #-}
    gpeekByteOff' ix t ptr offset = M1 <$> gpeekByteOff' ix (inline t) ptr offset
    -- Discard the metadata and go further.
    -- {-# INLINE gpokeByteOff' #-}
    gpokeByteOff' ix t ptr offset (M1 x) = gpokeByteOff' ix (inline t) ptr offset x 
    

    gnumberOf' (M1 v) = gnumberOf' v
    glistSizeOf'    _ = glistSizeOf' (undefined :: f p)
    {-#INLINE glistAlignment' #-}
    glistAlignment' _ = glistAlignment' (undefined :: f p)

------------------------------------------
--   The important part of the code!    --
------------------------------------------

instance (KnownNat (NoFields f), KnownNat (NoFields h), GStorable' f, GStorable' h) => GStorable' (f :*: h) where
    -- Tree-like traversal for reading the type.
    -- {-#INLINE gpeekByteOff' #-}
    gpeekByteOff' ix t ptr offset = (:*:) <$> (peeker (ix-n2)) <*>  (peeker ix)
        where n1 = gnumberOf' (undefined :: f a)               -- Number of elements for the left part of the tree.
              n2 = gnumberOf' (undefined :: h a)-- gnumberOf' (undefined :: g a)               -- Number of elements for the right part of the tree
              is_ok = True -- n1+n2 == ix                  -- Check if offset number is the same as the number of subelements.
              error_action = error "Foreign.Storable.Generic.Internal.gpeekByteOff': Mismatch between number of fields and number of offsets"
              --(offs1,offs2) = splitAt n1 offsets               -- Offsets for the left and right part of the tree.
              peeker n_ix = gpeekByteOff' n_ix (inline t) ptr offset      -- gpeekByteOff' wrapped to peek into subtrees.
    -- Tree like traversal for writing the type.
    -- {-#INLINE gpokeByteOff' #-}
    gpokeByteOff' ix t ptr offset (x :*: y) = (peeker (ix-n2) x) >> (peeker ix y)
        where n1 = gnumberOf' (undefined :: f a)               -- Number of elements for the left part of the tree.
              n2 = gnumberOf' y -- gnumberOf' (undefined :: h a)               -- Number of elements for the right part of the tree.
              is_ok = True -- n1+n2 == ix                  -- Check if offset number is the same as the number of subelements.
              error_action = error "Foreign.Storable.Generic.Internal.gpokeByteOff': Mismatch between number of fields and number of offsets"
              --(offs1,offs2) = splitAt n1 offsets               -- Offsets for the left and right part of the tree.
              peeker n_ix z = gpokeByteOff' n_ix (inline t) ptr offset z  -- gpokeByteOff' wrapped to peek into the subtree

    gnumberOf' _ = gnumberOf' (undefined :: f a) + gnumberOf' (undefined :: h a)
    -- Concatenate the lists. 
    glistSizeOf' _ = glistSizeOf' (undefined :: f a) ++ glistSizeOf' (undefined :: h a)
    -- Concatenate the lists.
    {-#INLINE glistAlignment' #-}
    glistAlignment' _ = glistAlignment' (undefined :: f a) ++ glistAlignment' (undefined :: h a)

instance (GStorable a) => GStorable' (K1 i a) where
    -- gpeekByteOff' :: (GStorable' g) => Int -> g q -> Ptr b -> Int -> IO (K1 i a p)
    gpeekByteOff' ix t ptr offset = K1 <$> gpeekByteOff ptr (off1 + offset)
        where off1 = inline $ getOffset t ix 
              debug_info = "ix: " ++ show ix ++ ", off1: " ++ show off1 ++ ", offset: " ++ show offset
              debug      = trace debug_info
    -- gpeekByteOff' offsets ptr offset = error "Foreign.Storable.Generic.Internal.gpeekByteOff': Incorrect number of field offsets for K1"    
    gpokeByteOff' ix t ptr offset (K1 x) = gpokeByteOff ptr (off1 + offset) x
        where off1 = inline $ getOffset t ix 
              debug_info = "ix: " ++ show ix ++ ", off1: " ++ show off1 ++ ", offset: " ++ show offset
              debug      = trace debug_info
    -- gpokeByteOff' offsets ptr offset (K1 x) = error "Foreign.Storable.Generic.Internal.gpokeByteOff': Incorrect number of field offsets for K1"


    -- When we use the contructor, just return one.
    gnumberOf' _ = 1
    -- When the constructor is used, return the size of 
    -- the constructed type in a list.
    glistSizeOf' _ = [gsizeOf (undefined :: a)]
    -- When the constructor is used, return the alignment of 
    -- the constructed type in a list.
    {-#INLINE glistAlignment' #-}
    glistAlignment' _ = [galignment (undefined :: a)]  


-- These functions were moved outside GStorable type class.
-- They take generic representations as input.

-- | Calculates the size of generic data-type.
internalSizeOf :: forall f p. (GStorable' f)
               => f p  -- ^ Generic representation 
               -> Int  -- ^ Resulting size
internalSizeOf _  = calcSize $ (zip sizes aligns) ++ [(0 ,max_align)]
    where sizes  = glistSizeOf'    (undefined :: f p)
          aligns = glistAlignment' (undefined :: f p)
          max_align = calcAlignment aligns

-- | Calculates the alignment of generic data-type.
internalAlignment :: forall f p. (GStorable' f) 
                  => f p -- ^ Generic representation
                  -> Int -- ^ Resulting alignment
internalAlignment  _  = calcAlignment aligns
    where aligns = glistAlignment' (undefined :: f p)

-- | View the variable under a pointer, with offset.
internalPeekByteOff :: forall a b p. (GStorable a, GStorable' (Rep a), KnownNat (NoFields (Rep a))) 
                    => a
                    -> Ptr b    -- ^ Pointer to peek 
                    -> Int      -- ^ Offset 
                    -> IO (Rep a p) -- ^ Resulting generic representation
internalPeekByteOff t ptr off  = gpeekByteOff' (ix-1)  t ptr off
    where offsets = internalOffsets (undefined :: Rep a p)
          ix      = gnumberOf' (undefined :: Rep a p) --gnumberOf' (undefined :: f p)
          is_ok  expr = if ix /= getNoFields (undefined :: a) then error "Nooo!" else expr

-- | Write the variable under the pointer, with offset.
internalPokeByteOff :: forall a b p. (GStorable a, GStorable' (Rep a), KnownNat (NoFields (Rep a))) 
                    => a
                    -> Ptr b -- ^ Pointer to write to
                    -> Int   -- ^ Offset 
                    -> Rep a p   -- ^ Written generic representation 
                    -> IO () 
internalPokeByteOff t ptr off rep = gpokeByteOff' (ix-1) t ptr off rep
    where offsets = internalOffsets (undefined :: Rep a p)
          ix      = gnumberOf' (undefined :: Rep a p) -- gnumberOf' (undefined :: f p)
          is_ok  expr = if ix /= getNoFields (undefined :: a) then error "Nooo!" else expr

-- {-# INLINE internalOffsets #-}
internalOffsets :: forall f p. (GStorable' f)
                => f p
                -> [Int]
internalOffsets _ = aligns -- calcOffsets $ zip sizes aligns ++ [(0 ,max_align)]
    where sizes = glistSizeOf'    (undefined :: f p)
          aligns= glistAlignment' (undefined :: f p)
          max_align = calcAlignment aligns

-- {-#INLINE internalGetOffset #-}
-- internalGetOffset :: forall f p. (GStorable' f, KnownNat (NoFields (f)))
--                   => f p
--                   -> Int
--                   -> Int
-- internalGetOffset _ ix = caseN size offsets ix 
--     -- where size    = (gnumberOf'    (undefined :: f p))
--     where size    = getNoFields (undefined :: f p) -- !?! getNoFields vs gnumberOf' - change in behaviour!11
--           is_ok  expr = if size /= getNoFields (undefined :: f p) then error "Nooo!" else expr
--           offsets = inline $ glistAlignment' (undefined :: f p) -- internalOffsets (undefined :: f p) -- needs this ? for caseN rules to fire?...
--           -- offsets = [3,3,3] -- internalOffsets (undefined :: f p)
--           --       CHange this to 3 - fail!

-- | The class uses the default Generic based implementations to 
-- provide Storable instances for types made from primitive types.
-- Does not work on Algebraic Data Types with more than one constructor.
class GStorable a where
    -- | Calculate the size of the type.
    gsizeOf :: a   -- ^ Element of a given type. Can be undefined.
            -> Int -- ^ Size.
    default gsizeOf :: (Generic a, GStorable' (Rep a))
                    => a -> Int
    gsizeOf _ = internalSizeOf (undefined :: Rep a p) 
    
    -- | Calculate the alignment of the type.
    galignment :: a   -- ^ Element of a given type. Can be undefined  
               -> Int -- ^ Alignment.
    default galignment :: (Generic a, GStorable' (Rep a))
                         => a -> Int
    galignment _ = internalAlignment (undefined :: Rep a p) 

    -- | Read the variable from a given pointer.
    gpeekByteOff :: Ptr b -- ^ Pointer to the variable
                 -> Int   -- ^ Offset
                 -> IO a  -- ^ Returned variable.
    default gpeekByteOff :: (Generic a, GStorable' (Rep a), KnownNat (NoFields (Rep a)))
                         => Ptr b -> Int -> IO a
    {-# INLINE gpeekByteOff #-}
    gpeekByteOff ptr offset = to <$> (internalPeekByteOff (undefined :: a) ptr offset)

-- | Write the variable to a pointer. 
    gpokeByteOff :: Ptr b -- ^ Pointer to the variable. 
                 -> Int   -- ^ Offset.
                 -> a     -- ^ The variable
                 -> IO ()
    default gpokeByteOff :: (Generic a, GStorable' (Rep a), KnownNat (NoFields (Rep a)))
                         => Ptr b -> Int -> a -> IO ()
    --{-# INLINE gpokeByteOff #-}
    gpokeByteOff ptr offset x = internalPokeByteOff x ptr offset (from x)

    {-# INLINE getNoFields #-}
    getNoFields :: a -> Int
    default getNoFields :: (Generic a, GStorable' (Rep a), KnownNat (NoFields (Rep a)) ) => a -> Int
    getNoFields _ = fromIntegral $ inline $ natVal (Proxy :: Proxy (NoFields (Rep a) )) 

    getOffset :: a -> Int -> Int
    default getOffset :: (Generic a, GStorable' (Rep a), KnownNat (NoFields (Rep a)) ) => a -> Int -> Int
    getOffset _ ix = caseN size offsets ix
        where size    = getNoFields (undefined :: a)
              offsets = inline $ internalOffsets (undefined :: Rep a p)
