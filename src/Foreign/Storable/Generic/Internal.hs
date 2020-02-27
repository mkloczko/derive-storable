{-|
Module      : Foreign.Storable.Generic.Internal
Copyright   : (c) Mateusz Kłoczko, 2016
License     : MIT
Maintainer  : mateusz.p.kloczko@gmail.com
Stability   : experimental
Portability : portable


-}

{-#LANGUAGE FlexibleInstances    #-}
{-#LANGUAGE FlexibleContexts     #-}
{-#LANGUAGE DefaultSignatures    #-}
{-#LANGUAGE TypeOperators        #-}
{-#LANGUAGE ScopedTypeVariables  #-}
{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE DataKinds            #-}

{-#LANGUAGE TypeApplications     #-}
{-#LANGUAGE TypeFamilies         #-}
{-#LANGUAGE KindSignatures       #-}
{-#LANGUAGE MultiParamTypeClasses#-}
{-#LANGUAGE AllowAmbiguousTypes  #-}
{-#LANGUAGE ConstraintKinds      #-}

module Foreign.Storable.Generic.Internal (
     GStorable'(..),
     GStorable (..),
--      GStorableSum (..),
     internalSizeOf,
     internalAlignment,
     internalPeekByteOff,
     internalPokeByteOff,
     internalOffsets
  ) where

import GHC.TypeLits
import GHC.TypeNats
import GHC.Generics
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.Types

import Data.Word
import Data.Int

import Debug.Trace

import Foreign.Storable.Generic.Tools
import Foreign.Storable.Generic.Tools.TypeFuns

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
    
    glistSizeOf' _ = glistSizeOf' (undefined :: f p)
    glistAlignment' _ = glistAlignment' (undefined :: f p)

instance GStorable' U1 where
    -- Wrap the peeked value in metadata.
    {-# INLINE gpeekByteOff' #-}
    gpeekByteOff' offsets ix ptr offset = return U1
    -- Discard the metadata and go further.
    {-# INLINE gpokeByteOff' #-}
    gpokeByteOff' offsets ix ptr offset (U1) = return ()
    
    glistSizeOf'    _ = []
    glistAlignment' _ = []

instance (KnownNat (NoFields f), KnownNat (NoFields g)
         , GStorable' f, GStorable' g) => GStorable' (f :*: g) where
    -- Tree-like traversal for reading the type.
    {-# INLINE gpeekByteOff' #-}
    gpeekByteOff' offsets ix ptr offset = (:*:) <$> peeker1 new_ix <*>  peeker2 ix
        where new_ix =  ix - n2                                        -- The new index for the left part of the tree.
              n2 = noFields (undefined :: g a)                       -- Number of elements for the right part of the tree
              peeker1 n_ix = gpeekByteOff' offsets n_ix ptr offset      -- gpeekByteOff' wrapped to peek into subtrees.
              peeker2 n_ix = gpeekByteOff' offsets n_ix ptr offset      -- gpeekByteOff' wrapped to peek into subtrees.
    -- Tree like traversal for writing the type.
    {-# INLINE gpokeByteOff' #-}
    gpokeByteOff' offsets ix ptr offset (x :*: y) = peeker1 new_ix x >> peeker2 ix y
        where new_ix = ix - n2                                 
              n2 = noFields (undefined :: g a)               -- Number of elements for the right part of the tree.
              peeker1 n_ix z = gpokeByteOff' offsets n_ix ptr offset z  -- gpokeByteOff' wrapped to peek into the subtree
              peeker2 n_ix z = gpokeByteOff' offsets n_ix ptr offset z  -- gpokeByteOff' wrapped to peek into the subtree



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
internalPeekByteOff :: forall f p b. (KnownNat (NoFields f), GStorable' f) 
                    => Ptr b    -- ^ Pointer to peek 
                    -> Offset   -- ^ Offset 
                    -> IO (f p) -- ^ Resulting generic representation
internalPeekByteOff ptr off  = gpeekByteOff' offsets ix ptr off
    where offsets = internalOffsets (undefined :: f p)
          ix      = noFields (undefined :: f p) - 1

{-# INLINE internalPokeByteOff #-}
-- | Write the variable under the pointer, with offset.
internalPokeByteOff :: forall f p b. (KnownNat (NoFields f), GStorable' f) 
                    => Ptr b  -- ^ Pointer to write to
                    -> Offset -- ^ Offset 
                    -> f p    -- ^ Written generic representation 
                    -> IO () 
internalPokeByteOff ptr off rep = gpokeByteOff' offsets ix ptr off rep
    where offsets = internalOffsets (undefined :: f p)
          ix      = noFields (undefined :: f p) - 1

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
    default gsizeOf :: (ConstraintsSize a, GStorableChoice a)
                    => a -> Int
    -- gsizeOf _ = internalSizeOf (undefined :: Rep a p) 
    gsizeOf _ = chSizeOf @(IsSumType (Rep a)) (undefined :: a) -- internalSizeOf (undefined :: Rep a p) 
    
    -- | Calculate the alignment of the type.
--     {-# INLINE galignment #-}
    galignment :: a   -- ^ Element of a given type. Can be undefined  
               -> Int -- ^ Alignment.
    default galignment :: (ConstraintsAlignment a, GStorableChoice a)
                         => a -> Int
    galignment _ = chAlignment @(IsSumType (Rep a) ) (undefined :: a) 
--     default galignment :: (Generic a, GStorable' (Rep a))
--                          => a -> Int
--     galignment _ = internalAlignment (undefined :: Rep a p) 

    -- | Read the variable from a given pointer.
    gpeekByteOff :: Ptr b -- ^ Pointer to the variable
                 -> Int   -- ^ Offset
                 -> IO a  -- ^ Returned variable.
    default gpeekByteOff :: (GStorableChoice a, ConstraintsPeek a)
                         => Ptr b -> Int -> IO a
    gpeekByteOff = chPeekByteOff @(IsSumType (Rep a))
--     default gpeekByteOff :: ( KnownNat (NoFields (Rep a))
--                             , Generic a, GStorable' (Rep a))
--                          => Ptr b -> Int -> IO a
--     {-# INLINE gpeekByteOff #-}
--     gpeekByteOff ptr offset = to <$> internalPeekByteOff ptr offset

    -- | Write the variable to a pointer. 
    gpokeByteOff :: Ptr b -- ^ Pointer to the variable. 
                 -> Int   -- ^ Offset.
                 -> a     -- ^ The variable
                 -> IO ()
    default gpokeByteOff :: (GStorableChoice a, ConstraintsPoke a)
                         => Ptr b -> Int -> a -> IO ()
    {-# INLINE gpokeByteOff #-}
    gpokeByteOff = chPokeByteOff @(IsSumType (Rep a))
--     default gpokeByteOff :: ( KnownNat (NoFields (Rep a))
--                             , Generic a, GStorable' (Rep a))
--                          => Ptr b -> Int -> a -> IO ()
--     {-# INLINE gpokeByteOff #-}
--     gpokeByteOff ptr offset x = internalPokeByteOff ptr offset (from x)


type GStorableChoice a = GStorableChoice' (IsSumType (Rep a)) a

class GStorableChoice' (choice :: Bool) a where
    chSizeOf      :: a     -> Int
    chAlignment   :: a     -> Int
    chPeekByteOff :: Ptr b -> Int -> IO a
    chPokeByteOff :: Ptr b -> Int ->    a -> IO ()

instance ( Generic a, KnownNat (SumArity (Rep a))
         , GStorableSum' (Rep a), IsSumType (Rep a) ~ True) => GStorableChoice' True a where
    chSizeOf    _ = gsizeOfSum' (undefined :: Rep a p)
    chAlignment _ = alignOfSum' (undefined :: Rep a p)
    chPeekByteOff ptr off = do
        choice <- peekByteOff ptr off :: IO Word8
        to <$> gpeekByteOffSum' (fromIntegral choice) ptr (off + chAlignment @True (undefined :: a))
    chPokeByteOff ptr off v = do
        pokeByteOff ptr off (internalTagValue v - 1)
        gpokeByteOffSum' ptr (off + chAlignment @True v) (from v)
    
instance (ConstraintsAll a, IsSumType (Rep a) ~ False) => GStorableChoice' False a where
    chSizeOf    _ = internalSizeOf    (undefined :: Rep a p)
    chAlignment _ = internalAlignment (undefined :: Rep a p)
    chPeekByteOff ptr offset = to <$> internalPeekByteOff ptr offset
    chPokeByteOff ptr offset x = internalPokeByteOff ptr offset (from x)


type ConstraintsAll       a = (ConstraintsSize a, ConstraintsPeek a)
type ConstraintsAlignment a = ConstraintsSA' (IsSumType (Rep a)) a 
type ConstraintsSize      a = ConstraintsSA' (IsSumType (Rep a)) a 
type ConstraintsPeek      a = ConstraintsP'  (IsSumType (Rep a)) a
type ConstraintsPoke      a = ConstraintsP'  (IsSumType (Rep a)) a


type family ConstraintsSA' (t :: Bool) a where
    ConstraintsSA' True  a = (Generic a, GStorableSum' (Rep a))
    ConstraintsSA' False a = (Generic a, GStorable'    (Rep a))

type family ConstraintsP' (t :: Bool) a where
    ConstraintsP' True   a = ( Generic a, GStorableSum' (Rep a))
    ConstraintsP' False  a = ( KnownNat (NoFields (Rep a)), Generic a, GStorable' (Rep a))


class GStorableSum' f where
    seeFirstByte'    :: f p -> Int -> Word8
    gsizeOfSum'      :: f p -> Int
    alignOfSum'      :: f p -> Int
    gpeekByteOffSum' :: Int -> Ptr b -> Int -> IO (f p)
    gpokeByteOffSum' ::        Ptr b -> Int -> f p -> IO ()

instance (GStorableSum' f) => GStorableSum' (M1 S t f) where
    seeFirstByte'    (M1 v) acc = seeFirstByte' v acc
    gsizeOfSum'      (M1 v)     = error "Shouldn't be here"
    alignOfSum'      (M1 v)     = error "Shouldn't be here"
    gpeekByteOffSum' _ _ _      = error "Shouldn't be here"
    gpokeByteOffSum' _ _ _      = error "Shouldn't be here"

instance (GStorableSum' f) => GStorableSum' (M1 D t f) where
    seeFirstByte'    (M1 v) acc = seeFirstByte' v acc
    gsizeOfSum'      (M1 v)     = gsizeOfSum' v
    alignOfSum'      (M1 v)     = alignOfSum' v
    gpeekByteOffSum' ch ptr off        = M1 <$> gpeekByteOffSum' ch ptr off
    gpokeByteOffSum'    ptr off (M1 v) =        gpokeByteOffSum'    ptr off v

instance (KnownNat (NoFields f), GStorable' f, GStorableSum' f) => GStorableSum' (M1 C t f) where
    seeFirstByte' (M1 v) acc = fromIntegral acc
    gsizeOfSum'   (M1 v)     = calcSize $ zip sizes aligns
        where sizes  = (word8s:) $ glistSizeOf'    (undefined :: f p)
              aligns = (word8a:) $ glistAlignment' (undefined :: f p)
              word8s = sizeOf    (undefined :: Word8)
              word8a = alignment (undefined :: Word8)
    alignOfSum'   _  = calcAlignment aligns
        where aligns = (word8a:) $ glistAlignment' (undefined :: f p)
              word8a = alignment (undefined :: Word8)
    gpeekByteOffSum' _ ptr off   = M1 <$> internalPeekByteOff ptr off
    gpokeByteOffSum'   ptr off v = internalPokeByteOff ptr off v

instance ( KnownNat (SumArity g), KnownNat (SumArity f)
         , GStorableSum' f, GStorableSum' g) => GStorableSum' (f :+: g) where
    seeFirstByte' (L1 l) acc = seeFirstByte' l $ acc - (sumArity (undefined :: g p))
    seeFirstByte' (R1 r) acc = seeFirstByte' r   acc
    gsizeOfSum'   _ = max (gsizeOfSum' (undefined :: f p)) (gsizeOfSum' (undefined :: g p))
    alignOfSum'   _ = max (alignOfSum' (undefined :: f p)) (alignOfSum' (undefined :: g p))
    gpeekByteOffSum' choice ptr off = if arityL > choice
            then L1 <$> gpeekByteOffSum'  choice           ptr off
            else R1 <$> gpeekByteOffSum' (choice - arityL) ptr off
        where arityL = sumArity (undefined :: f p) 
    gpokeByteOffSum'        ptr off (R1 v) = gpokeByteOffSum' ptr off v
    gpokeByteOffSum'        ptr off (L1 v) = gpokeByteOffSum' ptr off v

instance GStorableSum' (f :*: g) where
    seeFirstByte' (l :*: g) acc = undefined
    gsizeOfSum'   _ = undefined
    alignOfSum'   _ = undefined
    gpeekByteOffSum' _ _ _ = undefined
    gpokeByteOffSum' _ _ _ = undefined

instance GStorableSum' (K1 i a) where
    seeFirstByte' _ acc = undefined
    gsizeOfSum'   _ = undefined
    alignOfSum'   _ = undefined
    gpeekByteOffSum' _ _ _ = undefined
    gpokeByteOffSum' _ _ _ = undefined

instance GStorableSum' (U1) where
    seeFirstByte' _ _ = undefined
    gsizeOfSum'   _   = undefined
    alignOfSum'   _   = undefined
    gpeekByteOffSum' _ _ _ = undefined
    gpokeByteOffSum' _ _ _ = undefined

instance (TypeError (Text "Haha!")) => GStorableSum' (V1) where
    seeFirstByte' _ _ = undefined
    gsizeOfSum'   _   = undefined
    alignOfSum'   _   = undefined
    gpeekByteOffSum' _ _ _ = undefined
    gpokeByteOffSum' _ _ _ = undefined


internalTagValue :: ( KnownNat (SumArity (Rep a))
                    , GStorableSum' (Rep a), Generic a)
                 => a -> Word8
internalTagValue (a :: a) = seeFirstByte' (from a) (sumArity (undefined :: Rep a p))


-- class GStorableSum a where
--     sizeOfSum :: a -> Int
--     default sizeOfSum :: ( GStorableSum' (Rep a), Generic a) 
--                       => a -> Int
--     sizeOfSum a = gsizeOfSum' (undefined :: Rep a p)
-- 
--     alignOfSum :: a -> Int
--     default alignOfSum :: ( GStorableSum' (Rep a), Generic a) 
--                        => a -> Int
--     alignOfSum a = alignOfSum' (undefined :: Rep a p)
-- 
--     gpeekByteOffSum :: Ptr b -> Int -> IO a
--     default gpeekByteOffSum :: ( GStorableSum' (Rep a), Generic a) 
--                             => Ptr b -> Int -> IO a
--     gpeekByteOffSum ptr off = do
--         choice <- peekByteOff ptr off :: IO Word8
--         to <$> gpeekByteOffSum' (fromIntegral choice) ptr (off + alignOfSum (undefined :: a))
-- 
--     gpokeByteOffSum :: Ptr b -> Int -> a -> IO ()
--     default gpokeByteOffSum :: ( GStorableSum' (Rep a), Generic a) 
--                             => Ptr b -> Int -> a -> IO ()
--     gpokeByteOffSum ptr off v = do
--         pokeByteOff ptr off (internalTagValue v - 1)
--         gpokeByteOffSum' ptr (off + alignOfSum v) (from v)
        
