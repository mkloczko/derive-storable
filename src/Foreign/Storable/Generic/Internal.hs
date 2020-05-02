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
{-#LANGUAGE DeriveGeneric        #-}
{-#LANGUAGE TypeOperators        #-}
{-#LANGUAGE ScopedTypeVariables  #-}
{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE DataKinds            #-}

{-#LANGUAGE TypeFamilies         #-}
{-#LANGUAGE MultiParamTypeClasses#-}
{-#LANGUAGE ConstraintKinds      #-}
{-#LANGUAGE CPP                  #-}

module Foreign.Storable.Generic.Internal (
     GStorable(..),
     Generically (..),
#ifdef GSTORABLE_SUMTYPES
     GStorableSum(..),
     GStorableChoice'(..),
     GStorableChoice,
     internalTagValue,
#endif
     internalSizeOf,
     internalAlignment,
     internalPeekByteOff,
     internalPokeByteOff,
     internalOffsets
  ) where

import GHC.TypeLits
import GHC.Generics
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.Types

import Data.Coerce
import Data.Proxy
import Data.Word
import Data.Int

import Debug.Trace

import Foreign.Storable.Generic.Tools
import Foreign.Storable.Generic.Tools.TypeFuns

import GHC.Exts

-- Defining the generics ---

class GStorable f where
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


instance (GStorable f) => GStorable (M1 i t f) where
    -- Wrap the peeked value in metadata.
    {-# INLINE gpeekByteOff' #-}
    gpeekByteOff' offsets ix ptr offset = M1 <$> gpeekByteOff' offsets ix ptr offset
    -- Discard the metadata and go further.
    {-# INLINE gpokeByteOff' #-}
    gpokeByteOff' offsets ix ptr offset (M1 x) = gpokeByteOff' offsets ix ptr offset x 
    
    glistSizeOf' _ = glistSizeOf' (undefined :: f p)
    glistAlignment' _ = glistAlignment' (undefined :: f p)

instance GStorable U1 where
    -- Wrap the peeked value in metadata.
    {-# INLINE gpeekByteOff' #-}
    gpeekByteOff' offsets ix ptr offset = return U1
    -- Discard the metadata and go further.
    {-# INLINE gpokeByteOff' #-}
    gpokeByteOff' offsets ix ptr offset (U1) = return ()
    
    glistSizeOf'    _ = []
    glistAlignment' _ = []

instance (KnownNat (NoFields f), KnownNat (NoFields g)
         , GStorable f, GStorable g) => GStorable (f :*: g) where
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

instance (Storable a) => GStorable (K1 i a) where
    {-# INLINE gpeekByteOff' #-}
    gpeekByteOff' offsets ix ptr offset = K1 <$> peekByteOff ptr (off1 + offset)
        where off1 = inline (offsets !! ix)
    {-# INLINE gpokeByteOff' #-}
    gpokeByteOff' offsets ix ptr offset (K1 x) = pokeByteOff ptr (off1 + offset) x
        where off1 = inline (offsets !! ix) 


    -- When the constructor is used, return the size of 
    -- the constructed type in a list.
    glistSizeOf' _ = [sizeOf (undefined :: a)]
    -- When the constructor is used, return the alignment of 
    -- the constructed type in a list.
    glistAlignment' _ = [alignment (undefined :: a)]  


#ifndef GSTORABLE_SUMTYPES
type SumTypesDisabled = Text "By default sum types are not supported by GStorable instances." :$$: Text "You can pass a 'sumtypes' flag through 'cabal new-configure' to enable them." :$$: Text "In case of trouble, one can use '-DGSTORABLE_SUMTYPES' ghc flag instead." 

instance (TypeError SumTypesDisabled) => GStorable (f :+: g) where
    gpeekByteOff'   = undefined
    gpokeByteOff'   = undefined
    glistSizeOf'    = undefined
    glistAlignment' = undefined
#endif

-- These functions were moved outside GStorable type class.
-- They take generic representations as input.

{-# INLINE internalSizeOf #-}
-- | Calculates the size of generic data-type.
internalSizeOf :: forall f p. (GStorable f)
               => f p  -- ^ Generic representation 
               -> Int  -- ^ Resulting size
internalSizeOf _  = calcSize $ zip sizes aligns
    where sizes  = glistSizeOf'    (undefined :: f p)
          aligns = glistAlignment' (undefined :: f p)

{-# INLINE internalAlignment #-}
-- | Calculates the alignment of generic data-type.
internalAlignment :: forall f p. (GStorable f) 
                  => f p       -- ^ Generic representation
                  -> Alignment -- ^ Resulting alignment
internalAlignment  _  = calcAlignment aligns
    where aligns = glistAlignment' (undefined :: f p)

{-# INLINE internalPeekByteOff #-}
-- | View the variable under a pointer, with offset.
internalPeekByteOff :: forall f p b. (KnownNat (NoFields f), GStorable f) 
                    => Ptr b    -- ^ Pointer to peek 
                    -> Offset   -- ^ Offset 
                    -> IO (f p) -- ^ Resulting generic representation
internalPeekByteOff ptr off  = gpeekByteOff' offsets ix ptr off
    where offsets = internalOffsets (undefined :: f p)
          ix      = noFields (undefined :: f p) - 1

{-# INLINE internalPokeByteOff #-}
-- | Write the variable under the pointer, with offset.
internalPokeByteOff :: forall f p b. (KnownNat (NoFields f), GStorable f) 
                    => Ptr b  -- ^ Pointer to write to
                    -> Offset -- ^ Offset 
                    -> f p    -- ^ Written generic representation 
                    -> IO () 
internalPokeByteOff ptr off rep = gpokeByteOff' offsets ix ptr off rep
    where offsets = internalOffsets (undefined :: f p)
          ix      = noFields (undefined :: f p) - 1

{-# INLINE internalOffsets #-}
-- | Obtain the list of offsets
internalOffsets :: forall f p. (GStorable f)
                => f p      -- Generic representation
                -> [Offset] -- List of offsets
internalOffsets _ = calcOffsets $ zip sizes aligns
    where sizes = glistSizeOf'    (undefined :: f p)
          aligns= glistAlignment' (undefined :: f p)

newtype Generically a = Generically { unGenerically :: a }

#ifdef GSTORABLE_SUMTYPES
instance (ConstraintsSize a, ConstraintsAlignment a, ConstraintsPeek a, ConstraintsPoke a, GStorableChoice a) =>
  Storable (Generically a) where
    sizeOf (Generically x)= chSizeOf (Proxy :: Proxy (IsSumType (Rep a))) x
    alignment (Generically x)= chAlignment (Proxy :: Proxy (IsSumType (Rep a))) x
    peekByteOff ptr offset = Generically <$> chPeekByteOff (Proxy :: Proxy (IsSumType (Rep a))) ptr offset
    pokeByteOff ptr offset (Generically x) = chPokeByteOff (Proxy :: Proxy (IsSumType (Rep a))) ptr offset x
#else
instance (Generic a, GStorable (Rep a), KnownNat (NoFields (Rep a))) =>
  Storable (Generically a) where
    sizeOf _ = internalSizeOf (undefined :: Rep a p)
    alignment _ = internalAlignment (undefined :: Rep a p)
    peekByteOff ptr offset = Generically <$> to <$> internalPeekByteOff ptr offset
    pokeByteOff ptr offset (Generically x) = internalPokeByteOff ptr offset (from x)
#endif


#ifdef GSTORABLE_SUMTYPES
type GStorableChoice a = GStorableChoice' (IsSumType (Rep a)) a

-- | Choose a GStorable implementation - whether a sum type (with tag) or
-- raw product type (without the tag).
class GStorableChoice' (choice :: Bool) a where
    chSizeOf      :: proxy choice -> a     -> Int
    chAlignment   :: proxy choice -> a     -> Int
    chPeekByteOff :: proxy choice -> Ptr b -> Int -> IO a
    chPokeByteOff :: proxy choice -> Ptr b -> Int ->    a -> IO ()

-- | Implementation for the sum types.
instance ( Generic a, KnownNat (SumArity (Rep a))
         , GStorableSum (Rep a), IsSumType (Rep a) ~ True) => GStorableChoice' True a where
    {-# INLINE chSizeOf #-}
    {-# INLINE chPeekByteOff #-}
    {-# INLINE chPokeByteOff #-}
    {-# INLINE chAlignment #-}
    chSizeOf _  _ = calcSize $ zip sizes aligns
        where sizes  = (word8s:gsizeOfSum' (undefined :: Rep a p):[])
              aligns = (word8a:alignOfSum' (undefined :: Rep a p):[])
              word8s = sizeOf    (undefined :: Word8)
              word8a = alignment (undefined :: Word8) 
    chAlignment _ _  = calcAlignment $ (word8a:align:[])
        where align  = alignOfSum' (undefined :: Rep a p)
              word8a = alignment   (undefined :: Word8)
    chPeekByteOff _ ptr off = do
        let proxy = (Proxy :: Proxy True)
        choice <- peekByteOff ptr off :: IO Word8
        to <$> gpeekByteOffSum' (fromIntegral choice) ptr (off + chAlignment proxy (undefined :: a))
    chPokeByteOff _ ptr off v = do
        let proxy = (Proxy :: Proxy True)
        pokeByteOff ptr off (internalTagValue v - 1)
        gpokeByteOffSum' ptr (off + chAlignment proxy v) (from v)

-- | Implementation for the non-sum types. 
instance (ConstraintsAll a, IsSumType (Rep a) ~ False) => GStorableChoice' False a where
    {-# INLINE chSizeOf #-}
    {-# INLINE chPeekByteOff #-}
    {-# INLINE chPokeByteOff #-}
    {-# INLINE chAlignment #-}
    chSizeOf    _ _ = internalSizeOf    (undefined :: Rep a p)
    chAlignment _ _ = internalAlignment (undefined :: Rep a p)
    chPeekByteOff _ ptr offset = to <$> internalPeekByteOff ptr offset
    chPokeByteOff _ ptr offset x = internalPokeByteOff ptr offset (from x)


type ConstraintsAll       a = (ConstraintsSize a, ConstraintsPeek a)
type ConstraintsAlignment a = ConstraintsSA' (IsSumType (Rep a)) a 
type ConstraintsSize      a = ConstraintsSA' (IsSumType (Rep a)) a 
type ConstraintsPeek      a = ConstraintsP'  (IsSumType (Rep a)) a
type ConstraintsPoke      a = ConstraintsP'  (IsSumType (Rep a)) a

-- | Constrains for sizeof and alignment, either for sum or non-sum types.
type family ConstraintsSA' (t :: Bool) a where
    ConstraintsSA' True  a = (Generic a, GStorableSum (Rep a))
    ConstraintsSA' False a = (Generic a, GStorable    (Rep a))

-- | Constrains for peek and poke operations, either for sum or non-sum types.
type family ConstraintsP' (t :: Bool) a where
    ConstraintsP' True   a = ( Generic a, GStorableSum (Rep a))
    ConstraintsP' False  a = ( KnownNat (NoFields (Rep a)), Generic a, GStorable (Rep a))

-- | Get the tag value from the generic representation.
internalTagValue :: ( KnownNat (SumArity (Rep a))
                    , GStorableSum (Rep a), Generic a)
                 => a -> Word8
internalTagValue (a :: a) = seeFirstByte' (from a) (sumArity (undefined :: Rep a p))

-- | Work on the sum type.
class GStorableSum f where
    seeFirstByte'    :: f p -> Int -> Word8
    -- | The size of the biggest subtree
    gsizeOfSum'      :: f p -> Int
    -- | Alignment of the biggest subtree
    alignOfSum'      :: f p -> Int
    -- | Peek the type based on the tag.
    gpeekByteOffSum' :: Int -> Ptr b -> Int -> IO (f p)
    gpokeByteOffSum' ::        Ptr b -> Int -> f p -> IO ()

instance (GStorableSum f) => GStorableSum (M1 D t f) where
    {-# INLINE seeFirstByte'      #-}
    {-# INLINE gsizeOfSum'        #-}
    {-# INLINE alignOfSum'        #-}
    {-# INLINE gpeekByteOffSum'   #-}
    {-# INLINE gpokeByteOffSum'   #-}
    seeFirstByte'    (M1 v) acc = seeFirstByte' v acc
    gsizeOfSum'      (M1 v)     = gsizeOfSum' v
    alignOfSum'      (M1 v)     = alignOfSum' v
    gpeekByteOffSum' ch ptr off        = M1 <$> gpeekByteOffSum' ch ptr off
    gpokeByteOffSum'    ptr off (M1 v) =        gpokeByteOffSum'    ptr off v

instance (KnownNat (NoFields f), GStorable f, GStorableSum f) => GStorableSum (M1 C t f) where
    {-# INLINE seeFirstByte'      #-}
    {-# INLINE gsizeOfSum'        #-}
    {-# INLINE alignOfSum'        #-}
    {-# INLINE gpeekByteOffSum'   #-}
    {-# INLINE gpokeByteOffSum'   #-}
    seeFirstByte' (M1 v) acc = fromIntegral acc
    gsizeOfSum'   (M1 v)     = internalSizeOf    v
    alignOfSum'   (M1 v)     = internalAlignment v
    gpeekByteOffSum' _ ptr off   = M1 <$> internalPeekByteOff ptr off
    gpokeByteOffSum'   ptr off v = internalPokeByteOff ptr off v

instance ( KnownNat (SumArity g), KnownNat (SumArity f)
         , GStorableSum f, GStorableSum g) => GStorableSum (f :+: g) where
    {-# INLINE seeFirstByte'      #-}
    {-# INLINE gsizeOfSum'        #-}
    {-# INLINE alignOfSum'        #-}
    {-# INLINE gpeekByteOffSum'   #-}
    {-# INLINE gpokeByteOffSum'   #-}
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

instance (GStorableSum f) => GStorableSum (M1 S t f) where
    seeFirstByte'    _   _ = error "Shouldn't be here"
    gsizeOfSum'      _     = error "Shouldn't be here"
    alignOfSum'      _     = error "Shouldn't be here"
    gpeekByteOffSum' _ _ _ = error "Shouldn't be here"
    gpokeByteOffSum' _ _ _ = error "Shouldn't be here"


instance GStorableSum (f :*: g) where
    seeFirstByte' (l :*: g) acc = undefined
    gsizeOfSum'   _ = undefined
    alignOfSum'   _ = undefined
    gpeekByteOffSum' _ _ _ = undefined
    gpokeByteOffSum' _ _ _ = undefined

instance GStorableSum (K1 i a) where
    seeFirstByte' _ acc = undefined
    gsizeOfSum'   _ = undefined
    alignOfSum'   _ = undefined
    gpeekByteOffSum' _ _ _ = undefined
    gpokeByteOffSum' _ _ _ = undefined

instance GStorableSum (U1) where
    seeFirstByte' _ _ = undefined
    gsizeOfSum'   _   = undefined
    alignOfSum'   _   = undefined
    gpeekByteOffSum' _ _ _ = undefined
    gpokeByteOffSum' _ _ _ = undefined

instance GStorableSum (V1) where
    seeFirstByte' _ _ = undefined
    gsizeOfSum'   _   = undefined
    alignOfSum'   _   = undefined
    gpeekByteOffSum' _ _ _ = undefined
    gpokeByteOffSum' _ _ _ = undefined
#endif
