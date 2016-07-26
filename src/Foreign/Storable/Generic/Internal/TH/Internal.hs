{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift #-}

-- For the Lift (Ptr a) instance
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Foreign.Storable.Generic.Internal.TH.Internal where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Foreign.Storable.Generic.Internal (GStorable'(..), GStorable(..))
import qualified Foreign.Storable.Generic.Tools.TH as TH
import Foreign.Storable.Generic.Tools
import Data.Proxy
import Control.Monad
import GHC.Ptr (Ptr(..))

import GHC.Generics



deriving instance Lift (f p) => Lift (M1 i c f p)
deriving instance (Lift (f p), Lift (g p)) => Lift ((:*:) f g p)
deriving instance Lift c => Lift (K1 i c p)
-- deriving instance (Lift b) => Lift (Ptr b)
deriving instance Lift (Rep a p) => Lift (Ptr a)


-- | Calculates the size of generic data-type.
internalSizeOf :: forall f p. (GStorable' f)
               => f p  -- ^ Generic representation 
               -> Q Exp  -- ^ Resulting size
internalSizeOf _  = do 
    let sizes  = glistSizeOf'    (undefined :: f p)
        aligns = glistAlignment' (undefined :: f p) 
    lift $ calcSize $ zip sizes aligns


-- | Calculates the alignment of generic data-type.
internalAlignment :: forall f p. (GStorable' f) 
                  => f p       -- ^ Generic representation
                  -> Q Exp -- ^ Resulting alignment
internalAlignment  _  = TH.calcAlignment $ glistAlignment' (undefined :: f p)
    where aligns = glistAlignment' (undefined :: f p)


-- | A workaround for not being able to pass types in ...
passInt :: Int -> Q Exp 
passInt x = [| x |]

internalNumberOf :: forall f p. ( GStorable' f)
                 => f p 
                 -> Q Exp
internalNumberOf _ = passInt (gnumberOf' (undefined :: f p) )

-- | Write the variable under the pointer, with offset.
internalPokeByteOff :: forall f p b. (GStorable' f) 
                    => f p
                   -- -> Ptr b  -- ^ Pointer to write to
                   -- -> Offset -- ^ Offset 
                   -- -> f p    -- ^ Written generic representation 
                    -> Q Exp
internalPokeByteOff _ {- ptr off rep -} = [| do  
    let ix      = $(internalNumberOf (undefined :: f p)) - 1
        offsets = $(internalOffsets  (undefined :: f p))
    gpokeByteOff' offsets ix
    |]


-- | View the variable under a pointer, with offset.
internalPeekByteOff :: forall f p b. (GStorable' f) 
                    => f p
                   -- -> Ptr b    -- ^ Pointer to peek 
                   -- -> Offset   -- ^ Offset 
                    -> Q Exp -- ^ Resulting generic representation
internalPeekByteOff _ {- ptr off -}  = [| do
    let offsets = $(internalOffsets  (undefined :: f p))
        ix      = $(internalNumberOf (undefined :: f p)) - 1
    gpeekByteOff' offsets ix
    |]
   

-- internalPokeByteOffTH = [| do
--     let ix      = gnumberOf' prox -1 
--         offsets = |]

-- | Obtain the list of offsets
internalOffsets :: forall f p. (GStorable' f)
                => f p      -- Generic representation
                -> Q Exp -- List of offsets
internalOffsets _ = TH.calcOffsets $ zip sizes aligns
    where sizes = glistSizeOf'    (undefined :: f p)
          aligns= glistAlignment' (undefined :: f p)
