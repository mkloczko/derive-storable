{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE UndecidableInstances #-}


module Foreign.Storable.Generic (GStorable (..), Storable(..)) where



import Foreign.Storable (Storable(..))

import Foreign.Storable.Generic.Internal (GStorable (..))
import Foreign.Storable.Generic.Instances


------Association to Storable class-------

instance {-# OVERLAPS #-} (GStorable a) => (Storable a) where
    {-# INLINE sizeOf #-}
    sizeOf      = gsizeOf
    {-# INLINE alignment #-}
    alignment   = galignment
    {-# INLINE peekByteOff #-}
    peekByteOff = gpeekByteOff
    {-# INLINE pokeByteOff #-}
    pokeByteOff = gpokeByteOff


