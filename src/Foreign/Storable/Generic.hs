{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Foreign.Storable.Generic
-- Copyright   : (c) Mateusz Kłoczko, 2016
-- License     : MIT
-- Maintainer  : mateusz.p.kloczko@gmail.com
-- Stability   : experimental
-- Portability : portable
module Foreign.Storable.Generic
  ( Storable (..),
    Generically (..),
  )
where

import Foreign.Storable (Storable (..))
import Foreign.Storable.Generic.Internal (Generically (..))

{-
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
-}
