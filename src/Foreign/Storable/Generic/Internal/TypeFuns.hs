{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Foreign.Storable.Generic.Internal.TypeFuns where

import GHC.Generics
import GHC.TypeLits
import Data.Proxy (Proxy(..))
import GHC.Exts (inline)

type family NoFields (f :: * -> *) :: Nat where
    NoFields (K1 i c)    = 1
    NoFields ((:*:) f g) = (NoFields f + NoFields g)
    NoFields (M1 i c f)  =  NoFields f
    NoFields a           = TypeError (Text "Could not calculate the number of fields for the given type.\n\t I got stuck at: " :<>: ShowType a)

{-# INLINE getNoFields #-}
getNoFields :: forall f p. (KnownNat (NoFields (f))) => f p -> Int
getNoFields _ = fromIntegral $ inline $ natVal (Proxy :: Proxy (NoFields f )) 
