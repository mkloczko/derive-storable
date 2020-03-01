{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module Foreign.Storable.Generic.Tools.TypeFuns where

import Data.Proxy
import GHC.Generics
import GHC.TypeLits

type family SumArity (arg :: * -> *) where
    SumArity (M1 C _ t) = 1
    SumArity (M1 _ _ t) =  SumArity t
    SumArity  (f :+: g) = (SumArity f) + (SumArity g)
    -- There should be no more constructors within products, but who knows..   
    SumArity  (f :*: g) = (SumArity f) + (SumArity g)
    SumArity _          = 0

type family NoFields (arg :: * -> *) where
    NoFields (M1 _ _ t) =  NoFields t
    NoFields (f  :+: g) = (NoFields f) + (NoFields g)
    NoFields (f  :*: g) = (NoFields f) + (NoFields g)
    NoFields (K1  _  _) = 1
    NoFields _          = 0

type IsSumType (arg :: * -> *) = IsSumType' (CmpNat (SumArity arg) (1))

type family IsSumType' (ret :: Ordering) :: Bool where
    IsSumType' GT = True
    IsSumType' _  = False

noFields :: (KnownNat (NoFields f)) => f p -> Int
noFields (a :: f p) = fromIntegral.natVal $ (Proxy :: Proxy (NoFields f)) 

sumArity :: (KnownNat (SumArity f)) => f p -> Int
sumArity (a :: f p) = fromIntegral.natVal $ (Proxy :: Proxy (SumArity f)) 
