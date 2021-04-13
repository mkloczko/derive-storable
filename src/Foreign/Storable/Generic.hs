{-|
Module      : Foreign.Storable.Generic
Copyright   : (c) Mateusz Kłoczko, 2016
License     : MIT
Maintainer  : mateusz.p.kloczko@gmail.com
Stability   : experimental
Portability : portable



-}

{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE UndecidableInstances #-}


module Foreign.Storable.Generic (GStorable (..), Storable(..), getFilling) where



import Foreign.Storable (Storable(..))
import GHC.Generics

import Foreign.Storable.Generic.Internal (GStorable (..), GStorable' (..))
import qualified Foreign.Storable.Generic.Tools as Tools

{- | A helper to visualize layout.

>>> data Foo = Foo Word8 Float Word32 Float deriving (Show, Generic, GStorable)
>>> getFilling (Foo 0 0 0 0)
[Size 1,Padding 3,Size 4,Size 4,Size 4]

Nested structures are opaque to their parents:

>>> data Bar = Bar Foo Double Double
>>> getFilling $ Bar (Foo 0 0 0 0) 0 0
[Size 16,Size 8,Size 8]
-}
getFilling :: (Generic a, GStorable a, GStorable' (Rep a)) => a -> [Tools.Filling]
getFilling x = Tools.getFilling $ zip sizes aligns
    where sizes  = glistSizeOf'    gx
          aligns = glistAlignment' gx
          gx = from x
