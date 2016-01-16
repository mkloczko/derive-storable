{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE DefaultSignatures #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE UndecidableInstances #-}


module Foreign.Storable.Generic (GStorable (..)) where



import Generics.Deriving
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import Data.Int

import Debug.Trace

import Foreign.Storable.Generic.Tools
import Foreign.Storable.Generic.Internal (GStorable (..))


--testData1 :: [(Size, Alignment)]
--testData1 = [(4,4),(3,1),(2,2),(1,1),(4,4)]

--testData2 :: [(Size, Alignment)]
--testData2 = [(1,1), (2,2), (2,1),(4,4),(8,2)]


