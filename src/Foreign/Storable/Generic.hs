{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE UndecidableInstances #-}


module Foreign.Storable.Generic (GStorable (..), Storable(..)) where



import Generics.Deriving
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc


--import Foreign.Storable.Generic.Tools
import Foreign.Storable.Generic.Internal (GStorable (..))


