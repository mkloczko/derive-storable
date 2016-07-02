{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE UndecidableInstances #-}


module Foreign.Storable.Generic (GStorable (..), Storable(..)) where



import Foreign.Storable
import Foreign.Storable.Generic.Instances

--import Foreign.Storable.Generic.Tools
import Foreign.Storable.Generic.Internal (GStorable (..))


