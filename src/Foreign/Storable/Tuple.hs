module Foreign.Storable.Tuple where

import Foreign.Storable.Generic
import Foreign.Storable

instance (Storable a, Storable b) => GStorable (a,b)
instance (Storable a, Storable b, Storable c) => GStorable (a,b,c)
instance (Storable a, Storable b, Storable c,Storable d) => GStorable (a,b,c,d)
