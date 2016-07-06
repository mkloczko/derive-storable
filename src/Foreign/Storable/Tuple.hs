module Foreign.Storable.Tuple where

import Foreign.Storable.Generic
import Foreign.Storable

instance (GStorable a, GStorable b) => GStorable (a,b)
instance (GStorable a, GStorable b, GStorable c) => GStorable (a,b,c)
instance (GStorable a, GStorable b, GStorable c,GStorable d) => GStorable (a,b,c,d)
