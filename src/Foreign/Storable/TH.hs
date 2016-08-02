module Foreign.Storable.TH 
    ( GStorable(..)
    , Storable (..)
    , deriveGStorable)
where

import Foreign.Storable.Generic.Internal.TH (deriveGStorable)
import Foreign.Storable.Generic             (GStorable (..) )
import Foreign.Storable                     (Storable  (..) )