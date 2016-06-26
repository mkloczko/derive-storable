{-#LANGUAGE DefaultSignatures #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Foreign.Storable.Generic.Meta where 

import Foreign.Storable.Generic.Internal
import Foreign.Storable.Generic.Tools
import GHC.Generics

class GStorableMeta a where
    -- | Get the fields' offsets.
    goffsets :: a     -- ^ The data type
             -> [Int] -- ^ Fields' offsets
    default goffsets :: (Generic a, GStorable' (Rep a))
                     => a -> [Int]
    goffsets _ = offsets   
        where sizes      = glistSizeOf'    (from (undefined :: a))
              alignments = glistAlignment' (from (undefined :: a))
              g_align    = maximum alignments -- Using galigment here generated bugs.
              offsets    = calcOffsets g_align $ zip sizes alignments
    -- | Get the fields' alignments.
    gsizes   :: a     -- ^ The data type
             -> [Int] -- ^ Fields' sizes
    default gsizes :: (Generic a, GStorable' (Rep a))
                     => a -> [Int]
    gsizes _ = glistSizeOf' (from (undefined :: a))  

    -- | Get the fields' alignments,
    galignments :: a     -- ^ The data type
                -> [Int] -- ^ Fields' alignments
    default galignments :: (Generic a, GStorable' (Rep a))
                        => a -> [Int]
    galignments _ = glistAlignment' (from (undefined :: a)) 
