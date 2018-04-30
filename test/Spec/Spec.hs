import Test.Hspec

import qualified Foreign.Storable.Generic.InternalSpec            as I
import qualified Foreign.Storable.Generic.ToolsSpec               as T
import qualified Foreign.Storable.Generic.Internal.GStorable'Spec as GS1
import qualified Foreign.Storable.Generic.Internal.GStorableSpec  as GS2



main :: IO ()
main = hspec (I.spec >> T.spec >> GS1.spec >> GS2.spec)


