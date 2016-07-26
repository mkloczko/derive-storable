{-# LANGUAGE TemplateHaskell #-}

module Foreign.Storable.Generic.Tools.TH (
    Alignment,
    Size,
    Offset,
    Filling(..),
    calcOffsets,
    calcSize,
    calcAlignment,
    getOffset,
    deriveShow
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Foreign.Storable.Generic.Tools as R
import Foreign.Storable.Generic.Tools (Alignment, Size, Offset, Filling(..))
import Control.Monad

calcOffsets :: [(Size, Alignment)] -> Q Exp
calcOffsets size_align = [| $(lift $ R.calcOffsets size_align) |]

calcSize :: [(Size, Alignment)] -> Q Exp
calcSize size_align = [| $(lift $ R.calcSize size_align) |]

calcAlignment :: [Alignment] -> Q Exp
calcAlignment aligns = [| $(lift $ R.calcAlignment aligns) |]

getOffset :: [Offset] -> Int -> Q Exp 
getOffset offsets ix = [| $( lift $ offsets !! ix )|]

data T1 = T1
data T2 a = T2 a

deriveShow t = do
  -- Get list of constructors for type t
  TyConI (DataD _ _ _ _ constructors _)  <-  reify t

  -- Make `show` clause for one constructor:
  --   show (A x1 x2) = "A "++show x1++" "++show x2
  let showClause (NormalC name fields) = do
        -- Name of constructor, i.e. "A". Will become string literal in generated code
        let constructorName = nameBase name
        -- Get variables for left and right side of function definition
        (pats,vars) <- genPE (length fields)
        -- Recursively build (" "++show x1++...++"") expression from [x1...] variables list
        let f []       = [| "" |]
            f (v:vars) = [| " " ++ show $v ++ $(f vars) |]
        -- Generate function clause for one constructor
        clause [conP name pats]                                 -- (A x1 x2)
               (normalB [| constructorName ++ $(f vars) |]) []  -- "A "++show x1++" "++show x2

  -- Make body for function `show`:
  --   show (A x1 x2) = "A "++show x1++" "++show x2
  --   show (B x1)    = "B "++show x1
  --   show C         = "C"
  showbody <- mapM showClause constructors

  -- Generate template instance declaration and then replace
  --   type name (T1) and function body (\x -> "text") with our data
  d <- [d| instance Show T1 where
             show x = "text"
       |]
  let    [InstanceD a [] (AppT showt (ConT _T1)) [FunD showf _text]] = d
  return [InstanceD a [] (AppT showt (ConT t  )) [FunD showf showbody]]


-- Generate n unique variables and return them in form of patterns and expressions
genPE n = do
  ids <- replicateM n (newName "x")
  return (map varP ids, map varE ids)
