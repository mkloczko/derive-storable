module GenTestCases where

import Data.List


-- Parsable examples
sample1 = "C1 = Int32 Int32 Int8"
sample2 = "C2 = C1 Int8"
sample3 = "C3 = Int8 | Int16 Int32"

data Info = Info String [[String]] Bool deriving (Show)


getSums :: [String] -> [[String]]
getSums strs = go strs [] []
    where go []       [] acc = reverse acc
          go []       s1 acc = reverse ((reverse s1):acc)
          go ("|":ss) s1 acc = go ss [] ((reverse s1):acc)
          go (s:ss)   s1 acc = go ss (s:s1) acc

-- Parses the text.
getInfo :: String -> Maybe Info
getInfo str = case words str of
    (('*':type_name):"=":f1:fields) -> Just $ Info type_name (getSums $ f1:fields) True
    (type_name:"=":f1:fields)     -> Just $ Info type_name (getSums $ f1:fields) False
    [type_name] -> Just $ Info type_name [] False
    otherwise   -> Nothing

-------------
-- Haskell --
-------------

-- name (type, is_prim)
data HSStruct = HSStruct String [[(String, Bool)]]
instance Show HSStruct where
    show (HSStruct name types) = concat [fst_line, deriv_line]
        where fst_line   = case length types of
                  0 -> concat ["data ", name, " = ", name, "\n"]
                  1 -> concat ["data ", name, " = ", name, s_ix 0, " ", concat $ intersperse " " (map fst $ head types), "\n"]
                  n -> concat ["data ", name, " = ", name, s_ix 1, " ", concat $ intersperse " " (map fst $ head types), "\n"] 
                       ++ concat next_lines
              next_lines = case length types of
                  0 -> []
                  1 -> []
                  n -> zipWith (\i t -> next_fn i $ map fst t) [2..] (tail types) 
              deriv_line = concat ["    deriving (Show, Eq, Generic, GStorable)\n\n"]
              next_fn ix fields = concat [replicate (length name + 6) ' ',"| ", name, s_ix ix, " ", concat $ intersperse " " fields, "\n"]

infoToHSStruct :: Info -> HSStruct
infoToHSStruct (Info name types _) = HSStruct name hs_types
    where hs_types = map (map (\t -> (t, t `elem` hs_prims))) types

s_ix      ix  = if ix > 0 then '_':show ix else ""
six       ix  = if ix > 0 then     show ix else ""

genCheckable :: HSStruct -> String
-- genCheckable (HSStruct name types) = concat [fst_line, fields_line, offsets_line, size_line, alignment_line, new_line]
genCheckable (HSStruct name types) = concat [fst_line, fields_line, offsets_lines, size_line, alignment_line, new_line]
    where fst_line       = concat ["instance Checkable ", name, " where\n"]
          fields_line    = concat ["    checkFields    ptr1 ptr2 = (==1) <$> checkFields",name," ptr1 ptr2\n"]
          -- offsets_line   = concat ["    checkOffsets   _    offs = (==1) <$> checkOffsets",name," offs\n"]
          size_line      = concat ["    getSize        a          = fromIntegral <$> getSize",name,"\n"]
          alignment_line = concat ["    getAlignment   a          = fromIntegral <$> getAlignment",name,"\n"]
          --- news
          new_line'       = case length types of
              0 -> new_fn 0 []
              1 -> new_fn 0 $ head types
              n -> concat $ zipWith new_fn [1..] types
          new_line       = new_line' ++ "\n"
          new_fn ix tps  = concat ["    new ",pattern_match ix tps, " = do\n", do_vars ix tps] 
          ---
          offsets_lines     = case length types of
              0 -> offsets_fn 0 []
              1 -> offsets_fn 0 $ head types
              n -> concat $ zipWith offsets_fn [1..] types
          offsets_fn 0  tps  = concat ["    checkOffsets   _    offs = (==1) <$> checkOffsets",name," offs\n"]
          offsets_fn ix tps  = concat ["    checkOffsets ", pattern_match ix tps, " offs = (==1) <$> checkOffsets",name, s_ix ix, " offs\n"]
          ---
          variables tps  = map return $ take (length tps) ['a'..'z']
          is_prs    tps  = map snd tps
          constr_vars var ptrs = concat $ intersperse " " $ zipWith (\v is_pr -> if is_pr then v else "ptr_"++v) var ptrs
          do_vars ix tps  = do
              let vars        = variables tps
                  prs         = is_prs tps
                  constants   = constr_vars vars prs
                  the_vars    = map fst $ filter (not.snd) $ zip vars prs
                  ptr_vars    = map (\v -> "ptr_"++v) the_vars
                  alloc_lines = zipWith (\ptr var -> concat ["        ", ptr, " <- newStorable " , var,"\n"]) ptr_vars the_vars
                  the_new_line= concat ["        ptr <- new",name,s_ix ix, " ", constants,"\n"]
                  free_lines  = map (\ptr -> concat ["        free ",ptr,"\n"]) ptr_vars
                  papa_line   = "        return ptr\n"
              concat $ [concat alloc_lines, the_new_line, concat free_lines, papa_line]
          pattern_match ix tps = concat ["(",name,s_ix ix," ", concat $ intersperse " " $ variables tps, ")"]

genArbitrary :: HSStruct -> String
genArbitrary (HSStruct name     []) = genArbitrary' name []
genArbitrary (HSStruct name [prod]) = genArbitrary' name prod
genArbitrary (HSStruct name  types) = concat [fst_line, snd_line,"\n"]
    where fst_line = concat ["instance Arbitrary ", name, " where \n"]
          snd_line = concat ["    arbitrary = oneof ", list_lines]   
          list_lines  = concat $ intersperse "                      " fillings
          fillings    = ('[':' ':f):(map (\x -> ',':' ':x) illings) ++ ["]"]
          (f:illings) = zipWith (\ix t -> filling ix t ++ "\n") [1..] types
          filling ix  tps = concat [name, s_ix ix, " <$> ", arbitraries tps]
          arbitraries tps = concat $ intersperse " <*> "$ take (length tps) $ repeat "arbitrary"
    

genArbitrary' :: String -> [(String, Bool)] -> String
genArbitrary' name tps = concat [fst_line, snd_line, "\n"]
    where fst_line = concat ["instance Arbitrary ", name, " where \n"]
          snd_line = case tps of 
              []  -> concat ["    arbitrary = return ",name, "\n"]
              tps -> concat ["    arbitrary = ",name, " <$> ", arbitraries, "\n"]
          arbitraries = concat $ intersperse " <*> "$ take (length tps) $ repeat "arbitrary"

genFFI :: HSStruct -> String
genFFI (HSStruct name types) = concat [new_lines, fields_line, offsets_lines, size_line, alignment] 
    where new_lines     = case length types of
              0 -> new_fn 0 []
              1 -> new_fn 0 $ head types
              n -> concat $ zipWith new_fn [1..] types
          new_fn ix tps = concat [beginning, "new", name, s_ix ix," :: ", arguments tps,"\n"]
          types'    tps = map (\(t,b) -> if b then t else "Ptr " ++ t) tps
          arguments tps = concat [concat (intersperse " -> " (types' tps ++ ["IO (Ptr "++ name++ ")"]))]  
          fields_line = concat [beginning, "checkFields" ,name, " :: Ptr ", name, " -> Ptr ",name, " -> IO Int8\n"] 
          offsets_lines = case length types of
              0 -> offsets_un
              1 -> offsets_un
              n -> concat $ offsets_un:map offsets_fn [1..length types]
          offsets_un    = concat [beginning, "checkOffsets",name, " :: Ptr Int16 -> IO Int8\n"] 
          offsets_fn ix = concat [beginning, "checkOffsets",name, s_ix ix, " :: Ptr Int16 -> IO Int8\n"] 
          size_line   = concat [beginning, "getSize"     ,name, " :: IO Int16\n"] 
          alignment   = concat [beginning, "getAlignment",name, " :: IO Int16\n\n"]
          beginning   = "foreign import ccall "


headerHS = ["{-# LANGUAGE ForeignFunctionInterface #-}"
           ,"{-# LANGUAGE CApiFFI #-}"
           ,"{-# LANGUAGE CPP     #-}"
           ,"{-# LANGUAGE DeriveGeneric #-}"
           ,"{-# LANGUAGE DeriveAnyClass #-}"
           ,"{-# LANGUAGE FlexibleContexts #-}"
           ,"{-# LANGUAGE FlexibleInstances    #-}"
           ,"{-# LANGUAGE ScopedTypeVariables  #-}"
           ,"{-# LANGUAGE TypeOperators        #-}"
           ,"{-# LANGUAGE DataKinds            #-}"
           ,"{-# LANGUAGE UndecidableInstances #-}"
           ,""
           ,"module TestCases where"
           ,""
           ,"import GHC.Generics hiding (C1,S1)"
           ,"import GHC.TypeLits "
           ,"import Foreign.C.Types"
           ,"import Foreign.Storable"
           ,"import Foreign.Storable.Generic"
           ,"import Foreign.Ptr (Ptr)"
           ,"import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr)"
           ,"import Foreign.Marshal.Alloc (malloc, free)"
           ,"import Foreign.Marshal.Array (mallocArray, pokeArray)"
           ,""
           ,"import Foreign.Storable.Generic.Tools"
           ,"import Foreign.Storable.Generic.Internal"
           ,"import Foreign.Storable.Generic.Instances"
           ,"import Data.Int"
           ,"import Control.Monad (sequence, liftM)"
           ,"import System.Exit"
           ,""
           ,"import Test.QuickCheck"
           ,"-- The module tests the memory alignment of Storable data-types "
           ,"-- derived with generic-storable package. "
           ,""
           ,"-- Adding parametric polimorphism to reduce the boilerplate."
           ,"class (Storable a) => Checkable a where"
           ,"  -- | Checks whether the fields are the same"
           ,"  checkFields  :: Ptr a -> Ptr a -> IO Bool "
           ,"  -- | Checks whether the offsets are the same"
           ,"  checkOffsets :: a -> Ptr Int16 -> IO Bool "
           ,"  -- | Checks whether the size is the same"
           ,"  getSize    :: a -> IO Int              "
           ,"  -- | Checks whether the alignment is the same"
           ,"  getAlignment :: a-> IO Int              "
           ,"  "
           ,"  new          :: a -> IO (Ptr a)"
           ,""
           ,""
           ,"newStorable :: Storable a => a -> IO (Ptr a)"
           ,"newStorable val = do"
           ,"  ptr <- malloc"
           ,"  poke ptr val"
           ,"  return ptr"
           ,""
           ,""
           ,"class SumOffsets' f where"
           ,"    sumOffsets' :: f p -> [Offset]"
           ,""
           ,"instance (SumOffsets' f) => SumOffsets' (M1 D t f) where"
           ,"    sumOffsets' (M1 v) = sumOffsets' v"
           ,""
           ,"instance (GStorable' f, SumOffsets' f) => SumOffsets' (M1 C t f) where"
           ,"    sumOffsets' (M1 v) = internalOffsets v"
           ,""
           ,"instance (SumOffsets' f, SumOffsets' g) => SumOffsets' (f :+: g) where"
           ,"    sumOffsets' (L1 v) = sumOffsets' v"
           ,"    sumOffsets' (R1 v) = sumOffsets' v"
           ,""
           ,"instance SumOffsets' (M1 S t f) where"
           ,"    sumOffsets' _ = undefined"
           ,"instance SumOffsets' (f :*: g) where"
           ,"    sumOffsets' _ = undefined"
           ,"instance SumOffsets' (K1 i a) where"
           ,"    sumOffsets' _ = undefined"
           ,"instance SumOffsets' (U1) where"
           ,"    sumOffsets' _ = undefined"
           ,"instance SumOffsets' (V1) where"
           ,"    sumOffsets' _ = undefined"
           ,""
           ,""
           ,"goffsets :: (SumOffsets' (Rep a), GStorable a, Generic a) => a -> [Int16]"
           ,"goffsets v = map fromIntegral $ sumOffsets' (from v)"
--           ,"goffsets :: (GStorable' (Rep a), GStorable a, Generic a) => a -> [Int16]"
--           ,"goffsets v = map fromIntegral $ internalOffsets (from v)"
           ,""
           ]

-------
-- C --
-------
-- | name [((type, is_prim), fieldname)]
data CStruct = CStruct String [[((String,Bool),String)]]

cstructArguments :: CStruct -> [[String]]
cstructArguments (CStruct _ structs) = map cstructArguments' structs

cstructArguments' :: [((String,Bool),String)] -> [String]
cstructArguments' types_names = map (\(t,n) -> concat [type_str t, " ", n]) types_names
    where type_str (tn, False) = concat [tn, "*"] 
          type_str (tn, True)= tn



accessAsPointer :: ((String,Bool),String) -> String
accessAsPointer ((_,False),n) = "*" ++ n
accessAsPointer ((_, True),n) = n

instance Show CStruct where
    show cs@(CStruct name types_names) = structs 
       where structs = case length types_names of
                  0 -> struct_def 0 []
                  1 -> struct_def 0 $ head types_names
                  n -> concat $ (zipWith struct_def [1..] types_names) ++ [union]
             struct_def ix tps = concat [beginning ix, field_lines tps, end ix]
             -- struct things
             beginning ix = concat ["typedef struct ", name, s_ix ix ,"{\n"] :: String
             args   tps = map (\((t,_),n) -> concat [t, " ", n]) tps
             field_lines tps = concat $ map (\arg -> concat ["    ",arg, ";\n"]) $ args tps
             end  ix = concat ["} ", name,s_ix ix, ";\n\n"]
             -- union things
             union = concat [ union_beginning, union_fill, union_end
                            , stunion_beginning, tag_line, union_line, stunion_end 
                            ]
             stunion_beginning = concat ["typedef struct ", name, " {\n"]
             stunion_end       = concat ["} ",name,";\n\n"]
             tag_line          = concat ["    HsWord8 tag;\n"]
             union_line        = concat ["    ",name,"_union val;\n"]
             union_beginning   = concat ["typedef union ", name, "_union","{\n"]
             union_fill        = concat $ zipWith union_fill_fn [1..length types_names] $ map return ['a'..'z']
             union_end         = concat ["} ",name,"_union;\n\n"]
             union_fill_fn ix n= concat ["    ", name, s_ix ix, " ", n,";\n"]


infoToCStruct :: Info -> CStruct
infoToCStruct (Info name field_names _) = CStruct name $ map types_names field_names
    where types_names tps = zip (c_types tps) names
          c_types tps = map (\n -> (toC n, (toC n) `elem` c_prims) ) tps
          names   = map return ['a'..'z']
 
genConstructor :: CStruct -> String
genConstructor (CStruct name    []) = genConstructor' name []
genConstructor (CStruct name [one]) = genConstructor' name $ one
genConstructor (CStruct name  sums) = concat $ zipWith (\ix ft -> genConstructorUnion' ix name ft) [1..] sums  

genConstructorUnion' :: Int -> String -> [((String, Bool),String)] -> String
genConstructorUnion' ix name types_names = concat [fst_line, snd_line, trd_line, concat middle_lines, prelast_line, last_line]
    where fst_line = concat [name, " * new", name, s_ix ix, "(", args, "){\n"] 
          args     = concat $ intersperse ", "$ cstructArguments' types_names
          snd_line = concat ["    ",name, " * ret = (",name,"*) malloc(sizeof(",name,"));\n"]
          trd_line = concat ["    ret->tag = ",show (ix-1), ";\n"]
          middle_lines = zipWith (\n1 n2 -> concat ["    ret->val.",union_val, ".",n1," = ",n2, ";\n"])  (map snd types_names) (map accessAsPointer types_names)
          prelast_line = "    return ret;\n"
          last_line    = "}\n\n"
          union_val    = (map return ['a'..'z']) !! (ix - 1)

genConstructor' :: String -> [((String, Bool),String)] -> String
genConstructor' name types_names = concat [fst_line, snd_line, concat middle_lines, prelast_line, last_line]
    where fst_line = concat [name, " * new", name,"(", args, "){\n"] 
          args     = concat $ intersperse ", "$ cstructArguments' types_names
          snd_line = concat ["    ",name, " * ret = (",name,"*) malloc(sizeof(",name,"));\n"]
          middle_lines = zipWith (\n1 n2 -> concat ["    ret->",n1," = ",n2, ";\n"])  (map snd types_names) (map accessAsPointer types_names)
          prelast_line = "    return ret;\n"
          last_line    = "}\n\n"

genPoke :: CStruct -> String
genPoke (CStruct name    []) = genPoke' name []
genPoke (CStruct name [one]) = genPoke' name $ one
genPoke (CStruct name  sums) = concat $ zipWith (\ix ft -> genPokeUnion' ix name ft) [1..] sums  

genPokeUnion' :: Int -> String -> [((String,Bool),String)] -> String
genPokeUnion' ix name types_names = concat [fst_line, snd_line, concat middle_lines, last_line] 
    where fst_line = concat ["void poke",name,s_ix ix,"(", args, "){\n"]
          args     = concat $ intersperse ", " $(concat [name, "* un"]) :cstructArguments' types_names
          snd_line = concat ["    un->tag = ", show $ ix - 1, ";\n"]
          middle_lines = zipWith (\n1 n2 -> concat ["    un->val.",union_val,".",n1," = ", n2, ";\n"]) (map snd types_names) (map accessAsPointer types_names)
          last_line = "}\n\n"
          union_val = (map return ['a'..'z']) !! (ix - 1)

genPoke' :: String -> [((String,Bool),String)] -> String
genPoke' name types_names = concat [fst_line, concat middle_lines, last_line] 
    where fst_line = concat ["void poke",name,"(", args, "){\n"]
          args     = concat $ intersperse ", " $(concat [name, "* val"]) :cstructArguments' types_names
          middle_lines = zipWith (\n1 n2 -> concat ["    val->",n1," = ", n2, ";\n"]) (map snd types_names) (map accessAsPointer types_names)
          last_line = "}\n\n"


genCheckOffsets :: CStruct -> String
genCheckOffsets (CStruct name    []) = genCheckOffsets' name []
genCheckOffsets (CStruct name [one]) = genCheckOffsets' name $ one
genCheckOffsets (CStruct name  sums) = concat $ offsets ++ [genCheckOffsetsUnion' name]
    where offsets = zipWith (\ix ft -> genCheckOffsets' (name ++ s_ix ix) ft) [1..] sums

genCheckOffsetsUnion' :: String -> String
genCheckOffsetsUnion' name = concat [fst_line, snd_line, trd_line, fth_line, last_line]
    where fst_line = concat ["int checkOffsets",name,"(HsInt16 *offs){\n"]
          snd_line = concat ["    int t = offsetof(",name,", tag) == offs[0];\n"]
          trd_line = concat ["    int v = offsetof(",name,", val) == offs[1];\n"]
          fth_line = concat ["    return t && v;\n"]
          last_line    = "}\n\n"

genCheckOffsets' :: String -> [((String,Bool),String)] -> String
genCheckOffsets' name types_names = concat [fst_line, concat middle_lines, prelst_line, last_line]
    where fst_line = concat ["int checkOffsets",name,"(HsInt16 *offs){\n"]
          names_ixs = zip (map snd types_names) (map show [0,1..])
          middle_lines = map (\(n,i) -> concat ["    int ",n," = offsetof(",name,", ",n,") == offs[",i,"];\n"]) names_ixs
          prelst_line  = if length types_names > 0
                           then concat ["    return ",concat $ intersperse " && " $ map snd types_names, ";\n"]
                           else "    return 1;\n"
          last_line    = "}\n\n"

genCheckFields :: CStruct -> String
genCheckFields (CStruct name    []) = genCheckFields' name []
genCheckFields (CStruct name [one]) = genCheckFields' name $ one
genCheckFields (CStruct name  sums) = concat $ offsets ++ [genCheckFieldsUnion' (length sums) name]
    where offsets = zipWith (\ix ft -> genCheckFields' (name ++ s_ix ix) ft) [1..] sums

genCheckFieldsUnion' :: Int -> String -> String
genCheckFieldsUnion' n name = concat [fst_line, snd_line, mid_lines, prelst_line, last_line]
    where fst_line  = concat ["int checkFields",name,"(",name,"* s1, ", name,"* s2){\n"]
          snd_line  = concat ["    if (s1->tag != s2->tag) return 0;\n"] 
          mid_lines = concat $ map mid_fn [1..n]
          mid_fn ix = concat ["    if (s1->tag == ", show (ix-1),") return checkFields",name++s_ix ix
                             ,"(&s1->val.",u_val ix,",&s2->val.",u_val ix,");\n"] 
          prelst_line = concat ["    return 0;\n"]
          last_line = "}\n\n"
          u_val  ix = (map return ['a'..'z']) !! (ix - 1)

genCheckFields' :: String -> [((String,Bool),String)] -> String
genCheckFields' name types_names = concat [fst_line, concat middle_lines, prelst_line, last_line]
    where fst_line = concat ["int checkFields",name,"(",name,"* s1, ", name,"* s2){\n"]
          names    = map snd types_names
          middle_lines = map (\tp@((_,_),n) -> concat ["    int ",n," = ", as_prim tp, ";\n"]) types_names
              where as_prim ((_,True ),n) = concat ["s1->",n, " == s2->",n]
                    as_prim ((t,False),n) = concat ["checkFields",t,"(&(s1->",n,"),&(s2->",n,"))"]
          prelst_line  = if length types_names > 0
                           then concat ["    return ",concat $ intersperse " && " names, ";\n"]
                           else "    return 1; \n"
          last_line = "}\n\n"

genGetSize :: CStruct -> String
genGetSize (CStruct name _) = concat [fst_line, middle_line, last_line]
    where fst_line    = concat ["HsInt16 getSize", name,"() {\n"]
          middle_line = concat ["    return sizeof(",name,");\n"]
          last_line   = "}\n\n";

genGetAlignment :: CStruct -> String
genGetAlignment (CStruct name _) = concat [fst_line, middle_line, last_line]
    where fst_line    = concat ["HsInt16 getAlignment", name,"() {\n"]
          middle_line = concat ["    return alignof(",name,");\n"]
          last_line   = "}\n\n";

headerC = ["#include <stddef.h>"
          ,"#include <stdio.h>"
          ,"#include <stdalign.h>"
          ,"#include <stdlib.h>"
          ,"#include \"HsFFI.h\""
          ]

-----------
-- Hspec --
-----------

headerTest =  ["{-# LANGUAGE ScopedTypeVariables #-}"
              ,"{-# LANGUAGE FlexibleContexts    #-}"
              ,"{-# LANGUAGE CPP                 #-}"
              ,"module Main where"
              ,""
              ,""
              ,"-- Tested module."
              ,"import TestCases"
              ,""
              ,"-- Test libraries"
              ,"import Test.Hspec"
              ,"import Test.QuickCheck hiding (getSize)"
              ,""
              ,"-- Helpers"
              ,"import Foreign.Marshal.Array"
              ,"import Foreign.Storable"
              ,""
              ,""
              ,"same_alignment a = getAlignment a `shouldReturn` alignment a"
              ,"same_size a = getSize a `shouldReturn` sizeOf a"
              ,"same_offsets a = do"
              ,"    let offsets = goffsets a"
              ,"    ptr <- mallocArray $ length offsets"
              ,"    pokeArray ptr offsets"
              ,"    checkOffsets a ptr `shouldReturn` True"
              ,""
              ,"same_fields a = do"
              ,"    ptr1 <- newStorable a"
              ,"    ptr2 <- new a"
              ,"    checkFields ptr1 ptr2 `shouldReturn` True"
              ]


genTests :: [String] -> String
genTests types = (++) fst_line $ concat $ map ("    "++) $ concat [sizes_lines, aligns_lines, off_lines, fields_lines]
    where fst_line  = "main = hspec $ do\n"
          size  n   = concat ["it \"", n, "\" $ property $ (same_size      :: ", n," -> Expectation)\n"]
          align n   = concat ["it \"", n, "\" $ property $ (same_alignment :: ", n," -> Expectation)\n"]
          off   n   = concat ["it \"", n, "\" $ property $ (same_offsets   :: ", n," -> Expectation)\n"]
          fields n  = concat ["it \"", n, "\" $ property $ (same_fields    :: ", n," -> Expectation)\n"]
          sizes_lines  = ("describe \"Test for same size\" $ do\n" ): (map (\t -> "    " ++ size t) types)
          aligns_lines = ("describe \"Test for same alignment\" $ do\n" ): (map (\t -> "    " ++ align t) types)
          off_lines    = ("describe \"Test for same offsets\" $ do\n" ): (map (\t -> "    " ++ off t) types)
          fields_lines = ("describe \"Test for same fields\" $ do\n" ): (map (\t -> "    " ++ fields t) types)

genTestsS :: [String] -> String
genTestsS types = concat $ map ("    "++) $ concat [sizes_lines, aligns_lines, off_lines, fields_lines]
    where fst_line  = "main = hspec $ do\n"
          size  n   = concat ["it \"", n, "\" $ property $ (same_size      :: ", n," -> Expectation)\n"]
          align n   = concat ["it \"", n, "\" $ property $ (same_alignment :: ", n," -> Expectation)\n"]
          off   n   = concat ["it \"", n, "\" $ property $ (same_offsets   :: ", n," -> Expectation)\n"]
          fields n  = concat ["it \"", n, "\" $ property $ (same_fields    :: ", n," -> Expectation)\n"]
          sizes_lines  = ("describe \"Test for same size - sums\" $ do\n" ): (map (\t -> "    " ++ size t) types)
          aligns_lines = ("describe \"Test for same alignment - sums\" $ do\n" ): (map (\t -> "    " ++ align t) types)
          off_lines    = ("describe \"Test for same offsets - sums\" $ do\n" ): (map (\t -> "    " ++ off t) types)
          fields_lines = ("describe \"Test for same fields - sums\" $ do\n" ): (map (\t -> "    " ++ fields t) types)

-- -------
-- -------
-- 
toC :: String -> String
toC "Int64" = "HsInt64"
toC "Int32" = "HsInt32"
toC "Int16" = "HsInt16"
toC "Int8"  = "HsInt8"
toC "Double" = "HsDouble"
toC "Float" = "HsFloat"
toC v       = v

c_prims  = ["HsInt64", "HsInt32", "HsInt16", "HsInt8","HsDouble", "HsFloat"]
hs_prims = ["Int64", "Int32", "Int16", "Int8", "Double", "Float"]



-- Generates C structs and related functions
stuffC info = do
    let c_struct = infoToCStruct info
        c_cons  = genConstructor c_struct
        c_checkOffs = genCheckOffsets   c_struct
        c_checkFields = genCheckFields  c_struct
        c_size        = genGetSize      c_struct
        c_alignment   = genGetAlignment c_struct
        c_poke        = genPoke c_struct
    concat [show c_struct, c_cons, c_poke, c_checkOffs, c_checkFields, c_size, c_alignment] 

-- Generates Haskell datatypes and related instances
stuffHS info = do
    let hs_struct = infoToHSStruct info 
        datatype  = show hs_struct
        instanced = genCheckable hs_struct
        arbitrary = genArbitrary hs_struct
        ffi       = genFFI hs_struct 
    concat [datatype, instanced, arbitrary, ffi]

wrapInIfdefs :: String -> String
wrapInIfdefs s = concat ["#ifdef GSTORABLE_SUMTYPES\n", s, "#endif"]

-- Generated the files.
genFiles filename = do
   file <- readFile filename
   let cases = lines file
       infos = [i | Just i <- map getInfo cases]
       infosP = filter (\(Info _ _ t) -> not t) infos
       infosS = filter (\(Info _ _ t) ->     t) infos
       header_hs   = concat $ map (++"\n") headerHS
       header_test = concat $ map (++"\n") headerTest
       header_c    = concat $ map (++"\n") headerC
       hs_codeP   = concat $ map stuffHS infosP
       hs_codeS   = concat $ map stuffHS infosS
       test_codeP = genTests  $ map (\(Info n _ _) -> n) infosP
       test_codeS = genTestsS $ map (\(Info n _ _) -> n) infosS
       c_code     = concat $ map stuffC  infos
   writeFile "MemoryCSpec.hs" (header_test ++ test_codeP ++ wrapInIfdefs test_codeS)
   writeFile "TestCases.hs"   (header_hs   ++ hs_codeP   ++ wrapInIfdefs hs_codeS)
   writeFile "cbits/TestCases.c"  (header_c ++ c_code)

-- Default usage
main = genFiles "TestCases"
