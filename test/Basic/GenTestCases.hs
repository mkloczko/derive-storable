module GenTestCases where

import Data.List


-- Parsable examples
sample1 = "C1 = Int32 Int32 Int8"
sample2 = "C2 = C1 Int8"

data Info = Info String [String] deriving (Show)

-- Parses the text.
getInfo :: String -> Maybe Info
getInfo str = case words str of
    (type_name:"=":f1:fields) -> Just $ Info type_name (f1:fields)
    otheriwse              -> Nothing

-------------
-- Haskell --
-------------

-- name (type, is_prim)
data HSStruct = HSStruct String [(String, Bool)]
instance Show HSStruct where
    show (HSStruct name types) = concat [fst_line, deriv_line]
        where fst_line   = concat ["data ",name , " = ", name," ", concat $ intersperse " " fields,"\n"]
              deriv_line = concat ["    deriving (Show, Eq, Generic, GStorable)\n\n"]
              fields     = map fst types

infoToHSStruct :: Info -> HSStruct
infoToHSStruct (Info name types) = HSStruct name hs_types
    where hs_types = map (\t -> (t, t `elem` hs_prims)) types

genCheckable :: HSStruct -> String
genCheckable (HSStruct name types) = concat [fst_line, fields_line, offsets_line, size_line, alignment_line, new_line]
    where fst_line       = concat ["instance Checkable ", name, " where\n"]
          fields_line    = concat ["    checkFields    ptr1 ptr2 = (==1) <$> checkFields",name," ptr1 ptr2\n"]
          offsets_line   = concat ["    checkOffsets   _    offs = (==1) <$> checkOffsets",name," offs\n"]
          size_line      = concat ["    getSize        a          = fromIntegral <$> getSize",name,"\n"]
          alignment_line = concat ["    getAlignment   a          = fromIntegral <$> getAlignment",name,"\n"]
          new_line       = concat ["    new ",pattern_match, " = do\n", do_vars, "\n"] 
          variables      = map return $ take (length types) ['a'..'z']
          is_prs         = map snd types
          constr_vars    = concat $ intersperse " " $ zipWith (\v is_pr -> if is_pr then v else "ptr_"++v) variables is_prs
          do_vars        = do
              let the_vars    = map fst $ filter (not.snd) $ zip variables is_prs
                  ptr_vars    = map (\v -> "ptr_"++v) the_vars
                  alloc_lines = zipWith (\ptr var -> concat ["        ", ptr, " <- newStorable " , var,"\n"]) ptr_vars the_vars
                  the_new_line  = concat ["        ptr <- new",name, " ", constr_vars,"\n"]
                  free_lines  = map (\ptr -> concat ["        free ",ptr,"\n"]) ptr_vars
                  papa_line   = "        return ptr\n"
              concat $ [concat alloc_lines, the_new_line, concat free_lines, papa_line]
          pattern_match  = concat ["(",name," ", concat $ intersperse " " $ variables, ")"]

genArbitrary :: HSStruct -> String
genArbitrary (HSStruct name types) = concat [fst_line, snd_line]
    where fst_line    = concat ["instance Arbitrary ", name, " where \n"]
          snd_line    = concat ["    arbitrary = ",name, " <$> ", arbitraries , "\n\n"]
          arbitraries = concat $ intersperse " <*> "$ take (length types) $ repeat "arbitrary"

genFFI :: HSStruct -> String
genFFI (HSStruct name types) = concat [new_line, fields_line, offsets_line, size_line, alignment] 
    where new_line    = concat [beginning, "new"         ,name," :: ", arguments,"\n"]
          types'      = map (\(t,b) -> if b then t else "Ptr " ++ t) types
          arguments   = concat [concat (intersperse " -> " types'), " -> IO (Ptr ", name, ")"]  
          fields_line = concat [beginning, "checkFields" ,name, " :: Ptr ", name, " -> Ptr ",name, " -> IO Int8\n"] 
          offsets_line= concat [beginning, "checkOffsets",name, " :: Ptr Int16 -> IO Int8\n"] 
          size_line   = concat [beginning, "getSize"     ,name, " :: IO Int16\n"] 
          alignment   = concat [beginning, "getAlignment",name, " :: IO Int16\n\n"]
          beginning   = "foreign import ccall "


headerHS = ["{-# LANGUAGE ForeignFunctionInterface #-}"
           ,"{-# LANGUAGE CApiFFI #-}" 
           ,"{-# LANGUAGE DeriveGeneric #-}"
           ,"{-# LANGUAGE DeriveAnyClass #-}"
           ,"{-# LANGUAGE FlexibleContexts #-}"
           ,"{-# LANGUAGE ScopedTypeVariables #-}"
           ,""
           ,"module TestCases where"
           ,""
           ,"import GHC.Generics (Generic, Rep, from)"
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
           ,"import Data.Int"
           ,"import Control.Monad (sequence, liftM)"
           ,"import System.Exit"
           ,""
           ,"import Test.QuickCheck"
           ,"-- The module tests the memory alignment of Storable data-types "
           ,"-- derived with generic-storable package. "
           ,"--"
           ,"-- The module will define data-types and the corresponding C structs"
           ,""
           ,"-- If fields ok and offsets not - the value of the fields before could use only some of the bits"
           ,""
           ,"-- If the offsets are ok and fields are not -- different types ? (Int vs Unsigned int)."
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
           ,"goffsets :: (GStorable' (Rep a), GStorable a, Generic a) => a -> [Int16]"
           ,"goffsets v = map fromIntegral $ calcOffsets $ zip sizes alignments "
           ,"    where sizes = glistSizeOf' (from v)"
           ,"          alignments = glistAlignment' (from v)"
           ]

-------
-- C --
-------
-- | name ((type, is_prim), fieldname)
data CStruct = CStruct String [((String,Bool),String)]

cstructArguments :: CStruct -> [String]
cstructArguments (CStruct name types_names) = map (\(t,n) -> concat [type_str t, " ", n]) types_names
    where type_str (tn, False) = concat [tn, "*"] 
          type_str (tn, True)= tn


accessAsPointer :: ((String,Bool),String) -> String
accessAsPointer ((_,False),n) = "*" ++ n
accessAsPointer ((_,True),n) = n

instance Show CStruct where
    show cs@(CStruct name types_names) = concat [beginning, concat field_lines, end]
       where beginning  = concat ["typedef struct ", name ,"{\n"] :: String
             args       = map (\((t,_),n) -> concat [t, " ", n]) types_names
             field_lines= map (\arg -> concat ["    ",arg, ";\n"]) args
             end        = concat ["} ", name, ";\n\n"]


infoToCStruct :: Info -> CStruct
infoToCStruct (Info name field_names) = CStruct name types_names
    where types_names = zip c_types names
          c_types = map (\n -> (toC n, (toC n) `elem` c_prims) ) field_names
          names   = map return ['a'..'z']

genConstructor :: CStruct -> String
genConstructor cs@(CStruct name types_names) = concat [fst_line, snd_line, concat middle_lines, prelast_line, last_line]
    where fst_line = concat [name, " * new", name,"(", args, "){\n"] 
          args     = concat $ intersperse ", "$ cstructArguments cs
          snd_line = concat ["    ",name, " * ret = (",name,"*) malloc(sizeof(",name,"));\n"]
          middle_lines = zipWith (\n1 n2 -> concat ["    ret->",n1," = ",n2, ";\n"])  (map snd types_names) (map accessAsPointer types_names)
          prelast_line = "    return ret;\n"
          last_line    = "}\n\n"

genPoke :: CStruct -> String
genPoke cs@(CStruct name types_names) = concat [fst_line, concat middle_lines, last_line] 
    where fst_line = concat ["void poke",name,"(", name,"* val, ", args,"){\n"]
          args     = concat $ intersperse ", " $ cstructArguments cs
          middle_lines = zipWith (\n1 n2 -> concat ["    val->",n1," = ", n2, ";\n"]) (map snd types_names) (map accessAsPointer types_names)
          last_line = "}\n\n"

genCheckOffsets :: CStruct -> String
genCheckOffsets (CStruct name types_names) = concat [fst_line, concat middle_lines, prelst_line, last_line]
    where fst_line = concat ["int checkOffsets",name,"(HsInt16 *offs){\n"]
          names_ixs = zip (map snd types_names) (map show [0,1..])
          middle_lines = map (\(n,i) -> concat ["    int ",n," = offsetof(",name,", ",n,") == offs[",i,"];\n"]) names_ixs
          prelst_line  = concat ["    return ",concat $ intersperse " && " $ map snd types_names, ";\n"]
          last_line    = "}\n\n"

genCheckFields :: CStruct -> String
genCheckFields (CStruct name types_names) = concat [fst_line, concat middle_lines, prelst_line, last_line]
    where fst_line = concat ["int checkFields",name,"(",name,"* s1, ", name,"* s2){\n"]
          names    = map snd types_names
          middle_lines = map (\tp@((_,_),n) -> concat ["    int ",n," = ", as_prim tp, ";\n"]) types_names
              where as_prim ((_,True ),n) = concat ["s1->",n, " == s2->",n]
                    as_prim ((t,False),n) = concat ["checkFields",t,"(&(s1->",n,"),&(s2->",n,"))"]
          prelst_line  = concat ["    return ",concat $ intersperse " && " names, ";\n"]
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

headerTest = ["{-# LANGUAGE ScopedTypeVariables #-}"
              ,"{-# LANGUAGE FlexibleContexts#-}"
              ,"module Main where"
              ,""
              ,""
              ,"-- Tested module."
              ,"import TestCases"
              ,""
              ,"-- Test libraries"
              ,"import Test.Hspec"
              ,"import Test.QuickCheck"
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

-------
-------

toC :: String -> String
toC "Int64" = "HsInt64"
toC "Int32" = "HsInt32"
toC "Int16" = "HsInt16"
toC "Int8"  = "HsInt8"
toC v       = v

c_prims  = ["HsInt64", "HsInt32", "HsInt16", "HsInt8"]
hs_prims = ["Int64", "Int32", "Int16", "Int8"]



-- Generates C structs and related functions
stuffC sample = do
    let info = getInfo sample
        c_struct = infoToCStruct <$> info
        c_cons  = genConstructor <$> c_struct
        c_checkOffs = genCheckOffsets   <$> c_struct
        c_checkFields = genCheckFields  <$> c_struct
        c_size        = genGetSize      <$> c_struct
        c_alignment   = genGetAlignment <$> c_struct
        c_poke        = genPoke <$> c_struct
    concat <$> sequence [show <$> c_struct, c_cons, c_poke, c_checkOffs, c_checkFields, c_size, c_alignment] 

-- Generates Haskell datatypes and related instances
stuffHS sample = do
    let info = getInfo sample
        hs_struct = infoToHSStruct <$> info 
        datatype  = show <$> hs_struct
        instanced = genCheckable <$> hs_struct
        arbitrary = genArbitrary <$> hs_struct
        ffi       = genFFI <$> hs_struct 
    concat <$> sequence [datatype, instanced, arbitrary, ffi]


-- Generated the files.
genFiles filename = do
   file <- readFile filename
   let cases = lines file
       header_hs   = concat $ map (++"\n") headerHS
       header_test = concat $ map (++"\n") headerTest
       header_c    = concat $ map (++"\n") headerC
       hs_code   = concat $ [ code | Just code <- map stuffHS cases]
       test_code = genTests [ t | Just (Info t _) <- map getInfo cases]
       c_code    = concat $ [ code | Just code <- map stuffC  cases]
   writeFile "MemoryCSpec.hs" (header_test ++ test_code)
   writeFile "TestCases.hs" (header_hs ++ hs_code)
   writeFile "cbits/TestCases.c"  (header_c ++ c_code)
