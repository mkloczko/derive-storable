{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts#-}
module Main where


-- Tested module.
import TestCases

-- Test libraries
import Test.Hspec
import Test.QuickCheck hiding (getSize)

-- Helpers
import Foreign.Marshal.Array
import Foreign.Storable


same_alignment a = getAlignment a `shouldReturn` alignment a
same_size a = getSize a `shouldReturn` sizeOf a
same_offsets a = do
    let offsets = goffsets a
    ptr <- mallocArray $ length offsets
    pokeArray ptr offsets
    checkOffsets a ptr `shouldReturn` True

same_fields a = do
    ptr1 <- newStorable a
    ptr2 <- new a
    checkFields ptr1 ptr2 `shouldReturn` True
main = hspec $ do
    describe "Test for same size" $ do
        it "C1" $ property $ (same_size      :: C1 -> Expectation)
        it "C2" $ property $ (same_size      :: C2 -> Expectation)
        it "C3" $ property $ (same_size      :: C3 -> Expectation)
        it "C4" $ property $ (same_size      :: C4 -> Expectation)
        it "C5" $ property $ (same_size      :: C5 -> Expectation)
        it "C6" $ property $ (same_size      :: C6 -> Expectation)
        it "C7" $ property $ (same_size      :: C7 -> Expectation)
        it "C8" $ property $ (same_size      :: C8 -> Expectation)
        it "C9" $ property $ (same_size      :: C9 -> Expectation)
        it "C10" $ property $ (same_size      :: C10 -> Expectation)
        it "C11" $ property $ (same_size      :: C11 -> Expectation)
        it "C12" $ property $ (same_size      :: C12 -> Expectation)
        it "C13" $ property $ (same_size      :: C13 -> Expectation)
        it "C14" $ property $ (same_size      :: C14 -> Expectation)
        it "C15" $ property $ (same_size      :: C15 -> Expectation)
        it "C16" $ property $ (same_size      :: C16 -> Expectation)
        it "C17" $ property $ (same_size      :: C17 -> Expectation)
        it "C18" $ property $ (same_size      :: C18 -> Expectation)
        it "C19" $ property $ (same_size      :: C19 -> Expectation)
        it "C20" $ property $ (same_size      :: C20 -> Expectation)
    describe "Test for same alignment" $ do
        it "C1" $ property $ (same_alignment :: C1 -> Expectation)
        it "C2" $ property $ (same_alignment :: C2 -> Expectation)
        it "C3" $ property $ (same_alignment :: C3 -> Expectation)
        it "C4" $ property $ (same_alignment :: C4 -> Expectation)
        it "C5" $ property $ (same_alignment :: C5 -> Expectation)
        it "C6" $ property $ (same_alignment :: C6 -> Expectation)
        it "C7" $ property $ (same_alignment :: C7 -> Expectation)
        it "C8" $ property $ (same_alignment :: C8 -> Expectation)
        it "C9" $ property $ (same_alignment :: C9 -> Expectation)
        it "C10" $ property $ (same_alignment :: C10 -> Expectation)
        it "C11" $ property $ (same_alignment :: C11 -> Expectation)
        it "C12" $ property $ (same_alignment :: C12 -> Expectation)
        it "C13" $ property $ (same_alignment :: C13 -> Expectation)
        it "C14" $ property $ (same_alignment :: C14 -> Expectation)
        it "C15" $ property $ (same_alignment :: C15 -> Expectation)
        it "C16" $ property $ (same_alignment :: C16 -> Expectation)
        it "C17" $ property $ (same_alignment :: C17 -> Expectation)
        it "C18" $ property $ (same_alignment :: C18 -> Expectation)
        it "C19" $ property $ (same_alignment :: C19 -> Expectation)
        it "C20" $ property $ (same_alignment :: C20 -> Expectation)
    describe "Test for same offsets" $ do
        it "C1" $ property $ (same_offsets   :: C1 -> Expectation)
        it "C2" $ property $ (same_offsets   :: C2 -> Expectation)
        it "C3" $ property $ (same_offsets   :: C3 -> Expectation)
        it "C4" $ property $ (same_offsets   :: C4 -> Expectation)
        it "C5" $ property $ (same_offsets   :: C5 -> Expectation)
        it "C6" $ property $ (same_offsets   :: C6 -> Expectation)
        it "C7" $ property $ (same_offsets   :: C7 -> Expectation)
        it "C8" $ property $ (same_offsets   :: C8 -> Expectation)
        it "C9" $ property $ (same_offsets   :: C9 -> Expectation)
        it "C10" $ property $ (same_offsets   :: C10 -> Expectation)
        it "C11" $ property $ (same_offsets   :: C11 -> Expectation)
        it "C12" $ property $ (same_offsets   :: C12 -> Expectation)
        it "C13" $ property $ (same_offsets   :: C13 -> Expectation)
        it "C14" $ property $ (same_offsets   :: C14 -> Expectation)
        it "C15" $ property $ (same_offsets   :: C15 -> Expectation)
        it "C16" $ property $ (same_offsets   :: C16 -> Expectation)
        it "C17" $ property $ (same_offsets   :: C17 -> Expectation)
        it "C18" $ property $ (same_offsets   :: C18 -> Expectation)
        it "C19" $ property $ (same_offsets   :: C19 -> Expectation)
        it "C20" $ property $ (same_offsets   :: C20 -> Expectation)
    describe "Test for same fields" $ do
        it "C1" $ property $ (same_fields    :: C1 -> Expectation)
        it "C2" $ property $ (same_fields    :: C2 -> Expectation)
        it "C3" $ property $ (same_fields    :: C3 -> Expectation)
        it "C4" $ property $ (same_fields    :: C4 -> Expectation)
        it "C5" $ property $ (same_fields    :: C5 -> Expectation)
        it "C6" $ property $ (same_fields    :: C6 -> Expectation)
        it "C7" $ property $ (same_fields    :: C7 -> Expectation)
        it "C8" $ property $ (same_fields    :: C8 -> Expectation)
        it "C9" $ property $ (same_fields    :: C9 -> Expectation)
        it "C10" $ property $ (same_fields    :: C10 -> Expectation)
        it "C11" $ property $ (same_fields    :: C11 -> Expectation)
        it "C12" $ property $ (same_fields    :: C12 -> Expectation)
        it "C13" $ property $ (same_fields    :: C13 -> Expectation)
        it "C14" $ property $ (same_fields    :: C14 -> Expectation)
        it "C15" $ property $ (same_fields    :: C15 -> Expectation)
        it "C16" $ property $ (same_fields    :: C16 -> Expectation)
        it "C17" $ property $ (same_fields    :: C17 -> Expectation)
        it "C18" $ property $ (same_fields    :: C18 -> Expectation)
        it "C19" $ property $ (same_fields    :: C19 -> Expectation)
        it "C20" $ property $ (same_fields    :: C20 -> Expectation)
