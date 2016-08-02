{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts#-}
module Main where


-- Tested module.
import TestCases

-- Test libraries
import Test.Hspec
import Test.QuickCheck

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
        it "C17" $ property $ (same_size      :: C17 -> Expectation)
        it "C18" $ property $ (same_size      :: C18 -> Expectation)
    describe "Test for same alignment" $ do
        it "C1" $ property $ (same_alignment :: C1 -> Expectation)
        it "C2" $ property $ (same_alignment :: C2 -> Expectation)
        it "C3" $ property $ (same_alignment :: C3 -> Expectation)
        it "C4" $ property $ (same_alignment :: C4 -> Expectation)
        it "C5" $ property $ (same_alignment :: C5 -> Expectation)
        it "C6" $ property $ (same_alignment :: C6 -> Expectation)
        it "C17" $ property $ (same_alignment :: C17 -> Expectation)
        it "C18" $ property $ (same_alignment :: C18 -> Expectation)
    describe "Test for same offsets" $ do
        it "C1" $ property $ (same_offsets   :: C1 -> Expectation)
        it "C2" $ property $ (same_offsets   :: C2 -> Expectation)
        it "C3" $ property $ (same_offsets   :: C3 -> Expectation)
        it "C4" $ property $ (same_offsets   :: C4 -> Expectation)
        it "C5" $ property $ (same_offsets   :: C5 -> Expectation)
        it "C6" $ property $ (same_offsets   :: C6 -> Expectation)
        it "C17" $ property $ (same_offsets   :: C17 -> Expectation)
        it "C18" $ property $ (same_offsets   :: C18 -> Expectation)
    describe "Test for same fields" $ do
        it "C1" $ property $ (same_fields    :: C1 -> Expectation)
        it "C2" $ property $ (same_fields    :: C2 -> Expectation)
        it "C3" $ property $ (same_fields    :: C3 -> Expectation)
        it "C4" $ property $ (same_fields    :: C4 -> Expectation)
        it "C5" $ property $ (same_fields    :: C5 -> Expectation)
        it "C6" $ property $ (same_fields    :: C6 -> Expectation)
        it "C17" $ property $ (same_fields    :: C17 -> Expectation)
        it "C18" $ property $ (same_fields    :: C18 -> Expectation)
