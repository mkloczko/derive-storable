{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Foreign.Storable.Generic.Internal.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Foreign.Storable.Generic.Internal (GStorable'(..), GStorable(..), internalSizeOf, internalAlignment, internalOffsets)
import Foreign.Storable.Generic.Instances
import Foreign.Storable.Generic.Tools

import Control.Monad

import GHC.Generics


-- | Generate a lambda for transorming lists into case expressions.
-- ie. \(a:b:c:_) ix = case ix of
--          0 -> a
--          1 -> b
--          2 -> c

generateCase :: Int -> [a] -> Q Exp
generateCase n ls = do
    -- First, the the left side
    abcs <- replicateM n $ newName "a"
    let pattern_match = generateListPatternMatch abcs
    -- And the right side
    let matches  = zipWith (\ix name -> Match (LitP (IntegerL ix)) (NormalB $ VarE name) []) [0,1..] abcs
        caseExpr = CaseE (VarE $ mkName "ix") matches
    lamE [pattern_match, varP $ mkName "ix"] (return caseExpr)

-- | Generates a list pattern match
-- ie. a : b : c : _
generateListPatternMatch :: [Name] -> Q Pat 
generateListPatternMatch names = do
    let head_name = mkName ":"
        infixHead pat1 patr = infixP pat1 head_name patr
        patterns =  (map varP names) ++ [wildP]
    foldr1 infixHead patterns

deriveGStorable :: forall a. (GStorable' (Rep a)) => Name -> Q [Dec]
deriveGStorable t = do


    TyConI (DataD _ _ _ _ constructors _) <- reify t
    let cons = case length constructors of 
            1 -> head constructors
            n -> error "GStorable instances work for types with exactly one constructor"    
        -- Try with lookup type name
        type_part = ConT t


        undef = undefined :: Rep a p
        -- gsizeOf
        gsizeOfClause _ = do
            let gsizeOf_RHS = lift $ internalSizeOf undef
            clause [wildP] (normalB gsizeOf_RHS) []
        -- galignment
        galignmentClause _ = do
            let galignment_RHS = lift $ internalAlignment undef 
            clause [wildP] (normalB galignment_RHS) []
        -- gpeekByteOff
        gpeekClause _ = do
            let gpeek_RHS = [| to <$> gpeekByteOff' $(fun) offsets (no_fields-1) ptr offset |]
                offsets   = internalOffsets undef
                no_fields = gnumberOf' undef
                fun       = generateCase no_fields offsets
            clause [varP $ mkName "ptr", varP $ mkName "offset"] (normalB gpeek_RHS) []
        -- gpokeByteOf
        gpokeClause _ = do
            let gpeek_RHS = [| gpokeByteOff' $(fun) offsets (no_fields-1) ptr offset (from val)|]
                offsets   = internalOffsets undef
                no_fields = gnumberOf' undef
                fun       = generateCase no_fields offsets            
            clause [varP $ mkName "ptr", varP $ mkName "offset", varP $ mkName "val"] (normalB gpeek_RHS) []    

    gsizeOfBody    <- gsizeOfClause cons
    galignmentBody <- galignmentClause cons
    gpeekBody      <- gpeekClause cons
    gpokeBody      <- gpokeClause cons
    
    -- Instance 
    let inst_overlap = Nothing 
        inst_cxt     = []         
        inst_type    = AppT (ConT (mkName "GStorable"))  type_part
        inst_funs       = [ PragmaD $ InlineP (mkName "gsizeOf")      Inline FunLike AllPhases
                          , FunD (mkName "gsizeOf")      [gsizeOfBody]
                          , PragmaD $ InlineP (mkName "galignment")   Inline FunLike AllPhases
                          , FunD (mkName "galignment")   [galignmentBody]
                          , PragmaD $ InlineP (mkName "gpeekByteOff") Inline FunLike AllPhases
                          , FunD (mkName "gpeekByteOff") [gpeekBody]
                          , PragmaD $ InlineP (mkName "gpokeByteOff") Inline FunLike AllPhases
                          , FunD (mkName "gpokeByteOff") [gpokeBody]
                          ]
    return [InstanceD inst_overlap inst_cxt inst_type inst_funs]
