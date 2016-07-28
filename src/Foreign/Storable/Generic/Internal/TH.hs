{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Foreign.Storable.Generic.Internal.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Foreign.Storable.Generic.Internal (GStorable'(..), GStorable(..), internalSizeOf, internalAlignment, internalOffsets)
import Foreign.Storable.Generic.Instances
import Foreign.Storable.Generic.Tools
import qualified Foreign.Storable.Generic.Internal.TH.Internal as TH
import Data.Proxy
import Control.Monad
import GHC.Ptr (Ptr(..))

import GHC.Generics
import System.IO.Unsafe 

data Aaa = Aaaa Int Int deriving (Show, Generic )


generateCase :: Int -> [a] -> Q Exp
generateCase n ls = do
    -- First, the lambda:
    -- ... (a:b:c:rest) ix = ...
    abcs <- replicateM n $ newName "a"
    let pattern_match = generateListPatternMatch abcs
    -- And the body:
    -- case ix of
    --   0 -> a
    --   1 -> b
    --   ...
    let matches  = zipWith (\ix name -> Match (LitP (IntegerL ix)) (NormalB $ VarE name) []) [0,1..] abcs
        caseExpr = CaseE (VarE $ mkName "ix") matches
    lamE [pattern_match, varP $ mkName "ix"] (return caseExpr)

generateListPatternMatch :: [Name] -> Q Pat 
generateListPatternMatch names = do
    let head_name = mkName ":"
        infixHead pat1 patr = infixP pat1 head_name patr
        patterns =  (map varP names) ++ [wildP]
    foldr1 infixHead patterns

deriveGStorable t (theT :: a) = do
    -- Check whether a type is generic:
    -- TODO:
    -- case isClassInstance t
    TyConI (DataD _ _ _ _ constructors _) <- reify t
    let cons = case length constructors of 
            1 -> head constructors
            n -> error "GStorable instances work for types with exactly one constructor"    
        -- Try with lookup type name
        type_part = ConT t
        p_part   = VarT     $ mkName "p"
        rep_part = ConT    $ mkName "Rep"
        rep_type = AppT (AppT rep_part type_part) p_part
        rep_var  = SigE (VarE $ mkName "undefined") rep_type
        -- The undefined!
        --undef = VarI (mkName "undefined") rep_type Nothing
        undef = undefined :: Rep a p
        -- The Proxy!
        -- later dude
        gsizeOfClause _ = do
            let gsizeOf_RHS = lift $ internalSizeOf undef
            clause [wildP] (normalB gsizeOf_RHS) []
        galignmentClause _ = do
            let galignment_RHS = lift $ internalAlignment undef 
            clause [wildP] (normalB galignment_RHS) []
        gpeekClause _ = do
            let gpeek_RHS = [| to <$> gpeekByteOff' $(fun) offsets (no_fields-1) ptr offset |]
                offsets   = internalOffsets undef
                no_fields = gnumberOf' undef
                fun       = generateCase no_fields offsets
            clause [varP $ mkName "ptr", varP $ mkName "offset"] (normalB gpeek_RHS) []
        gpokeClause _ = do
            let gpeek_RHS = [| gpokeByteOff' $(fun) $(lift $ offsets) (no_fields-1) ptr offset (from val)|]
                offsets   = internalOffsets undef
                no_fields = gnumberOf' undef
                fun       = generateCase no_fields offsets            
            clause [varP $ mkName "ptr", varP $ mkName "offset", varP $ mkName "val"] (normalB gpeek_RHS) []    

    gsizeOfBody    <- gsizeOfClause cons
    galignmentBody <- galignmentClause cons
    gpeekBody      <- gpeekClause cons
    gpokeBody      <- gpokeClause cons
    -- Instance stuff
    let inst_overlap = Nothing
        inst_cxt     = []
        inst_type    = AppT (ConT (mkName "GStorable")) type_part
        inst_funs       = [ FunD (mkName "gsizeOf")    [gsizeOfBody]
                          , FunD (mkName "galignment") [galignmentBody]
                          , FunD (mkName "gpeekByteOff") [gpeekBody]
                          , FunD (mkName "gpokeByteOff") [gpokeBody]
                          ]
    return [InstanceD inst_overlap inst_cxt inst_type inst_funs]
