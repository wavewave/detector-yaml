{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Detector.Parser where

import           Control.Applicative ((<$>),(<*>))
import           Control.Monad ((<=<), liftM)
import qualified Data.List as L
import           Data.Scientific
import qualified Data.Text as T
--
import           Detector.Type
-- 
import           YAML.Parser
-- 
import           Debug.Trace


find :: T.Text -> [(T.Text,PYaml)] -> Maybe PYaml
find key = L.lookup key  

-- | Just only when PYText, and otherwise Nothing
maybeText :: PYaml -> Maybe T.Text 
maybeText (PYText txt) = Just txt
maybeText _ = Nothing

-- | Just only when PYObject, and otherwise Nothing
maybeObject :: PYaml -> Maybe [(T.Text,PYaml)]
maybeObject (PYObject kvlst) = Just kvlst
maybeObject _ = Nothing


-- | Just only when PYList, and otherwise Nothing
maybeList :: PYaml -> Maybe [PYaml]
maybeList (PYList lst) = Just lst
maybeList _ = Nothing

-- | Just only when PYNumber, and otherwise Nothing
maybeNum :: PYaml -> Maybe Scientific
maybeNum (PYNumber n) = (Just . realToFrac) n
maybeNum _ = Nothing


-- | get a detector description from parsed YAML object
getDetectorDescription :: [(T.Text, PYaml)] -> Maybe DetectorDescription
getDetectorDescription kvlst = do
    xs <- mapM (maybeText <=< flip find kvlst) 
            ["Name", "Description", "Reference"
            , "Comment", "ValidationInfo"]
    case xs of
      [ nm, dsc, ref, cmt, vinfo] -> do
        obj <- (getObjectDescription <=< maybeObject <=< find "Object") kvlst
        return DetectorDescription 
               { detectorName = nm
               , detectorDescription = dsc
               , detectorReference = ref
               , detectorComment = cmt
               , detectorValidationInfo = vinfo
               , detectorObject = obj }
      _ -> Nothing

-- | get an object description from parsed YAML object
getObjectDescription :: [(T.Text,PYaml)] -> Maybe ObjectDescription 
getObjectDescription kvlst = do
    xs <- mapM (maybeObject <=< flip find kvlst) 
            [ "Electron", "Photon", "BJet", "Muon", "Jet"
            , "Tau", "PTThresholds" ]
    case xs of 
      [ eleObj, phoObj, bjetObj, muObj, jetObj, tauObj, ptThre ] -> do
        let importOrDeal :: ([(T.Text, PYaml)] -> Maybe a) -> [(T.Text, PYaml)] -> Maybe (Either Import a) 
            importOrDeal func = (either (Just . Left) (fmap Right . func) . getEitherImportOrObj) 
            f = const undefined  
            -- (e,p,b,m,j,_pt) = 
        e <- importOrDeal getElectronEffData eleObj
        p <-  importOrDeal f phoObj
        b <-  importOrDeal f bjetObj
        m <- importOrDeal f muObj
        j <- importOrDeal f jetObj
        -- _pt <- importOrDeal f ptThre 
        ta <- importOrDeal getTauEffData tauObj
        return ObjectDescription
               { electron = e -- Left (Import "xx")
               , photon =  p --  Left (Import "xx")
               , bJet = b -- Left (Import "xx")
               , muon = m -- Left (Import "xx") 
               , jet =  j -- Left (Import "xx")
               , tau = ta --Left (Import "xx")
               , track = Nothing
               , ptThresholds = Left (Import "xx") 
               }
      _ -> Nothing
    
-- |
getEitherImportOrObj :: [(T.Text,PYaml)] -> Either Import [(T.Text,PYaml)]
getEitherImportOrObj kvlst =
    let mr = maybeText =<< find "Import" kvlst 
    in case mr of
      Just fname -> Left (Import fname)
      Nothing -> Right kvlst

-- | 
getPTEtaData :: [(T.Text,PYaml)] -> Maybe PTEtaData
getPTEtaData kvlst = do
    typ <- (maybeText <=< find "Type") kvlst
    binpt <- get1DList "PtBins" kvlst
    bineta <- get1DList "EtaBins" kvlst    
    g <- if | typ == "Grid" -> (getGrid <=< maybeObject <=< find "Grid") kvlst
            | typ == "Interpolation" -> error "Interpolation is not processed well yet" 
            | otherwise -> Nothing
    return PTEtaGrid { ptBins = binpt, etaBins = bineta, grid = g } 

-- | 
getGrid :: [(T.Text,PYaml)] -> Maybe Grid
getGrid kvlst = do
    typ <- (maybeText <=< find "Type") kvlst
    if | typ == "Full" -> GridFull <$> get2DList "Data" kvlst
       | typ == "Const" -> GridConst <$> (maybeNum <=< find "Data") kvlst
       | otherwise -> Nothing
    

-- | 
get1DList :: T.Text -> [(T.Text,PYaml)] -> Maybe [ Scientific ] 
get1DList key = (mapM maybeNum <=< maybeList <=< find key) 

-- | 
get2DList :: T.Text -> [(T.Text,PYaml)] -> Maybe [ [ Scientific ] ] 
get2DList key kvlst = do
    lst1 :: [ PYaml ] <- (maybeList <=< find key) kvlst 
    lstoflst :: [ [ PYaml ] ] <- mapM maybeList lst1 
    mapM (mapM maybeNum) lstoflst 


getMetaInfo :: [(T.Text, PYaml)] -> Maybe MetaInfo
getMetaInfo kvlst = do
    xs <- mapM (maybeText <=< flip find kvlst) ["Tag", "Description", "Comment", "Reference"]
    case xs of
      [ t, d, c, r ] ->  
        return (MetaInfo t d c r)
      _ -> Nothing

-- | 
getElectronEffData :: [ (T.Text,PYaml)] -> Maybe ElectronEffData
getElectronEffData kvlst = do
    nm <- (maybeText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (maybeObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    return (ElectronEffData nm meta eff)
      
-- |
getTauEffData :: [(T.Text,PYaml)] -> Maybe TauEffData
getTauEffData kvlst = do 
    nm <- (maybeText <=< find "Name") kvlst
    tagmtd <- (maybeText <=< find "TaggingMethod") kvlst
    meta <- getMetaInfo kvlst
    return TauEffData 
           { tauName = nm
           , tauMetaInfo = meta
           , tauTagMethod = tagmtd 
           , tauEfficiency = undefined
           }

    
-- efficiency