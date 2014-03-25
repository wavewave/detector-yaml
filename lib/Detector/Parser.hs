{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Detector.Parser where

import           Control.Applicative ((<$>), (<*>), liftA)
import           Control.Monad ((<=<))
import           Data.Functor.Identity
import qualified Data.List as L
import           Data.Scientific
import qualified Data.Text as T
import           System.FilePath
--
import           Detector.Type
-- 
import           YAML.Parser
-- 
-- import           Debug.Trace


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
getDetectorDescription :: [(T.Text, PYaml)] -> Maybe (DetectorDescription (Either Import))
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
getObjectDescription :: [(T.Text,PYaml)] -> Maybe (ObjectDescription (Either Import)) 
getObjectDescription kvlst = do
    xs <- mapM (maybeObject <=< flip find kvlst) 
            [ "Electron", "Photon", "BJet", "Muon", "Jet"
            , "Tau", "PTThresholds" ]
    case xs of 
      [ eleObj, phoObj, bjetObj, muObj, jetObj, tauObj, ptThre ] -> do
        let importOrDeal :: ([(T.Text, PYaml)] -> Maybe a) -> [(T.Text, PYaml)] -> Maybe (Either Import a) 
            importOrDeal func = (either (Just . Left) (fmap Right . func) . getEitherImportOrObj) 
        e <- importOrDeal getElectronEffData eleObj
        p <-  importOrDeal getPhotonEffData phoObj
        b <-  importOrDeal getBJetEffData bjetObj
        m <- importOrDeal getMuonEffData muObj
        j <- importOrDeal getJetEffData jetObj
        ta <- importOrDeal getTauEffData tauObj
        let tk = (importOrDeal getTrackEffData <=< maybeObject <=< find "Track") kvlst
        pt <- importOrDeal getPTThresholds ptThre 
        return ObjectDescription
               { electron = e, photon = p, bJet = b, muon = m
               , jet = j, tau = ta, track = tk, ptThresholds = pt }
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
getPhotonEffData :: [ (T.Text,PYaml) ] -> Maybe PhotonEffData
getPhotonEffData kvlst = do 
    nm <- (maybeText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (maybeObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    return (PhotonEffData nm meta eff)

-- | 
getBJetEffData :: [ (T.Text,PYaml) ] -> Maybe BJetEffData
getBJetEffData kvlst = do
    nm <- (maybeText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (maybeObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    rejkvlst <- (maybeObject <=< find "Rejection") kvlst
    rej <- getPTEtaData rejkvlst
    return (BJetEffData nm meta eff rej)

-- | 
getMuonEffData :: [ (T.Text,PYaml) ] -> Maybe MuonEffData
getMuonEffData kvlst = do
    nm <- (maybeText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (maybeObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    return (MuonEffData nm meta eff)
  
-- | 
getJetEffData :: [ (T.Text,PYaml) ] -> Maybe JetEffData
getJetEffData kvlst = do
    nm <- (maybeText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (maybeObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    return (JetEffData nm meta eff)

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

-- | 
getTrackEffData :: [ (T.Text,PYaml) ] -> Maybe TrackEffData
getTrackEffData kvlst = do
    nm <- (maybeText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (maybeObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    return (TrackEffData nm meta eff)

-- |
getPTThresholds :: [(T.Text,PYaml)] -> Maybe PTThresholds
getPTThresholds kvlst = do
    nm <- (maybeText <=< find "Name") kvlst
    xs <- mapM (maybeNum <=< flip find kvlst) 
            [ "MuPTMIN", "ElePTMIN", "PhoPTMIN", "JetPTMIN"
            , "BJetPTMIN", "TrkPTMIN", "TauPTMIN" ] 
    case xs of 
      [ m, e, p, j, b, tr, ta ] ->
        return PTThresholds 
               { pTThreName = nm, muPTMin = m, elePTMin = e, phoPTMin = p
               , jetPTMin = j, bJetPTMin = b, trkPTMin = tr, tauPTMin = ta } 
      _ -> Nothing


--------------
-- import   --
--------------

importData :: ([(T.Text, PYaml)] -> Maybe a)  
           -> FilePath
           -> Either Import a 
           -> IO (Identity a) 
importData _ _ (Right x) = (return . Identity) x 
importData f rdir (Left (Import n)) = do
    let fname = rdir </> T.unpack n <.> "yaml"
    r <- parseFile fname 
    case r of
      Left err -> error err
      Right (PYObject kvlst) -> do 
        maybe (error ("parse " ++ fname ++ " failed")) 
              (return . Identity)
              (f kvlst)
      Right _ -> error "not an object"

importObjectDescription :: FilePath 
                        -> ObjectDescription (Either Import)
                        -> IO (ObjectDescription Identity)
importObjectDescription rdir ObjectDescription {..} = do
    ObjectDescription 
    <$> importData getElectronEffData rdir electron 
    <*> importData getPhotonEffData rdir photon
    <*> importData getBJetEffData rdir bJet
    <*> importData getMuonEffData rdir muon
    <*> importData getJetEffData rdir jet
    <*> importData getTauEffData rdir tau
    <*> maybe (return Nothing) (liftA Just . importData getTrackEffData rdir) track
    <*> importData getPTThresholds rdir ptThresholds

importDetectorDescription :: FilePath 
                          -> DetectorDescription (Either Import)
                          -> IO (DetectorDescription Identity)
importDetectorDescription rdir dd@DetectorDescription {..} = do 
    od <- importObjectDescription rdir detectorObject
    return dd { detectorObject = od }
