{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Detector.Parser where

import           Control.Applicative
import           Control.Monad ((<=<))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
-- import           Data.Functor.Identity
import qualified Data.List as L
import           Data.Scientific
import qualified Data.Text as T
import           Data.Traversable
import           System.FilePath
--
import           Detector.Type
-- 
import           YAML.Parser
-- 
import Prelude hiding (mapM)
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
getDetectorDescription :: [(T.Text, PYaml)] -> Maybe (DetectorDescription ImportList) -- (Either Import)
getDetectorDescription kvlst = do
    xs <- mapM (maybeText <=< flip find kvlst) 
            ["Name", "Description", "Reference"
            , "Comment", "ValidationInfo"]
    case xs of
      [ nm, dsc, ref, cmt, vinfo] -> do
        idd <- (getIdentificationDescription <=< maybeObject <=< find "Identification") kvlst
        sm <- (getSmearingDescription <=< maybeObject <=< find "Smearing") kvlst
        return DetectorDescription 
               { detectorName = nm
               , detectorDescription = dsc
               , detectorReference = ref
               , detectorComment = cmt
               , detectorValidationInfo = vinfo
               , detectorIdentification = idd 
               , detectorSmearing = sm
               }
      _ -> Nothing

-- | get an object description from parsed YAML object
getIdentificationDescription :: [(T.Text,PYaml)] -> Maybe (IdentificationDescription ImportList) 
getIdentificationDescription kvlst = do
    xs <- mapM (maybeList <=< flip find kvlst) 
                 [ "Electron", "Photon", "BJet", "Muon", "Jet"
                 , "Tau", "PTThresholds" ]
    case xs of 
      [ eleObjs, phoObjs, bjetObjs, muObjs, jetObjs, tauObjs, ptThres ] -> do
        e <- ImportList <$> (traverse (importOrDeal getElectronEffData) =<< mapM maybeObject eleObjs)
        p <- ImportList <$> (traverse (importOrDeal getPhotonEffData) =<< mapM maybeObject phoObjs)
        b <- ImportList <$> (traverse (importOrDeal getBJetEffData) =<< mapM maybeObject bjetObjs)
        m <- ImportList <$> (traverse (importOrDeal getMuonEffData) =<< mapM maybeObject muObjs)
        j <- ImportList <$> (traverse (importOrDeal getJetEffData) =<< mapM maybeObject jetObjs)
        ta <- ImportList <$> (traverse (importOrDeal getTauEffData) =<< mapM maybeObject tauObjs)
        let mtk = do 
              trkObjs <- maybeList =<< find "Track" kvlst
              ImportList <$> (traverse (importOrDeal getTrackEffData) =<< mapM maybeObject trkObjs)
        pt <- ImportList <$> (traverse (importOrDeal getPTThresholds) =<< mapM maybeObject ptThres) 
        return IdentificationDescription 
               { electron = e
               , photon = p
               , bJet = b
               , muon = m
               , jet = j
               , tau = ta
               , track = mtk
               , ptThresholds = pt } 
      _ -> Nothing

importOrDeal :: forall a. ([(T.Text, PYaml)] -> Maybe a) -> [(T.Text, PYaml)] -> Maybe (Either Import a) 
importOrDeal func x = let y = fmap func (getEitherImportOrObj x) :: Either Import (Maybe a)
                      in sequenceA y


-- | get smearing description from parsed YAML object
getSmearingDescription :: [(T.Text,PYaml)] -> Maybe (SmearingDescription ImportList) 
getSmearingDescription kvlst = do
    xs <- mapM (maybeList <=< flip find kvlst) [ "Jet" ]
    case xs of 
      [ jetObjs ] -> do j <- ImportList <$> (traverse (importOrDeal getJetSmearData) =<< mapM maybeObject jetObjs)
                        return SmearingDescription { smearJet = j } 
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
    effkvlst <- (maybeObject <=< find "Efficiency") kvlst
    eff <- getTauEffDetail effkvlst
    return TauEffData 
           { tauName = nm
           , tauMetaInfo = meta
           , tauTagMethod = tagmtd 
           , tauEfficiency = eff
           }

-- | 
getTauEffDetail :: [ (T.Text,PYaml) ] -> Maybe TauEffDetail
getTauEffDetail kvlst = do 
    typ <- (maybeText <=< find "Type") kvlst
    if | typ == "Tau1or3Prong" -> getTau1or3Prong kvlst
       | typ == "TauCombined" -> getTauCombined kvlst

-- | 
getTau1or3Prong :: [ (T.Text,PYaml) ] -> Maybe TauEffDetail
getTau1or3Prong kvlst = do
    eff1 <- (getPTEtaData <=< maybeObject <=< find "Efficiency1Prong") kvlst
    rej1 <- (getPTEtaData <=< maybeObject <=< find "Rejection1Prong") kvlst
    eff3 <- (getPTEtaData <=< maybeObject <=< find "Efficiency3Prong") kvlst
    rej3 <- (getPTEtaData <=< maybeObject <=< find "Rejection3Prong") kvlst
    return (Tau1or3Prong eff1 rej1 eff3 rej3)

-- |
getTauCombined :: [ (T.Text,PYaml) ] -> Maybe TauEffDetail
getTauCombined kvlst = do 
    eff <- (getPTEtaData <=< maybeObject <=< find "Efficiency") kvlst
    rej <- (getPTEtaData <=< maybeObject <=< find "Rejection") kvlst
    return (TauCombined eff rej) 


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

getJetSmearData :: [ (T.Text,PYaml) ] -> Maybe JetSmearData
getJetSmearData kvlst = do
    nm <- (maybeText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    smkvlst <- (maybeObject <=< find "Smearing") kvlst
    sm <- getPTEtaData smkvlst
    return (JetSmearData nm meta sm)

--------------
-- import   --
--------------

importData :: ([(T.Text, PYaml)] -> Maybe a)  
           -> FilePath
           -> Either Import a 
           -> MaybeT IO a
importData _ _ (Right x) = return x 
importData f rdir (Left (Import n)) = do
    let fname = rdir </> T.unpack n <.> "yaml"
    r <- lift (parseFile fname)
    case r of
      Left err -> fail err
      Right (PYObject kvlst) -> do 
        maybe (fail ("parse " ++ fname ++ " failed")) 
              return
              (f kvlst)
      Right _ -> fail "not an object"

importIdentificationDescription :: FilePath 
                        -> IdentificationDescription ImportList
                        -> MaybeT IO (IdentificationDescription [])
importIdentificationDescription rdir IdentificationDescription {..} = do
    IdentificationDescription 
    <$> (traverse (importData getElectronEffData rdir) . unImportList) electron
    <*> (traverse (importData getPhotonEffData rdir) . unImportList) photon
    <*> (traverse (importData getBJetEffData rdir) . unImportList) bJet
    <*> (traverse (importData getMuonEffData rdir) . unImportList) muon
    <*> (traverse (importData getJetEffData rdir) . unImportList) jet
    <*> (traverse (importData getTauEffData rdir) . unImportList) tau
    <*> maybe (return Nothing) (liftA Just . (traverse (importData getTrackEffData rdir) . unImportList)) track
    <*> (traverse (importData getPTThresholds rdir) . unImportList) ptThresholds

importSmearingDescription :: FilePath 
                          -> SmearingDescription ImportList
                        -> MaybeT IO (SmearingDescription [])
importSmearingDescription rdir SmearingDescription {..} = do
    SmearingDescription 
    <$> (traverse (importData getJetSmearData rdir) . unImportList) smearJet

importDetectorDescription :: FilePath
                          -> DetectorDescription ImportList
                          -> MaybeT IO (DetectorDescription [])
importDetectorDescription rdir dd@DetectorDescription {..} = do 
    idd <- importIdentificationDescription rdir detectorIdentification
    sd <- importSmearingDescription rdir detectorSmearing 
    return dd { detectorIdentification = idd 
              , detectorSmearing = sd 
              }
