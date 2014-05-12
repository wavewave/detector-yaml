{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Detector.Parser where

import           Control.Applicative
import           Control.Monad ((<=<), liftM)
import           Control.Monad.Morph
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Either
-- import           Data.Bifunctor
import           Data.Functor.Identity
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import           Data.Scientific
import qualified Data.Text as T
import           Data.Traversable
import           System.Directory
import           System.FilePath
--
import           Detector.Type
-- 
import           YAML.Parser
-- 
import Prelude hiding (mapM)

type Context = [String]
type ErrorContextT m = EitherT String (StateT Context m)

withContext :: (Monad m) => 
               String 
            -> ErrorContextT m a
            -> ErrorContextT m a 
withContext ctxt action = do 
    lift $ modify (ctxt:) 
    r <- action
    lift $ modify tail 
    return r

             -- -> EitherT String (StateT [String] m) a 
             -- -> EitherT String (StateT [String] m) a


instance MFunctor (EitherT e) where
    hoist nat m = EitherT (nat (runEitherT m))

find :: (Monad m) => T.Text -> [(T.Text,PYaml)] -> EitherT String m PYaml
find key kvlst = maybe (left ("Cannot find " ++ T.unpack key)) 
                       right 
                       (L.lookup key kvlst)

-- | Just only when PYText, and otherwise Nothing
eitherText :: (Monad m) => PYaml -> EitherT String m T.Text 
eitherText (PYText txt) = right txt
eitherText _ = left "Not a text"

-- | 
eitherBool :: (Monad m) => PYaml -> EitherT String m Bool
eitherBool (PYText txt) = let txtCap = T.toUpper txt 
                         in if | txtCap == "TRUE" -> right True
                               | txtCap == "FALSE" -> right False
                               | otherwise -> left "Not a bool"
eitherBool _ = left "Not a bool"

-- | Just only when PYObject, and otherwise Nothing
eitherObject :: (Monad m) => PYaml -> EitherT String m [(T.Text,PYaml)]
eitherObject (PYObject kvlst) = right kvlst
eitherObject _ = left "Not an object"


-- | Just only when PYList, and otherwise Nothing
eitherList :: (Monad m) => PYaml -> EitherT String m [PYaml]
eitherList (PYList lst) = right lst
eitherList _ = left "Not a list"

-- | Just only when PYNumber, and otherwise Nothing
eitherNum :: (Monad m) => PYaml -> EitherT String m Scientific
eitherNum (PYNumber n) = (right . realToFrac) n
eitherNum _ = left "Not a number"


-- | get a detector description from parsed YAML object
getDetectorDescription :: (Monad m) => 
                          [(T.Text, PYaml)] 
                       -> ErrorContextT m (DetectorDescription ImportList) 
getDetectorDescription kvlst = withContext "DetectorDescription" $ do
    xs <- mapM (eitherText <=< flip find kvlst) 
            ["Name", "Description", "Reference"
            , "Comment", "ValidationInfo"]
    case xs of
      [ nm, dsc, ref, cmt, vinfo] -> do
        rd <- (getRangeDescription <=< eitherList <=< find "Range") kvlst
        idd <- (getIdentificationDescription <=< eitherObject <=< find "Identification") kvlst
        sm <- (getSmearingDescription <=< eitherObject <=< find "Smearing") kvlst
        return DetectorDescription 
               { detectorName = nm
               , detectorDescription = dsc
               , detectorReference = ref
               , detectorComment = cmt
               , detectorValidationInfo = vinfo
               , detectorRange = rd
               , detectorIdentification = idd 
               , detectorSmearing = sm
               }
      _ -> left "Not a Detector Description"

-- | get an object description from parsed YAML object
getIdentificationDescription :: (Monad m) =>
                                [(T.Text,PYaml)] 
                             -> ErrorContextT m (IdentificationDescription ImportList)
getIdentificationDescription kvlst = withContext "Identification Description" $ do 
    xs <- mapM (eitherList <=< flip find kvlst) 
                 [ "Electron", "Photon", "BJet", "Muon", "Jet"
                 , "Tau", "PTThresholds" ]
    case xs of 
      [ eleObjs, phoObjs, bjetObjs, muObjs, jetObjs, tauObjs, ptThres ] -> do
        e <- ImportList <$> (traverse (importOrDeal getElectronEffData) =<< mapM eitherObject eleObjs)
        p <- ImportList <$> (traverse (importOrDeal getPhotonEffData) =<< mapM eitherObject phoObjs)
        b <- ImportList <$> (traverse (importOrDeal getBJetEffData) =<< mapM eitherObject bjetObjs)
        m <- ImportList <$> (traverse (importOrDeal getMuonEffData) =<< mapM eitherObject muObjs)
        j <- ImportList <$> (traverse (importOrDeal getJetEffData) =<< mapM eitherObject jetObjs)
        ta <- ImportList <$> (traverse (importOrDeal getTauEffData) =<< mapM eitherObject tauObjs)
        let etk = runIdentity . flip evalStateT [] . runEitherT $ do 
              trkObjs <- eitherList =<< find "Track" kvlst
              ImportList <$> (traverse (importOrDeal getTrackEffData) =<< mapM eitherObject trkObjs)
            mtk = either (const Nothing) Just etk
        pt <- ImportList <$> (traverse (importOrDeal getPTThresholds) =<< mapM eitherObject ptThres) 
        return IdentificationDescription 
               { electron = e
               , photon = p
               , bJet = b
               , muon = m
               , jet = j
               , tau = ta
               , track = mtk
               , ptThresholds = pt } 
      _ -> left "Not an identification description"

importOrDeal :: forall a m. (Monad m) => 
                ([(T.Text, PYaml)] -> EitherT String m a)
             -> [(T.Text, PYaml)] 
             -> EitherT String m (Either Import a) 
importOrDeal func x = let y = fmap func (getEitherImportOrObj x) 
                      in sequenceA y


-- | get smearing description from parsed YAML object
getSmearingDescription :: forall m. (Monad m) => 
                          [(T.Text,PYaml)] 
                       -> ErrorContextT m (SmearingDescription ImportList) 
getSmearingDescription kvlst = withContext "SmearingDescription" $ do
    xs <- mapM (eitherList <=< flip find kvlst) [ "Electron", "Muon", "Photon", "Jet", "Track", "Tau", "MissingET" ]
    case xs of 
      [ elecObjs, muonObjs, phoObjs, jetObjs, trkObjs, tauObjs, metObjs ] -> do 
        SmearingDescription <$> implst elecObjs <*> implst muonObjs <*> implst phoObjs 
                            <*> implst jetObjs <*> implst trkObjs <*> implst tauObjs <*> implst metObjs
      _ -> left "Not a Smearing Description"
  where implst ys = ImportList <$> (traverse (importOrDeal getSmearData) =<< mapM eitherObject ys)

    
-- | 
getRangeDescription :: (Monad m) => 
                       [PYaml] 
                    -> ErrorContextT m (RangeDescription ImportList)
getRangeDescription lst = withContext "RangeDescription" $ RangeDescription <$> implst lst
  where implst ys = ImportList <$> (traverse (importOrDeal getRangeData) =<< mapM eitherObject ys)


-- |
getEitherImportOrObj :: [(T.Text,PYaml)] 
                     -> Either Import [(T.Text,PYaml)]
getEitherImportOrObj kvlst = 
    let mr = (runIdentity . runEitherT) (eitherText =<< find "Import" kvlst)
    in case mr of
         Right fname -> Left (Import fname)
         Left _ -> Right kvlst

-- | 
getPTEtaData :: (Monad m) => [(T.Text,PYaml)] -> ErrorContextT m PTEtaData
getPTEtaData kvlst = withContext "PTEtaData" $ do
    typ <- (eitherText <=< find "Type") kvlst
    b <- (eitherBool <=< find "IsEtaSymmetric") kvlst
    if | typ == "Grid" -> do 
         binpt <- get1DList "PtBins" kvlst
         bineta <- get1DList "EtaBins" kvlst    
         g <- (getGrid <=< eitherObject <=< find "Grid") kvlst
         return PTEtaGrid { isEtaSymmetric = b, ptBins = binpt, etaBins = bineta, grid = g } 
       | typ == "Interpolation" -> do
         i <- (getInterpolation <=< eitherObject <=< find "Interpolation") kvlst
         return (PTEtaInterpolation i b)
       | otherwise -> left ("Cannot understand Type : " ++ T.unpack typ)

-- | 
getGrid :: (Monad m) => [(T.Text,PYaml)] -> ErrorContextT m Grid
getGrid kvlst = do
    typ <- (eitherText <=< find "Type") kvlst
    if | typ == "Full" -> GridFull <$> get2DList "Data" kvlst
       | typ == "Const" -> GridConst <$> (eitherNum <=< find "Data") kvlst
       | otherwise -> left "Not a Grid"
    


-- | 
get1DList :: (Monad m) => T.Text -> [(T.Text,PYaml)] -> ErrorContextT m [Scientific] 
get1DList key = withContext ("obtaining 1D list of " ++ T.unpack key) .  
                  (mapM eitherNum <=< eitherList <=< find key) 

-- | 
get2DList :: (Monad m) => T.Text -> [(T.Text,PYaml)] -> ErrorContextT m [[Scientific]] 
get2DList key kvlst = withContext ("obtaining 2D list of " ++ T.unpack key) $ do
    lst1 :: [ PYaml ] <- (eitherList <=< find key) kvlst 
    lstoflst :: [ [ PYaml ] ] <- mapM eitherList lst1 
    mapM (mapM eitherNum) lstoflst 


-- | 
getInterpolation :: (Monad m) => [(T.Text,PYaml)] -> ErrorContextT m Interpolation
getInterpolation kvlst = withContext "getting Interpolation" $ do
    typ <- (eitherText <=< find "Type") kvlst
    if typ == "Constant" 
      then withContext "Constant interpolation" $ do 
        v <- (eitherNum <=< find "Value") kvlst
        return (IPConstant v) 
      else do 
        lst <- (eitherList <=< find "EtaBinContent") kvlst 
        lst2 <- mapM (getFuncBin <=< eitherObject) lst
        etabound <- (eitherNum <=< find "EtaBound") kvlst
        if | typ == "PredefinedMode1" -> return (IPPredefinedMode1 lst2 etabound)
           | typ == "PredefinedMode2" -> return (IPPredefinedMode2 lst2 etabound)
           | typ == "PredefinedMode3" -> return (IPPredefinedMode3 lst2 etabound)
           | otherwise -> left "cannot understand an interpolation Type"
      

-- | 
getFuncBin :: (Monad m) => [(T.Text,PYaml)] 
           -> EitherT String m (FuncBin (HM.HashMap Int Scientific) )
getFuncBin kvlst = do 
   b <- (eitherNum <=< find "BinStart") kvlst
   lst <- (eitherList <=< find "BinContent") kvlst
   lst' <- mapM getIntNum lst
   return (FuncBin b (HM.fromList lst'))

-- | 
getIntNum :: (Monad m) => PYaml -> EitherT String m (Int, Scientific)
getIntNum (PYList (PYNumber x : PYNumber y : [])) = return (round x, realToFrac y)
getIntNum _ = left "Not an (Int,Number)"





getMetaInfo :: (Monad m) => [(T.Text, PYaml)] -> EitherT String m MetaInfo
getMetaInfo kvlst = do
    xs <- mapM (eitherText <=< flip find kvlst) ["Tag", "Description", "Comment", "Reference"]
    case xs of
      [ t, d, c, r ] ->  
        return (MetaInfo t d c r)
      _ -> left "Not a metainfo"

-- | 
getElectronEffData :: (Monad m) => [ (T.Text,PYaml)] -> ErrorContextT m ElectronEffData
getElectronEffData kvlst = withContext "ElectronEffData" $ do
    nm <- (eitherText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (eitherObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    return (ElectronEffData nm meta eff)
      
-- | 
getPhotonEffData :: (Monad m) => [ (T.Text,PYaml) ] -> ErrorContextT m PhotonEffData
getPhotonEffData kvlst = withContext "PhotonEffData" $ do 
    nm <- (eitherText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (eitherObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    return (PhotonEffData nm meta eff)

-- | 
getBJetEffData :: (Monad m) => [ (T.Text,PYaml) ] -> ErrorContextT m BJetEffData
getBJetEffData kvlst = withContext "BJetEffData" $ do
    nm <- (eitherText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (eitherObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    rejkvlst <- (eitherObject <=< find "Rejection") kvlst
    rej <- getPTEtaData rejkvlst
    return (BJetEffData nm meta eff rej)

-- | 
getMuonEffData :: (Monad m) => [ (T.Text,PYaml) ] -> ErrorContextT m MuonEffData
getMuonEffData kvlst = withContext "MuonEffData" $ do
    nm <- (eitherText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (eitherObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    return (MuonEffData nm meta eff)
  
-- | 
getJetEffData :: (Monad m) => [ (T.Text,PYaml) ] -> ErrorContextT m JetEffData
getJetEffData kvlst = withContext "JetEffData" $ do
    nm <- (eitherText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (eitherObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    return (JetEffData nm meta eff)

-- |
getTauEffData :: (Monad m) => [(T.Text,PYaml)] -> ErrorContextT m TauEffData
getTauEffData kvlst = withContext "TauEffData" $ do 
    nm <- (eitherText <=< find "Name") kvlst
    tagmtd <- (eitherText <=< find "TaggingMethod") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (eitherObject <=< find "Efficiency") kvlst
    eff <- getTauEffDetail effkvlst
    return TauEffData 
           { tauName = nm
           , tauMetaInfo = meta
           , tauTagMethod = tagmtd 
           , tauEfficiency = eff
           }

-- | 
getTauEffDetail :: (Monad m) => [ (T.Text,PYaml) ] -> ErrorContextT m TauEffDetail
getTauEffDetail kvlst = withContext "TauEffDatail" $ do 
    typ <- (eitherText <=< find "Type") kvlst
    if | typ == "Tau1or3Prong" -> getTau1or3Prong kvlst
       | typ == "TauCombined" -> getTauCombined kvlst

-- | 
getTau1or3Prong :: (Monad m) => [ (T.Text,PYaml) ] -> ErrorContextT m TauEffDetail
getTau1or3Prong kvlst = withContext "Tau1or3Prong" $ do
    eff1 <- (getPTEtaData <=< eitherObject <=< find "Efficiency1Prong") kvlst
    rej1 <- (getPTEtaData <=< eitherObject <=< find "Rejection1Prong") kvlst
    eff3 <- (getPTEtaData <=< eitherObject <=< find "Efficiency3Prong") kvlst
    rej3 <- (getPTEtaData <=< eitherObject <=< find "Rejection3Prong") kvlst
    return (Tau1or3Prong eff1 rej1 eff3 rej3)

-- |
getTauCombined :: (Monad m) => [ (T.Text,PYaml) ] -> ErrorContextT m TauEffDetail
getTauCombined kvlst = withContext "TauCombined" $ do 
    eff <- (getPTEtaData <=< eitherObject <=< find "Efficiency") kvlst
    rej <- (getPTEtaData <=< eitherObject <=< find "Rejection") kvlst
    return (TauCombined eff rej) 


-- | 
getTrackEffData :: (Monad m) => [ (T.Text,PYaml) ] -> ErrorContextT m TrackEffData
getTrackEffData kvlst = withContext "TrackEffData" $ do
    nm <- (eitherText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (eitherObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    return (TrackEffData nm meta eff)

-- |
getPTThresholds :: (Monad m) => [(T.Text,PYaml)] -> ErrorContextT m PTThresholds
getPTThresholds kvlst = withContext "PTThresholds" $ do
    nm <- (eitherText <=< find "Name") kvlst
    xs <- mapM (eitherNum <=< flip find kvlst) 
            [ "MuPTMIN", "ElePTMIN", "PhoPTMIN", "JetPTMIN"
            , "BJetPTMIN", "TrkPTMIN", "TauPTMIN" ] 
    case xs of 
      [ m, e, p, j, b, tr, ta ] ->
        return PTThresholds 
               { pTThreName = nm, muPTMin = m, elePTMin = e, phoPTMin = p
               , jetPTMin = j, bJetPTMin = b, trkPTMin = tr, tauPTMin = ta } 
      _ -> left "Not a PTThresholds"

getSmearData :: (Monad m) => [ (T.Text,PYaml) ] -> ErrorContextT m (SmearData a)
getSmearData kvlst = withContext "getSmearData" $ do
    nm <- (eitherText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    smkvlst <- (eitherObject <=< find "Smearing") kvlst
    sm <- getPTEtaData smkvlst
    return (SmearData nm meta sm)

getRangeData :: (Monad m) => [ (T.Text,PYaml) ] -> ErrorContextT m Range
getRangeData kvlst = withContext "getRangeData" $ do 
    nm <- (eitherText <=< find "Name") kvlst
    ptrng_pre <- find "PTRange" kvlst
    ptrng <- case ptrng_pre of
               PYText "Full" -> return Nothing
               PYList lst -> 
                 case lst of 
                   p1:p2:[] -> liftM Just ((,) <$> eitherNum p1 <*> eitherNum p2)
                   _ -> left "PT1: Not two element list"
               _ -> left "PTRange parse error"
    etrng_pre <- find "EtaRange" kvlst
    etrng <- case etrng_pre of
               PYText "Full" -> return Nothing
               PYList lst -> 
                 case lst of
                   e1:e2:[] -> liftM Just ((,) <$> eitherNum e1 <*> eitherNum e2)
                   _ -> left "ET1: Not two element list"
               _ -> left "EtaRange parse error"
    return Range { rangeName=nm, rangePT=ptrng, rangeEta=etrng } 

--------------
-- import   --
--------------

importData :: (MonadIO m) => 
              ([(T.Text, PYaml)] -> ErrorContextT m a)
           -> FilePath
           -> Either Import a 
           -> ErrorContextT m a
importData _ _ (Right x) = return x 
importData f rdir (Left (Import n)) = 
    let fname = rdir </> T.unpack n <.> "yaml"
    in withContext ("Importing " ++ fname) $ do
      b <- liftIO $ doesFileExist fname
      if not b 
        then left ("File " ++ fname ++ " doesn't exist" )
        else do 
	  r <- liftIO (parseFile fname)
	  case r of
	    Left err -> left err
	    Right (PYObject kvlst) -> f kvlst
	    Right _ -> left ("File content of " ++ fname ++ " is not an object.")

importRangeDescription :: (MonadIO m) => 
                          FilePath        
                       -> RangeDescription ImportList 
                       -> ErrorContextT m (RangeDescription [])
importRangeDescription rdir RangeDescription {..} = 
    RangeDescription <$> (traverse (importData getRangeData rdir) . unImportList) ranges

importIdentificationDescription :: (MonadIO m) => 
                                   FilePath 
                                -> IdentificationDescription ImportList
                                -> ErrorContextT m (IdentificationDescription [])
importIdentificationDescription rdir IdentificationDescription {..} = 
    IdentificationDescription 
    <$> (traverse (importData getElectronEffData rdir) . unImportList) electron
    <*> (traverse (importData getPhotonEffData rdir) . unImportList) photon
    <*> (traverse (importData getBJetEffData rdir) . unImportList) bJet
    <*> (traverse (importData getMuonEffData rdir) . unImportList) muon
    <*> (traverse (importData getJetEffData rdir) . unImportList) jet
    <*> (traverse (importData getTauEffData rdir) . unImportList) tau
    <*> maybe (return Nothing) (liftA Just . (traverse (importData getTrackEffData rdir) . unImportList)) track
    <*> (traverse (importData getPTThresholds rdir) . unImportList) ptThresholds

importSmearingDescription :: (MonadIO m) => 
                             FilePath 
                          -> SmearingDescription ImportList
                          -> ErrorContextT m (SmearingDescription [])
importSmearingDescription rdir SmearingDescription {..} = do
    SmearingDescription 
    <$> (traverse (importData getSmearData rdir) . unImportList) smearElectron
    <*> (traverse (importData getSmearData rdir) . unImportList) smearMuon
    <*> (traverse (importData getSmearData rdir) . unImportList) smearPhoton
    <*> (traverse (importData getSmearData rdir) . unImportList) smearJet
    <*> (traverse (importData getSmearData rdir) . unImportList) smearTrack
    <*> (traverse (importData getSmearData rdir) . unImportList) smearTau
    <*> (traverse (importData getSmearData rdir) . unImportList) smearMET


importDetectorDescription :: (MonadIO m) => 
                             FilePath
                          -> DetectorDescription ImportList
                          -> ErrorContextT m (DetectorDescription [])
importDetectorDescription rdir dd@DetectorDescription {..} = 
    withContext "Importing files in DetectorDescription" $ do 
      rd  <- importRangeDescription rdir detectorRange
      idd <- importIdentificationDescription rdir detectorIdentification
      sd  <- importSmearingDescription rdir detectorSmearing 
      return dd { detectorRange = rd
                , detectorIdentification = idd 
                , detectorSmearing = sd 
                }
