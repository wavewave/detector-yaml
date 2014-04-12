{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Detector.Parser where

import           Control.Applicative
import           Control.Monad ((<=<))
import           Control.Monad.Morph
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
-- import           Data.Bifunctor
import           Data.Functor.Identity
import qualified Data.HashMap.Strict as HM
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
                       -> EitherT String m (DetectorDescription ImportList) 
getDetectorDescription kvlst = do
    xs <- mapM (eitherText <=< flip find kvlst) 
            ["Name", "Description", "Reference"
            , "Comment", "ValidationInfo"]
    case xs of
      [ nm, dsc, ref, cmt, vinfo] -> do
        idd <- (getIdentificationDescription <=< eitherObject <=< find "Identification") kvlst
        sm <- (getSmearingDescription <=< eitherObject <=< find "Smearing") kvlst
        return DetectorDescription 
               { detectorName = nm
               , detectorDescription = dsc
               , detectorReference = ref
               , detectorComment = cmt
               , detectorValidationInfo = vinfo
               , detectorIdentification = idd 
               , detectorSmearing = sm
               }
      _ -> left "Not a Detector Description"

-- | get an object description from parsed YAML object
getIdentificationDescription 
  :: (Monad m) => 
     [(T.Text,PYaml)] 
  -> EitherT String m (IdentificationDescription ImportList) 
getIdentificationDescription kvlst = do
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
        let etk = runIdentity . runEitherT $ do 
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
                (forall n. (Monad n) => [(T.Text, PYaml)] -> EitherT String n a) 
             -> [(T.Text, PYaml)] 
             -> EitherT String m (Either Import a) 
importOrDeal func x = let y = fmap func (getEitherImportOrObj x) 
                      in sequenceA y


-- | get smearing description from parsed YAML object
getSmearingDescription :: (Monad m) => 
                          [(T.Text,PYaml)] 
                       -> EitherT String m (SmearingDescription ImportList) 
getSmearingDescription kvlst = do
    xs <- mapM (eitherList <=< flip find kvlst) [ "Electron", "Muon", "Photon", "Jet", "Track", "Tau", "MissingET" ]
    case xs of 
      [ elecObjs, muonObjs, phoObjs, jetObjs, trkObjs, tauObjs, metObjs ] -> do 
        SmearingDescription <$> implst elecObjs <*> implst muonObjs <*> implst phoObjs 
                            <*> implst jetObjs <*> implst trkObjs <*> implst tauObjs <*> implst metObjs
      _ -> (hoistEither . Left) "Not a Smearing Description"
  where implst ys = ImportList <$> (traverse (importOrDeal getSmearData) =<< mapM eitherObject ys)

    
-- |
getEitherImportOrObj :: [(T.Text,PYaml)] 
                     -> Either Import [(T.Text,PYaml)]
getEitherImportOrObj kvlst = 
    let mr = (runIdentity . runEitherT) (eitherText =<< find "Import" kvlst)
    in case mr of
         Right fname -> Left (Import fname)
         Left _ -> Right kvlst

-- | 
getPTEtaData :: (Monad m) => [(T.Text,PYaml)] -> EitherT String m PTEtaData
getPTEtaData kvlst = do
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
       | otherwise -> (EitherT . return . Left) "Not a PTEtaData"

-- | 
getGrid :: (Monad m) => [(T.Text,PYaml)] -> EitherT String m Grid
getGrid kvlst = do
    typ <- (eitherText <=< find "Type") kvlst
    if | typ == "Full" -> GridFull <$> get2DList "Data" kvlst
       | typ == "Const" -> GridConst <$> (eitherNum <=< find "Data") kvlst
       | otherwise -> (EitherT . return . Left) "Not a Grid"
    


-- | 
get1DList :: (Monad m) => T.Text -> [(T.Text,PYaml)] -> EitherT String m [Scientific] 
get1DList key = (mapM eitherNum <=< eitherList <=< find key) 

-- | 
get2DList :: (Monad m) => T.Text -> [(T.Text,PYaml)] -> EitherT String m [[Scientific]] 
get2DList key kvlst = do
    lst1 :: [ PYaml ] <- (eitherList <=< find key) kvlst 
    lstoflst :: [ [ PYaml ] ] <- mapM eitherList lst1 
    mapM (mapM eitherNum) lstoflst 


-- | 
getInterpolation :: (Monad m) => [(T.Text,PYaml)] -> EitherT String m Interpolation
getInterpolation kvlst = do
    typ <- (eitherText <=< find "Type") kvlst
    if typ == "Constant" 
      then do 
        v <- (eitherNum <=< find "Value") kvlst
        return (IPConstant v) 
      else do 
        lst <- (eitherList <=< find "EtaBinContent") kvlst 
        lst2 <- mapM (getFuncBin <=< eitherObject) lst
        etabound <- (eitherNum <=< find "EtaBound") kvlst
        if | typ == "PredefinedMode1" -> return (IPPredefinedMode1 lst2 etabound)
           | typ == "PredefinedMode2" -> return (IPPredefinedMode2 lst2 etabound)
           | typ == "PredefinedMode3" -> return (IPPredefinedMode3 lst2 etabound)
           | otherwise -> (EitherT . return . Left) "Not an interpolation"
      

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
getIntNum _ = (EitherT . return . Left) "Not an (Int,Number)"





getMetaInfo :: (Monad m) => [(T.Text, PYaml)] -> EitherT String m MetaInfo
getMetaInfo kvlst = do
    xs <- mapM (eitherText <=< flip find kvlst) ["Tag", "Description", "Comment", "Reference"]
    case xs of
      [ t, d, c, r ] ->  
        return (MetaInfo t d c r)
      _ -> (EitherT . return . Left) "Not a metainfo"

-- | 
getElectronEffData :: (Monad m) => [ (T.Text,PYaml)] -> EitherT String m ElectronEffData
getElectronEffData kvlst = do
    nm <- (eitherText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (eitherObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    return (ElectronEffData nm meta eff)
      
-- | 
getPhotonEffData :: (Monad m) => [ (T.Text,PYaml) ] -> EitherT String m PhotonEffData
getPhotonEffData kvlst = do 
    nm <- (eitherText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (eitherObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    return (PhotonEffData nm meta eff)

-- | 
getBJetEffData :: (Monad m) => [ (T.Text,PYaml) ] -> EitherT String m BJetEffData
getBJetEffData kvlst = do
    nm <- (eitherText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (eitherObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    rejkvlst <- (eitherObject <=< find "Rejection") kvlst
    rej <- getPTEtaData rejkvlst
    return (BJetEffData nm meta eff rej)

-- | 
getMuonEffData :: (Monad m) => [ (T.Text,PYaml) ] -> EitherT String m MuonEffData
getMuonEffData kvlst = do
    nm <- (eitherText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (eitherObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    return (MuonEffData nm meta eff)
  
-- | 
getJetEffData :: (Monad m) => [ (T.Text,PYaml) ] -> EitherT String m JetEffData
getJetEffData kvlst = do
    nm <- (eitherText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (eitherObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    return (JetEffData nm meta eff)

-- |
getTauEffData :: (Monad m) => [(T.Text,PYaml)] -> EitherT String m TauEffData
getTauEffData kvlst = do 
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
getTauEffDetail :: (Monad m) => [ (T.Text,PYaml) ] -> EitherT String m TauEffDetail
getTauEffDetail kvlst = do 
    typ <- (eitherText <=< find "Type") kvlst
    if | typ == "Tau1or3Prong" -> getTau1or3Prong kvlst
       | typ == "TauCombined" -> getTauCombined kvlst

-- | 
getTau1or3Prong :: (Monad m) => [ (T.Text,PYaml) ] -> EitherT String m TauEffDetail
getTau1or3Prong kvlst = do
    eff1 <- (getPTEtaData <=< eitherObject <=< find "Efficiency1Prong") kvlst
    rej1 <- (getPTEtaData <=< eitherObject <=< find "Rejection1Prong") kvlst
    eff3 <- (getPTEtaData <=< eitherObject <=< find "Efficiency3Prong") kvlst
    rej3 <- (getPTEtaData <=< eitherObject <=< find "Rejection3Prong") kvlst
    return (Tau1or3Prong eff1 rej1 eff3 rej3)

-- |
getTauCombined :: (Monad m) => [ (T.Text,PYaml) ] -> EitherT String m TauEffDetail
getTauCombined kvlst = do 
    eff <- (getPTEtaData <=< eitherObject <=< find "Efficiency") kvlst
    rej <- (getPTEtaData <=< eitherObject <=< find "Rejection") kvlst
    return (TauCombined eff rej) 


-- | 
getTrackEffData :: (Monad m) => [ (T.Text,PYaml) ] -> EitherT String m TrackEffData
getTrackEffData kvlst = do
    nm <- (eitherText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    effkvlst <- (eitherObject <=< find "Efficiency") kvlst
    eff <- getPTEtaData effkvlst
    return (TrackEffData nm meta eff)

-- |
getPTThresholds :: (Monad m) => [(T.Text,PYaml)] -> EitherT String m PTThresholds
getPTThresholds kvlst = do
    nm <- (eitherText <=< find "Name") kvlst
    xs <- mapM (eitherNum <=< flip find kvlst) 
            [ "MuPTMIN", "ElePTMIN", "PhoPTMIN", "JetPTMIN"
            , "BJetPTMIN", "TrkPTMIN", "TauPTMIN" ] 
    case xs of 
      [ m, e, p, j, b, tr, ta ] ->
        return PTThresholds 
               { pTThreName = nm, muPTMin = m, elePTMin = e, phoPTMin = p
               , jetPTMin = j, bJetPTMin = b, trkPTMin = tr, tauPTMin = ta } 
      _ -> (EitherT . return . Left) "Not a PTThresholds"

getSmearData :: (Monad m) => [ (T.Text,PYaml) ] -> EitherT String m (SmearData a)
getSmearData kvlst = do
    nm <- (eitherText <=< find "Name") kvlst
    meta <- getMetaInfo kvlst
    smkvlst <- (eitherObject <=< find "Smearing") kvlst
    sm <- getPTEtaData smkvlst
    return (SmearData nm meta sm)

--------------
-- import   --
--------------

importData :: (MonadIO m) => 
              (forall n. (Monad n) => [(T.Text, PYaml)] -> EitherT String n a)  
           -> FilePath
           -> Either Import a 
           -> EitherT String m a
importData _ _ (Right x) = return x 
importData f rdir (Left (Import n)) = do
    let fname = rdir </> T.unpack n <.> "yaml"
    r <- liftIO (parseFile fname)
    case r of
      Left err -> (EitherT . return . Left) err
      Right (PYObject kvlst) -> f kvlst
            -- (bimap (const ("parse " ++ fname ++ " failed")) id (f kvlst))
      Right _ -> (EitherT . return . Left) 
                    ("File content of " ++ fname ++ " is not an object.")

importIdentificationDescription 
    :: (MonadIO m) => 
       FilePath 
    -> IdentificationDescription ImportList
    -> EitherT String m (IdentificationDescription [])
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

importSmearingDescription 
    :: (MonadIO m) => 
       FilePath 
    -> SmearingDescription ImportList
    -> EitherT String m (SmearingDescription [])
importSmearingDescription rdir SmearingDescription {..} = do
    SmearingDescription 
    <$> (traverse (importData getSmearData rdir) . unImportList) smearElectron
    <*> (traverse (importData getSmearData rdir) . unImportList) smearMuon
    <*> (traverse (importData getSmearData rdir) . unImportList) smearPhoton
    <*> (traverse (importData getSmearData rdir) . unImportList) smearJet
    <*> (traverse (importData getSmearData rdir) . unImportList) smearTrack
    <*> (traverse (importData getSmearData rdir) . unImportList) smearTau
    <*> (traverse (importData getSmearData rdir) . unImportList) smearMET


importDetectorDescription 
    :: (MonadIO m) => 
       FilePath
    -> DetectorDescription ImportList
    -> EitherT String m (DetectorDescription [])
importDetectorDescription rdir dd@DetectorDescription {..} = do 
    idd <- importIdentificationDescription rdir detectorIdentification
    sd <- importSmearingDescription rdir detectorSmearing 
    return dd { detectorIdentification = idd 
              , detectorSmearing = sd 
              }
