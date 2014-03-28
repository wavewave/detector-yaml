{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Detector.Type.Identification where

import           Data.Functor.Identity
import           Data.Monoid ((<>))
import           Data.Scientific
import           Data.Text (Text)
-- 
import           YAML.Builder
--
import           Detector.Type.Common

data IdentificationDescription m = 
  IdentificationDescription 
  { electron :: m ElectronEffData 
  , photon :: m PhotonEffData 
  , bJet :: m BJetEffData 
  , muon :: m MuonEffData
  , jet :: m JetEffData 
  , tau :: m TauEffData 
  , track :: Maybe (m TrackEffData) 
  , ptThresholds :: m PTThresholds
  }

deriving instance Show (IdentificationDescription (Either Import))
deriving instance Show (IdentificationDescription Identity)


instance MakeYaml (IdentificationDescription (Either Import)) where
  makeYaml n IdentificationDescription {..} = 
    YObject $ [ ( "Electron", importOrEmbed (n+defIndent) electron)  
              , ( "Photon", importOrEmbed (n+defIndent) photon) 
              , ( "BJet", importOrEmbed (n+defIndent) bJet)
              , ( "Muon", importOrEmbed (n+defIndent) muon) 
              , ( "Jet", importOrEmbed (n+defIndent) jet)
              , ( "Tau", importOrEmbed (n+defIndent) tau) ] 
              <> maybe [] (\trk -> [("Track", importOrEmbed (n+defIndent) trk)]) track
              <> [ ( "PTThresholds", importOrEmbed (n+defIndent) ptThresholds) ]

 
instance MakeYaml (IdentificationDescription ImportList) where
  makeYaml n IdentificationDescription {..} = 
      YObject $ [ ( "Electron", importOrEmbed' (n+defIndent) electron)  
                , ( "Photon", importOrEmbed' (n+defIndent) photon) 
                , ( "BJet", importOrEmbed' (n+defIndent) bJet)
                , ( "Muon", importOrEmbed' (n+defIndent) muon) 
                , ( "Jet", importOrEmbed' (n+defIndent) jet)
                , ( "Tau", importOrEmbed' (n+defIndent) tau) ] 
                <> maybe [] (\trk -> [("Track", importOrEmbed' (n+defIndent) trk)]) track
                <> [ ( "PTThresholds", importOrEmbed' (n+defIndent) ptThresholds) ]
    where importOrEmbed' m = YIArray . fmap (importOrEmbed m) . unImportList


instance MakeYaml (IdentificationDescription []) where
  makeYaml n IdentificationDescription {..} = 
      YObject $ [ ( "Electron", embed' (n+defIndent) electron)  
                , ( "Photon", embed' (n+defIndent) photon) 
                , ( "BJet", embed' (n+defIndent) bJet)
                , ( "Muon", embed' (n+defIndent) muon) 
                , ( "Jet", embed' (n+defIndent) jet)
                , ( "Tau", embed' (n+defIndent) tau) ] 
                <> maybe [] (\trk -> [("Track", embed' (n+defIndent) trk)]) track
                <> [ ( "PTThresholds", embed' (n+defIndent) ptThresholds) ]
    where embed' m = YIArray . fmap (makeYaml m)

data ElectronEffData = ElectronEffData
                         { eleName :: Text
                         , eleMetaInfo :: MetaInfo 
                         , eleEfficiency :: PTEtaData
                         }
                      deriving (Show)

instance Nameable ElectronEffData where
  name = eleName

data PhotonEffData = PhotonEffData
                          { phoName :: Text
                          , phoMetaInfo :: MetaInfo
                          , phoEfficiency :: PTEtaData }
                   deriving (Show)

instance Nameable PhotonEffData where
  name = phoName
                       
data BJetEffData = BJetEffData
                     { bJetName :: Text 
                     , bJetMetaInfo :: MetaInfo
                     , bJetEfficiency :: PTEtaData
                     , bJetRejection :: PTEtaData
                     } 
                 deriving (Show)

instance Nameable BJetEffData where
  name = bJetName
 
data MuonEffData = MuonEffData 
                     { muonName :: Text
                     , muonMetaInfo :: MetaInfo
                     , muonEfficiency :: PTEtaData }
                 deriving (Show)

instance Nameable MuonEffData where
  name = muonName

data JetEffData = JetEffData 
                       { jetName :: Text 
                       , jetMetaInfo :: MetaInfo 
                       , jetEfficiency :: PTEtaData }
                deriving (Show)

instance Nameable JetEffData where
  name = jetName

data TauEffData = TauEffData
                    { tauName :: Text
                    , tauMetaInfo :: MetaInfo
                    , tauTagMethod :: Text
                    , tauEfficiency :: TauEffDetail
                    } 
                deriving (Show)

data TauEffDetail = Tau1or3Prong
                      { tau1ProngEff :: PTEtaData
                      , tau1ProngRej :: PTEtaData 
                      , tau3ProngEff :: PTEtaData
                      , tau3ProngRej :: PTEtaData
                      } 
                  | TauCombined 
                      { tauCombEff :: PTEtaData
                      , tauCombRej :: PTEtaData
                      } 
                  deriving Show

-- instance Show TauEffDetail where show _ = "TauEffDetail"

instance Nameable TauEffData where
  name = tauName

data PTThresholds = PTThresholds 
                      { pTThreName :: Text
                      , muPTMin :: Scientific
                      , elePTMin :: Scientific
                      , phoPTMin :: Scientific
                      , jetPTMin :: Scientific
                      , bJetPTMin :: Scientific
                      , trkPTMin :: Scientific
                      , tauPTMin :: Scientific
                      }
                  deriving (Show)

instance Nameable PTThresholds where
  name = pTThreName 

data TrackEffData = TrackEffData 
                       { trackName :: Text 
                       , trackMetaInfo :: MetaInfo 
                       , trackEfficiency :: PTEtaData }
                  deriving (Show)

instance Nameable TrackEffData where
  name = trackName 

instance MakeYaml TrackEffData where
  makeYaml n TrackEffData {..} = 
    YObject $ [ ("Name", mkString (n+defIndent) trackName) ]
              <> mkMetaInfoPairs (n+defIndent) trackMetaInfo
              <> [ ("Efficiency", makeYaml (n+defIndent) trackEfficiency) ]
             

instance MakeYaml ElectronEffData where
  makeYaml n ElectronEffData {..} = 
    YObject $ [ ("Name", mkString (n+defIndent) eleName) ]
              <> mkMetaInfoPairs (n+defIndent) eleMetaInfo 
              <> [ ("Efficiency", makeYaml (n+defIndent) eleEfficiency) ]


instance MakeYaml PhotonEffData where
  makeYaml n PhotonEffData {..} = 
    YObject $ [ ("Name", mkString (n+defIndent) phoName) ]
              <> mkMetaInfoPairs (n+defIndent) phoMetaInfo 
              <> [ ("Efficiency", makeYaml (n+defIndent) phoEfficiency ) ]


instance MakeYaml BJetEffData where
  makeYaml n BJetEffData {..} = 
    YObject $ [ ("Name", mkString (n+defIndent) bJetName) ]
              <> mkMetaInfoPairs (n+defIndent) bJetMetaInfo
              <> [ ("Efficiency", makeYaml (n+defIndent) bJetEfficiency) 
                 , ("Rejection", makeYaml (n+defIndent) bJetRejection) ]

-- charm rejection

instance MakeYaml MuonEffData where
  makeYaml n MuonEffData {..} = 
    YObject $ [ ("Name", mkString (n+defIndent) muonName ) ]
              <> mkMetaInfoPairs (n+defIndent) muonMetaInfo
              <> [ ("Efficiency", makeYaml (n+defIndent) muonEfficiency) ]


instance MakeYaml JetEffData where 
  makeYaml n JetEffData {..} = 
    YObject $ [ ( "Name", mkString (n+defIndent) jetName ) ]
              <> mkMetaInfoPairs (n+defIndent) jetMetaInfo
              <> [ ("Efficiency", makeYaml (n+defIndent) jetEfficiency) ] 

instance MakeYaml TauEffData where
  makeYaml n TauEffData {..} = 
    YObject $ [ ("Name", mkString (n+defIndent) tauName) ]
              <> mkMetaInfoPairs (n+defIndent) tauMetaInfo 
              <> [ ("TaggingMethod", mkString (n+defIndent) tauTagMethod) ] 
              <> [ ("Efficiency", makeYaml (n+defIndent) tauEfficiency) ]

instance MakeYaml TauEffDetail where
  makeYaml n Tau1or3Prong {..} = 
    YObject $ [ ("Type", mkString (n+defIndent) "Tau1or3Prong")
              , ("Efficiency1Prong", makeYaml (n+defIndent) tau1ProngEff)
              , ("Rejection1Prong" , makeYaml (n+defIndent) tau1ProngRej)
              , ("Efficiency3Prong", makeYaml (n+defIndent) tau3ProngEff)
              , ("Rejection3Prong" , makeYaml (n+defIndent) tau3ProngRej)
              ]
  makeYaml n TauCombined {..} = 
    YObject $ [ ("Type", mkString (n+defIndent) "TauCombined")
              , ("Efficiency", makeYaml (n+defIndent) tauCombEff)
              , ("Rejection" , makeYaml (n+defIndent) tauCombRej)
              ]
     
instance MakeYaml PTThresholds where
  makeYaml n PTThresholds {..} = 
    YObject $ [ ( "Name", mkString (n+defIndent) pTThreName)
              , ( "MuPTMIN", (YPrim . YNumber) muPTMin )  
              , ( "ElePTMIN", (YPrim . YNumber) elePTMin )
              , ( "PhoPTMIN", (YPrim . YNumber) phoPTMin )
              , ( "JetPTMIN", (YPrim . YNumber) jetPTMin )
              , ( "BJetPTMIN", (YPrim . YNumber) bJetPTMin ) 
              , ( "TrkPTMIN", (YPrim . YNumber) trkPTMin ) 
              , ( "TauPTMIN", (YPrim . YNumber) tauPTMin ) 
              ]


