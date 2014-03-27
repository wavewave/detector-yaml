{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Detector.Type where

import Control.Applicative
import Data.Foldable
-- import Data.Functor
import Data.Functor.Identity
import Data.Monoid ((<>))
import Data.Scientific
import Data.Text (Text)
import Data.Traversable
import qualified Data.Text.Lazy as L
-- 
import YAML.Builder
-- 
import Prelude hiding (lines)

deriving instance Foldable (Either e)

deriving instance Traversable (Either e)

class Nameable a where
  name :: a -> Text

data DetectorDescription m = 
  DetectorDescription { detectorName :: Text
                      , detectorDescription :: Text
                      , detectorReference :: Text
                      , detectorComment :: Text
                      , detectorValidationInfo :: Text 
                      , detectorObject :: ObjectDescription m
                      }

deriving instance Show (DetectorDescription (Either Import))
deriving instance Show (DetectorDescription Identity)

instance (Show a) => Show (Identity a) where
  show x = show (runIdentity x)

instance Nameable (DetectorDescription m) where
  name = detectorName  

data ObjectDescription m = 
  ObjectDescription 
  { electron :: m ElectronEffData 
  , photon :: m PhotonEffData 
  , bJet :: m BJetEffData 
  , muon :: m MuonEffData
  , jet :: m JetEffData 
  , tau :: m TauEffData 
  , track :: Maybe (m TrackEffData) 
  , ptThresholds :: m PTThresholds
  }

deriving instance Show (ObjectDescription (Either Import))
deriving instance Show (ObjectDescription Identity)

data Import = Import { fileName :: Text }
            deriving (Show)

data MetaInfo = MetaInfo { tag :: Text
                         , description :: Text
                         , comment :: Text 
                         , reference :: Text } 

              deriving (Show)


data Grid = GridFull { gridData :: [ [ Scientific ] ] 
                                  }
          | GridConst { gridConst :: Scientific } 
          deriving (Show)

data PTEtaData = PTEtaGrid 
                   { ptBins :: [Scientific]
                   , etaBins :: [Scientific]
                   , grid :: Grid
                   } 
                | PTEtaInterpolation
                   { interpolationFunction :: Text
                   }
               deriving (Show)

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
             

mkImport :: Int -> Import -> YamlValue
mkImport n Import {..} = 
  YObject $ [ ("Import", mkString (n+defIndent) fileName) ] 

mkMetaInfoPairs :: Int -> MetaInfo -> [ (L.Text, YamlValue) ]
mkMetaInfoPairs n MetaInfo {..} = 
  [ ("Tag" , mkString n tag)
  , ("Description", mkString n description) 
  , ("Comment", mkString n comment )
  , ("Reference", mkString n reference ) ]



importOrEmbed :: (MakeYaml a) => 
                 Int 
              -> Either Import a 
              -> YamlValue
importOrEmbed n = either (mkImport n) (makeYaml n)

importOrEmbedF :: (MakeYaml a, Applicative f) => 
                 Int 
              -> Either Import (f a) 
              -> f YamlValue
importOrEmbedF n = fmap (importOrEmbed n) . sequenceA



instance MakeYaml Grid where
  makeYaml n GridFull {..} = 
    YObject $ [ ("Type", mkString (n+defIndent) "Full")
              , ("Data", mkWrap (map mkInline gridData))
              ]
  makeYaml n GridConst {..} = 
    YObject $ [ ("Type", mkString (n+defIndent) "Const")
              , ("Data", (YPrim . YNumber) gridConst)
              ]

instance MakeYaml ElectronEffData where
  makeYaml n ElectronEffData {..} = 
    YObject $ [ ("Name", mkString (n+defIndent) eleName) ]
              <> mkMetaInfoPairs (n+defIndent) eleMetaInfo 
              <> [ ("Efficiency", makeYaml (n+defIndent) eleEfficiency) ]

instance MakeYaml PTEtaData where 
  makeYaml n PTEtaGrid {..} = 
    YObject $ [ ("Type", mkString (n+defIndent) "Grid" )
              , ("PtBins", mkInline ptBins)
              , ("EtaBins", mkInline etaBins)
              , ("Grid", makeYaml (n+defIndent) grid ) ]
  makeYaml n PTEtaInterpolation {..} =
    YObject $ [ ("Type", mkString (n+defIndent) "Interpolation")
              , ("Function", mkString (n+defIndent) interpolationFunction) ] 


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

instance MakeYaml (DetectorDescription ImportList) where
  makeYaml n DetectorDescription {..} = 
    YObject $ [ ( "Name", mkString (n+defIndent) detectorName )
              , ( "Class", mkString (n+defIndent) "TopLevel" )
              , ( "Description", mkString (n+defIndent) detectorDescription)
              , ( "Reference", mkString (n+defIndent) detectorReference)
              , ( "Comment", mkString (n+defIndent) detectorComment )
              , ( "ValidationInfo", mkString (n+defIndent) detectorValidationInfo )
              , ( "Object", makeYaml (n+defIndent) detectorObject ) 
              ]

instance MakeYaml (ObjectDescription (Either Import)) where
  makeYaml n ObjectDescription {..} = 
    YObject $ [ ( "Electron", importOrEmbed (n+defIndent) electron)  
              , ( "Photon", importOrEmbed (n+defIndent) photon) 
              , ( "BJet", importOrEmbed (n+defIndent) bJet)
              , ( "Muon", importOrEmbed (n+defIndent) muon) 
              , ( "Jet", importOrEmbed (n+defIndent) jet)
              , ( "Tau", importOrEmbed (n+defIndent) tau) ] 
              <> maybe [] (\trk -> [("Track", importOrEmbed (n+defIndent) trk)]) track
              <> [ ( "PTThresholds", importOrEmbed (n+defIndent) ptThresholds) ]

newtype ImportList a = ImportList { unImportList :: [Either Import a] }

deriving instance Functor ImportList
 
instance MakeYaml (ObjectDescription ImportList) where
  makeYaml n ObjectDescription {..} = 
      YObject $ [ ( "Electron", importOrEmbed' (n+defIndent) electron)  
                , ( "Photon", importOrEmbed' (n+defIndent) photon) 
                , ( "BJet", importOrEmbed' (n+defIndent) bJet)
                , ( "Muon", importOrEmbed' (n+defIndent) muon) 
                , ( "Jet", importOrEmbed' (n+defIndent) jet)
                , ( "Tau", importOrEmbed' (n+defIndent) tau) ] 
                <> maybe [] (\trk -> [("Track", importOrEmbed' (n+defIndent) trk)]) track
                <> [ ( "PTThresholds", importOrEmbed' (n+defIndent) ptThresholds) ]
    where importOrEmbed' m = YIArray . fmap (importOrEmbed m) . unImportList
