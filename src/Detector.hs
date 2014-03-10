{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Detector where

import Data.Monoid ((<>))
import Data.Scientific
import Data.Text.Lazy (Text(..))
import YAML

data DetectorDescription = 
  DetectorDescription { detectorName :: Text
                      , detectorDescription :: Text
                      , detectorReference :: Text
                      , detectorComment :: Text
                      , detectorValidationInfo :: Text 
                      , detectorObject :: ObjectDescription
                      }

instance Nameable DetectorDescription where
  name = detectorName  

data Import = Import { fileName :: Text }

data ObjectDescription = 
  ObjectDescription { electron :: Either Import ElectronEffData 
                    , photon :: Either Import PhotonEffData 
                    , bJet :: Either Import BJetEffData 
                    , muon :: Either Import MuonEffData 
                    , jet :: Either Import JetEffData 
                    , tau :: Either Import TauEffData
                    , ptThresholds :: PTThresholds }


data MetaInfo = MetaInfo { tag :: Text
                         , description :: Text
                         , comment :: Text 
                         , reference :: Text } 

data Grid = GridFull { gridData :: [ [ Scientific ] ] 
                                  }
          | GridConst { gridConst :: Scientific } 

data PTEtaData = PTEtaGrid 
                   { ptBins :: [Scientific]
                   , etaBins :: [Scientific]
                   , grid :: Grid
                   } 
               | PTEtaInterpolation
                   { interpolationFunction :: Text
                   }
			  
data ElectronEffData = ElectronEffData
                            { eleName :: Text
                            , eleMetaInfo :: MetaInfo 
                            , eleEfficiency :: PTEtaData
                            }

instance Nameable ElectronEffData where
  name = eleName
			    
data PhotonEffData = PhotonEffData
                          { phoName :: Text
                          , phoMetaInfo :: MetaInfo
                          , phoEfficiency :: PTEtaData }

instance Nameable PhotonEffData where
  name = phoName
                       
data BJetEffData = BJetEffData
                     { bJetName :: Text 
                     , bJetMetaInfo :: MetaInfo
                     , bJetEfficiency :: PTEtaData
                     , bJetRejection :: PTEtaData
                     } 

instance Nameable BJetEffData where
  name = bJetName
 
data MuonEffData = MuonEffData 
                     { muonName :: Text
                     , muonMetaInfo :: MetaInfo
                     , muonEfficiency :: PTEtaData }

instance Nameable MuonEffData where
  name = muonName

data JetEffData = JetEffData 
                       { jetName :: Text 
                       , jetMetaInfo :: MetaInfo 
                       , jetEfficiency :: PTEtaData }

instance Nameable JetEffData where
  name = jetName

data TauEffData = TauEffData
                    { tauName :: Text
                    , tauMetaInfo :: MetaInfo
                    , tauTagMethod :: Text
                    , tauEfficiency1Prong :: PTEtaData
                    , tauRejection1Prong :: PTEtaData 
                    , tauEfficiency3Prong :: PTEtaData
                    , tauRejection3Prong :: PTEtaData
                    } 

instance Nameable TauEffData where
  name = tauName

data PTThresholds = PTThresholds 
                      { muPTMin :: Scientific
                      , elePTMin :: Scientific
                      , phoPTMin :: Scientific
                      , jetPTMin :: Scientific
                      , bJetPTMin :: Scientific
                      , trkPTMin :: Scientific
                      , tauPTMin :: Scientific
                      }


mkImport :: Import -> YamlValue
mkImport Import {..} = 
  YObject $ [ ("Import", mkString fileName) ] 


mkString = YPrim . YString

mkMetaInfoPairs :: MetaInfo -> [ (Text, YamlValue) ]
mkMetaInfoPairs MetaInfo {..} = 
  [ ("Tag" , mkString tag)
  , ("Description", mkString description) 
  , ("Comment", mkString comment )
  , ("Reference", mkString reference ) ]

importOrEmbed :: (MakeYaml a) => Either Import a -> YamlValue
importOrEmbed = either mkImport makeYaml


instance MakeYaml Grid where
  makeYaml GridFull {..} = 
    YObject $ [ ("Type", mkString "Full")
              , ("Data", mkWrap (map mkInline gridData))
              ]
  makeYaml GridConst {..} = 
    YObject $ [ ("Type", mkString "Const")
              , ("Data", (YPrim . YNumber) gridConst)
              ]

instance MakeYaml ElectronEffData where
  makeYaml ElectronEffData {..} = 
    YObject $ [ ("Name", mkString eleName) ]
              <> mkMetaInfoPairs eleMetaInfo 
              <> [ ("Efficiency", makeYaml eleEfficiency) ]

instance MakeYaml PTEtaData where 
  makeYaml PTEtaGrid {..} = 
    YObject $ [ ("Type", mkString "Grid" )
              , ("PtBins", mkInline ptBins)
              , ("EtaBins", mkInline etaBins)
              , ("Grid", makeYaml grid ) ]
  makeYaml PTEtaInterpolation {..} =
    YObject $ [ ("Type", mkString "Interpolation")
              , ("Function", mkString interpolationFunction ) ] 


instance MakeYaml PhotonEffData where
  makeYaml PhotonEffData {..} = 
    YObject $ [ ("Name", mkString phoName) ] 
              <> mkMetaInfoPairs phoMetaInfo 
              <> [ ("Efficiency", makeYaml phoEfficiency ) ] 


instance MakeYaml BJetEffData where
  makeYaml BJetEffData {..} = 
    YObject $ [ ("Name", mkString bJetName) ]
              <> mkMetaInfoPairs bJetMetaInfo
              <> [ ("Efficiency", makeYaml bJetEfficiency) 
                 , ("Rejection", makeYaml bJetRejection) ]

-- charm rejection

instance MakeYaml MuonEffData where
  makeYaml MuonEffData {..} = 
    YObject $ [ ("Name", mkString muonName ) ]
              <> mkMetaInfoPairs muonMetaInfo
              <> [ ("Efficiency", makeYaml muonEfficiency) ]


instance MakeYaml JetEffData where 
  makeYaml JetEffData {..} = 
    YObject $ [ ( "Name", mkString jetName ) ]
              <> mkMetaInfoPairs jetMetaInfo
              <> [ ("Efficiency", makeYaml jetEfficiency) ] 

instance MakeYaml TauEffData where
  makeYaml TauEffData {..} = 
    YObject $ [ ("Name", mkString tauName) ] 
              <> mkMetaInfoPairs tauMetaInfo 
              <> [ ("TaggingMethod", mkString tauTagMethod) 
                 , ("Efficiency1Prong", makeYaml tauEfficiency1Prong)
                 , ("Rejection1Prong", makeYaml tauRejection1Prong)
                 , ("Efficiency3Prong", makeYaml tauEfficiency3Prong)
                 , ("Rejection3Prong", makeYaml tauRejection3Prong)
                 ] 

instance MakeYaml PTThresholds where
  makeYaml PTThresholds {..} = 
    YObject $ [ ( "MuPTMIN", (YPrim . YNumber) muPTMin )  
              , ( "ElePTMIN", (YPrim . YNumber) elePTMin )
              , ( "PhoPTMIN", (YPrim . YNumber) phoPTMin )
              , ( "JetPTMIN", (YPrim . YNumber) jetPTMin )
              , ( "BJetPTMIN", (YPrim . YNumber) bJetPTMin ) 
              , ( "TrkPTMIN", (YPrim . YNumber) trkPTMin ) 
              , ( "TauPTMIN", (YPrim . YNumber) tauPTMin ) 
              ]

instance MakeYaml DetectorDescription where
  makeYaml DetectorDescription {..} = 
    YObject $ [ ( "Name", mkString detectorName )
              , ( "Class", mkString "TopLevel" )
              , ( "Description", mkString detectorDescription )
              , ( "Reference", mkString detectorReference )
              , ( "Comment", mkString detectorComment )
              , ( "ValidationInfo", mkString detectorValidationInfo )
              , ( "Object", makeYaml detectorObject ) 
              ]

instance MakeYaml ObjectDescription where
  makeYaml ObjectDescription {..} = 
    YObject $ [ ( "Electron", importOrEmbed electron )  
              , ( "Photon", importOrEmbed photon ) 
              , ( "BJet", importOrEmbed bJet )
              , ( "Muon", importOrEmbed muon ) 
              , ( "Jet", importOrEmbed jet )
              , ( "Tau", importOrEmbed tau )
              , ( "PTThresholds", makeYaml ptThresholds )
              ] 
