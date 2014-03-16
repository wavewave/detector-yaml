{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Detector where

import Data.Monoid ((<>))
import Data.Scientific
import Data.Text.Lazy (Text)
import YAML.Builder

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
                    , track :: Maybe (Either Import TrackEffData)
                    , ptThresholds :: Either Import PTThresholds }


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
                    , tauEfficiency :: TauEffDetail
                    } 

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

instance Nameable PTThresholds where
  name = pTThreName 


data TrackEffData = TrackEffData 
                       { trackName :: Text 
                       , trackMetaInfo :: MetaInfo 
                       , trackEfficiency :: PTEtaData }

instance Nameable TrackEffData where
  name = trackName 

instance MakeYaml TrackEffData where
  makeYaml TrackEffData {..} = 
    YObject $ [ ("Name", mkString trackName) ]
              <> mkMetaInfoPairs trackMetaInfo
              <> [ ("Efficiency", makeYaml trackEfficiency) ]
             

mkImport :: Import -> YamlValue
mkImport Import {..} = 
  YObject $ [ ("Import", mkString fileName) ] 

mkString :: Text -> YamlValue
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
              <> [ ("TaggingMethod", mkString tauTagMethod) ] 
              <> [ ("Efficiency", makeYaml tauEfficiency) ]

instance MakeYaml TauEffDetail where
  makeYaml Tau1or3Prong {..} = 
    YObject $ [ ("Type", mkString "Tau1or3Prong")
              , ("Efficiency1Prong", makeYaml tau1ProngEff)
              , ("Rejection1Prong" , makeYaml tau1ProngRej)
              , ("Efficiency3Prong", makeYaml tau3ProngEff)
              , ("Rejection3Prong" , makeYaml tau3ProngRej)
              ]
  makeYaml TauCombined {..} = 
    YObject $ [ ("Type", mkString "TauCombined")
              , ("Efficiency", makeYaml tauCombEff)
              , ("Rejection" , makeYaml tauCombRej)
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
              , ( "Tau", importOrEmbed tau ) ] 
              <> maybe [] (\trk -> [("Track", importOrEmbed trk)]) track
              <> [ ( "PTThresholds", importOrEmbed ptThresholds ) ]
              
