{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Detector.Type where

import Data.Monoid ((<>))
import Data.Scientific
import Data.Text.Lazy (Text, lines)
-- 
import YAML.Builder
-- 
import Prelude hiding (lines)

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
  makeYaml n TrackEffData {..} = 
    YObject $ [ ("Name", mkString (n+defIndent) trackName) ]
              <> mkMetaInfoPairs (n+defIndent) trackMetaInfo
              <> [ ("Efficiency", makeYaml (n+defIndent) trackEfficiency) ]
             

mkImport :: Int -> Import -> YamlValue
mkImport n Import {..} = 
  YObject $ [ ("Import", mkString (n+defIndent) fileName) ] 

mkString :: Int -> Text -> YamlValue
mkString n txt = 
  if length (lines txt) <= 1 
    then (YPrim . YString) txt
    else (YPrim . makeLiteralBlock n) txt
 

mkMetaInfoPairs :: Int -> MetaInfo -> [ (Text, YamlValue) ]
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
  makeYaml _n PTThresholds {..} = 
    YObject $ [ ( "MuPTMIN", (YPrim . YNumber) muPTMin )  
              , ( "ElePTMIN", (YPrim . YNumber) elePTMin )
              , ( "PhoPTMIN", (YPrim . YNumber) phoPTMin )
              , ( "JetPTMIN", (YPrim . YNumber) jetPTMin )
              , ( "BJetPTMIN", (YPrim . YNumber) bJetPTMin ) 
              , ( "TrkPTMIN", (YPrim . YNumber) trkPTMin ) 
              , ( "TauPTMIN", (YPrim . YNumber) tauPTMin ) 
              ]

instance MakeYaml DetectorDescription where
  makeYaml n DetectorDescription {..} = 
    YObject $ [ ( "Name", mkString (n+defIndent) detectorName )
              , ( "Class", mkString (n+defIndent) "TopLevel" )
              , ( "Description", mkString (n+defIndent) detectorDescription)
              , ( "Reference", mkString (n+defIndent) detectorReference)
              , ( "Comment", mkString (n+defIndent) detectorComment )
              , ( "ValidationInfo", mkString (n+defIndent) detectorValidationInfo )
              , ( "Object", makeYaml (n+defIndent) detectorObject ) 
              ]

instance MakeYaml ObjectDescription where
  makeYaml n ObjectDescription {..} = 
    YObject $ [ ( "Electron", importOrEmbed (n+defIndent) electron)  
              , ( "Photon", importOrEmbed (n+defIndent) photon) 
              , ( "BJet", importOrEmbed (n+defIndent) bJet)
              , ( "Muon", importOrEmbed (n+defIndent) muon) 
              , ( "Jet", importOrEmbed (n+defIndent) jet)
              , ( "Tau", importOrEmbed (n+defIndent) tau) ] 
              <> maybe [] (\trk -> [("Track", importOrEmbed (n+defIndent) trk)]) track
              <> [ ( "PTThresholds", importOrEmbed (n+defIndent) ptThresholds) ]
              
