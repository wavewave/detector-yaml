{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ATLAS where

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
                      , detectorEfficiency :: EfficiencyDescription
                      }


data EfficiencyDescription = 
  EfficiencyDescription { electron :: ElectronEfficiency 
                        , photon :: PhotonEfficiency 
                        , bJet :: BJetEfficiency 
                        , muon :: MuonEfficiency
                        , jet :: JetEfficiency
                        , tau :: TauEfficiency 
                        , ptThresholds :: PTThresholds }

data Import = Import { fileName :: Text }

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

data ElectronEfficiency = ElectronEfficiency { elecEffFile :: Import }
			  
data ElectronEffData = ElectronEffData
                            { eleName :: Text
                            , eleMetaInfo :: MetaInfo 
                            , eleEfficiency :: PTEtaData
                            }
			    
data PhotonEfficiency = PhotonEfficiency { phoEffFile :: Import }

data PhotonEffData = PhotonEffData
                          { phoName :: Text
                          , phoMetaInfo :: MetaInfo
                          , phoEfficiency :: PTEtaData }
                       
data BJetEfficiency = BJetEfficiency { bJetEffFile :: Import }

data BJetEffData = BJetEffData
                     { bJetName :: Text 
                     , bJetMetaInfo :: MetaInfo
                     , bJetEfficiency :: PTEtaData
                     , bJetRejection :: PTEtaData
                     } 

data MuonEfficiency = MuonEfficiency { muonEffFile :: Import }

data MuonEffData = MuonEffData 
                     { muonName :: Text
                     , muonMetaInfo :: MetaInfo
                     , muonEfficiency :: PTEtaData }

data JetEfficiency = JetEfficiency { jetEffFile :: Import }

data JetEffData = JetEffData 
                       { jetName :: Text 
                       , jetMetaInfo :: MetaInfo 
                       , jetEfficiency :: PTEtaData }


data TauEfficiency = TauEfficiency { tauEffFile :: Import }

data TauEffData = TauEffData
                    { tauName :: Text
                    , tauMetaInfo :: MetaInfo
                    , tauTagMethod :: Text
                    , tauEfficiency1Prong :: PTEtaData
                    , tauRejection1Prong :: PTEtaData 
                    , tauEfficiency3Prong :: PTEtaData
                    , tauRejection3Prong :: PTEtaData
                    } 
{-
                       { tauEffPtBins :: [ Scientific ] 
                       , tauEffEtaBins :: [ Scientific ] 
                       , tauEffCutLSing :: Grid
                       , tauEffCutMSing :: Grid
                       , tauEffCutTSing :: Grid
                       , tauEffLikLSing :: Grid
                       , tauEffLikMSing :: Grid
                       , tauEffLikTSing :: Grid
                       , tauEffBdtLSing :: Grid
                       , tauEffBdtMSing :: Grid
                       , tauEffBdtTSing :: Grid
                       , tauEffCutLMult :: Grid
                       , tauEffCutMMult :: Grid
                       , tauEffCutTMult :: Grid
                       , tauEffLikLMult :: Grid
                       , tauEffLikMMult :: Grid
                       , tauEffLikTMult :: Grid
                       , tauEffBdtLMult :: Grid
                       , tauEffBdtMMult :: Grid
                       , tauEffBdtTMult :: Grid
                       , tauRejPtBins :: [ Scientific ] 
                       , tauRejEtaBins :: [ Scientific ] 
                       , tauRejCutLSing :: Grid
                       , tauRejCutMSing :: Grid
                       , tauRejCutTSing :: Grid
                       , tauRejLikLSing :: Grid
                       , tauRejLikMSing :: Grid
                       , tauRejLikTSing :: Grid
                       , tauRejBdtLSing :: Grid
                       , tauRejBdtMSing :: Grid
                       , tauRejBdtTSing :: Grid
                       , tauRejCutLMult :: Grid
                       , tauRejCutMMult :: Grid
                       , tauRejCutTMult :: Grid
                       , tauRejLikLMult :: Grid
                       , tauRejLikMMult :: Grid
                       , tauRejLikTMult :: Grid
                       , tauRejBdtLMult :: Grid
                       , tauRejBdtMMult :: Grid
                       , tauRejBdtTMult :: Grid
                       } 
-}

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

mkElectronEfficiency :: ElectronEfficiency -> YamlValue
mkElectronEfficiency ElectronEfficiency {..} = mkImport elecEffFile

mkPhotonEfficiency :: PhotonEfficiency -> YamlValue
mkPhotonEfficiency PhotonEfficiency {..} = mkImport phoEffFile

mkBJetEfficiency :: BJetEfficiency -> YamlValue
mkBJetEfficiency BJetEfficiency {..} = mkImport bJetEffFile

mkMuonEfficiency :: MuonEfficiency -> YamlValue
mkMuonEfficiency MuonEfficiency {..} = mkImport muonEffFile

mkJetEfficiency :: JetEfficiency -> YamlValue
mkJetEfficiency JetEfficiency {..} = mkImport jetEffFile

mkTauEfficiency :: TauEfficiency -> YamlValue
mkTauEfficiency TauEfficiency {..} = mkImport tauEffFile

mkGrid :: Grid -> YamlValue
mkGrid GridFull {..} = 
  YObject $ [ ("Type", mkString "Full")
            , ("Data", mkWrap (map mkInline gridData))
            ]
mkGrid GridConst {..} = 
  YObject $ [ ("Type", mkString "Const")
            , ("Data", (YPrim . YNumber) gridConst)
            ]



mkString = YPrim . YString

mkMetaInfoPairs :: MetaInfo -> [ (Text, YamlValue) ]
mkMetaInfoPairs MetaInfo {..} = 
  [ ("Tag" , mkString tag)
  , ("Description", mkString description) 
  , ("Comment", mkString comment )
  , ("Reference", mkString reference ) ]


mkElectronEffData :: ElectronEffData -> YamlValue
mkElectronEffData ElectronEffData {..} = 
    YObject $ [ ("Name", mkString eleName) ]
              <> mkMetaInfoPairs eleMetaInfo 
              <> [ ("Efficiency", mkPTEtaData eleEfficiency) ]


mkPTEtaData :: PTEtaData -> YamlValue
mkPTEtaData PTEtaGrid {..} = 
    YObject $ [ ("Type", mkString "Grid" )
              , ("PtBins", mkInline ptBins)
              , ("EtaBins", mkInline etaBins)
              , ("Grid", mkGrid grid ) ]
mkPTEtaData PTEtaInterpolation {..} =
    YObject $ [ ("Type", mkString "Interpolation")
              , ("Function", mkString interpolationFunction ) ] 


mkPhotonEffData :: PhotonEffData -> YamlValue
mkPhotonEffData PhotonEffData {..} = 
    YObject $ [ ("Name", mkString phoName) ] 
              <> mkMetaInfoPairs phoMetaInfo 
              <> [ ("Efficiency", mkPTEtaData phoEfficiency ) ] 


mkBJetEffData :: BJetEffData -> YamlValue
mkBJetEffData BJetEffData {..} = 
    YObject $ [ ("Name", mkString bJetName) ]
              <> mkMetaInfoPairs bJetMetaInfo
              <> [ ("Efficiency", mkPTEtaData bJetEfficiency) 
                 , ("Rejection", mkPTEtaData bJetRejection) ]

-- charm rejection

mkMuonEffData :: MuonEffData -> YamlValue
mkMuonEffData MuonEffData {..} = 
  YObject $ [ ("Name", mkString muonName ) ]
            <> mkMetaInfoPairs muonMetaInfo
            <> [ ("Efficiency", mkPTEtaData muonEfficiency) ]


mkJetEffData :: JetEffData -> YamlValue
mkJetEffData JetEffData {..} = 
  YObject $ [ ( "Name", mkString jetName ) ]
            <> mkMetaInfoPairs jetMetaInfo
            <> [ ("Efficiency", mkPTEtaData jetEfficiency) ] 

mkTauEffData :: TauEffData -> YamlValue
mkTauEffData TauEffData {..} = 
  YObject $ [ ("Name", mkString tauName) ] 
            <> mkMetaInfoPairs tauMetaInfo 
            <> [ ("TaggingMethod", mkString tauTagMethod) 
               , ("Efficiency1Prong", mkPTEtaData tauEfficiency1Prong)
               , ("Rejection1Prong", mkPTEtaData tauRejection1Prong)
               , ("Efficiency3Prong", mkPTEtaData tauEfficiency3Prong)
               , ("Rejection3Prong", mkPTEtaData tauRejection3Prong)
               ] 
{-
  YObject $ [ ( "TauEffPtBins", mkInline tauEffPtBins )
            , ( "TauEffEtaBins", mkInline tauEffEtaBins )
            , ( "TauEffCutLSing", (mkGrid  tauEffCutLSing) )
            , ( "TauEffCutMSing", (mkGrid  tauEffCutMSing) )
            , ( "TauEffCutTSing", (mkGrid  tauEffCutTSing) )
            , ( "TauEffLikLSing", (mkGrid  tauEffLikLSing) )
            , ( "TauEffLikMSing", (mkGrid  tauEffLikMSing) )
            , ( "TauEffLikTSing", (mkGrid  tauEffLikTSing) )
            , ( "TauEffBdtLSing", (mkGrid  tauEffBdtLSing) )
            , ( "TauEffBdtMSing", (mkGrid  tauEffBdtMSing) )
            , ( "TauEffBdtTSing", (mkGrid  tauEffBdtTSing) )
            , ( "TauEffCutLMult", (mkGrid  tauEffCutLMult) )
            , ( "TauEffCutMMult", (mkGrid  tauEffCutMMult) )
            , ( "TauEffCutTMult", (mkGrid  tauEffCutTMult) )
            , ( "TauEffLikLMult", (mkGrid  tauEffLikLMult) )
            , ( "TauEffLikMMult", (mkGrid  tauEffLikMMult) )
            , ( "TauEffLikTMult", (mkGrid  tauEffLikTMult) )
            , ( "TauEffBdtLMult", (mkGrid  tauEffBdtLMult) )
            , ( "TauEffBdtMMult", (mkGrid  tauEffBdtMMult) )
            , ( "TauEffBdtTMult", (mkGrid  tauEffBdtTMult) ) 
            , ( "TauRejPtBins", mkInline tauRejPtBins )
            , ( "TauRejEtaBins", mkInline tauRejEtaBins )
            , ( "TauRejCutLSing", (mkGrid  tauRejCutLSing) )
            , ( "TauRejCutMSing", (mkGrid  tauRejCutMSing) )
            , ( "TauRejCutTSing", (mkGrid  tauRejCutTSing) )
            , ( "TauRejLikLSing", (mkGrid  tauRejLikLSing) )
            , ( "TauRejLikMSing", (mkGrid  tauRejLikMSing) )
            , ( "TauRejLikTSing", (mkGrid  tauRejLikTSing) )
            , ( "TauRejBdtLSing", (mkGrid  tauRejBdtLSing) )
            , ( "TauRejBdtMSing", (mkGrid  tauRejBdtMSing) )
            , ( "TauRejBdtTSing", (mkGrid  tauRejBdtTSing) )
            , ( "TauRejCutLMult", (mkGrid  tauRejCutLMult) )
            , ( "TauRejCutMMult", (mkGrid  tauRejCutMMult) )
            , ( "TauRejCutTMult", (mkGrid  tauRejCutTMult) )
            , ( "TauRejLikLMult", (mkGrid  tauRejLikLMult) )
            , ( "TauRejLikMMult", (mkGrid  tauRejLikMMult) )
            , ( "TauRejLikTMult", (mkGrid  tauRejLikTMult) )
            , ( "TauRejBdtLMult", (mkGrid  tauRejBdtLMult) )
            , ( "TauRejBdtMMult", (mkGrid  tauRejBdtMMult) )
            , ( "TauRejBdtTMult", (mkGrid  tauRejBdtTMult) )
            ] 
-}

mkPTThresholds :: PTThresholds -> YamlValue
mkPTThresholds PTThresholds {..} = 
  YObject $ [ ( "MuPTMIN", (YPrim . YNumber) muPTMin )  
            , ( "ElePTMIN", (YPrim . YNumber) elePTMin )
            , ( "PhoPTMIN", (YPrim . YNumber) phoPTMin )
            , ( "JetPTMIN", (YPrim . YNumber) jetPTMin )
            , ( "BJetPTMIN", (YPrim . YNumber) bJetPTMin ) 
            , ( "TrkPTMIN", (YPrim . YNumber) trkPTMin ) 
            , ( "TauPTMIN", (YPrim . YNumber) tauPTMin ) 
            ]


mkDetector :: DetectorDescription -> YamlValue 
mkDetector DetectorDescription {..} = 
    YObject $ [ ( "Name", mkString detectorName )
              , ( "Description", mkString detectorDescription )
              , ( "Reference", mkString detectorReference )
              , ( "Comment", mkString detectorComment )
              , ( "ValidationInfo", mkString detectorValidationInfo )
              , ( "Efficiency", mkEfficiency detectorEfficiency ) 
              ]

mkEfficiency :: EfficiencyDescription -> YamlValue
mkEfficiency EfficiencyDescription {..} = 
    YObject $ [ ( "Electron", mkElectronEfficiency electron )  
              , ( "Photon", mkPhotonEfficiency photon ) 
              , ( "BJet", mkBJetEfficiency bJet )
              , ( "MuonEfficiency", mkMuonEfficiency muon ) 
              , ( "JetEfficiency", mkJetEfficiency jet )
              , ( "TauEfficiency", mkTauEfficiency tau )
              , ( "PTThresholds", mkPTThresholds ptThresholds )
              ] 

atlasElecEff :: ElectronEfficiency
atlasElecEff = ElectronEfficiency { elecEffFile = Import "Atlas2011_ElecEff.yaml" }

atlasPhoEff :: PhotonEfficiency
atlasPhoEff = PhotonEfficiency { phoEffFile = Import "Atlas2011_PhoEff.yaml" }

atlasBJetEff :: BJetEfficiency
atlasBJetEff = BJetEfficiency { bJetEffFile = Import "Atlas2011_BJetEff.yaml" }

atlasMuonEff :: MuonEfficiency
atlasMuonEff = MuonEfficiency { muonEffFile = Import "Atlas2011_MuonEff.yaml" }

atlasJetEff :: JetEfficiency
atlasJetEff = JetEfficiency { jetEffFile = Import "Atlas2011_JetEff.yaml" }

atlasTauEff :: TauEfficiency
atlasTauEff = TauEfficiency { tauEffFile = Import "Atlas2011_TauEff.yaml" }

atlas2011Eff = EfficiencyDescription { electron = atlasElecEff 
                                     , photon = atlasPhoEff 
                                     , bJet = atlasBJetEff
                                     , muon = atlasMuonEff
                                     , jet = atlasJetEff
                                     , tau = atlasTauEff
                                     , ptThresholds = atlasPTThresholds 
                                     }



atlasEleDataTight :: ElectronEffData
atlasEleDataTight = ElectronEffData
  { eleName = "ElectronTightATLAS"
  , eleMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "Tight electron object 2011 ATLAS"
      , comment = "We use table from reference" 
      , reference = "arXiv:xxxx.yyyy" }
  , eleEfficiency = PTEtaGrid 
      { ptBins = [4.0, 7.0, 10.0, 15.0, 20.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 80.0]
      , etaBins = [-2.5, -2.0, -1.52, -1.37, -0.75, 0.0, 0.75, 1.37, 1.52, 2.0, 2.5]
      , grid = atlasElecTightEff
      }
  } 

atlasEleDataMedium :: ElectronEffData
atlasEleDataMedium = ElectronEffData
  { eleName = "ElectronMediumATLAS"
  , eleMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "Medium electron object 2011 ATLAS"
      , comment = "We use table from reference" 
      , reference = "arXiv:xxxx.yyyy" }
  , eleEfficiency = PTEtaGrid
      { ptBins = [4.0, 7.0, 10.0, 15.0, 20.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 80.0]
      , etaBins = [-2.5, -2.0, -1.52, -1.37, -0.75, 0.0, 0.75, 1.37, 1.52, 2.0, 2.5]
      , grid = atlasElecMediumEff
      }
  } 

atlasEleDataLoose :: ElectronEffData
atlasEleDataLoose = ElectronEffData
  { eleName = "ElectronLooseATLAS"
  , eleMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "Loose electron object 2011 ATLAS"
      , comment = "We use table from reference" 
      , reference = "arXiv:xxxx.yyyy" }
  , eleEfficiency = PTEtaGrid
      { ptBins = [4.0, 7.0, 10.0, 15.0, 20.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 80.0]
      , etaBins = [-2.5, -2.0, -1.52, -1.37, -0.75, 0.0, 0.75, 1.37, 1.52, 2.0, 2.5]
      , grid = atlasElecLooseEff
      }
  } 

atlasElecTightEff = GridFull { gridData =
      [ [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ] ]
 }

atlasElecMediumEff = GridFull { gridData = 
      [ [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ] ]
    }
atlasElecLooseEff = GridFull { gridData = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
    }

atlasPhoDataLoose :: PhotonEffData
atlasPhoDataLoose = PhotonEffData
  { phoName = "PhotonLooseATLAS"
  , phoMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "Loose photon object 2011 ATLAS"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , phoEfficiency = PTEtaGrid 
      { ptBins = [15.0, 18.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 60.0, 80.0, 100.0, 150.0, 200.0, 250.0, 300.0, 350.0, 400.0, 450.0, 500.0] 
      , etaBins = [ -2.4, -2.2, -2.0, -1.8, -1.52, -1.37, -1.2, -1.0, -0.8, -0.6, -0.4, -0.2, -0.1, 0.0, 0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.37, 1.52, 1.8, 2.0, 2.2, 2.4 ] 
      , grid = GridConst { gridConst = 1.0 } }
  }

atlasPhoDataTight :: PhotonEffData
atlasPhoDataTight = PhotonEffData
  { phoName = "PhotonTightATLAS"
  , phoMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "Tight photon object 2011 ATLAS"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , phoEfficiency = PTEtaGrid 
      { ptBins = [15.0, 18.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 60.0, 80.0, 100.0, 150.0, 200.0, 250.0, 300.0, 350.0, 400.0, 450.0, 500.0] 
      , etaBins = [ -2.4, -2.2, -2.0, -1.8, -1.52, -1.37, -1.2, -1.0, -0.8, -0.6, -0.4, -0.2, -0.1, 0.0, 0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.37, 1.52, 1.8, 2.0, 2.2, 2.4 ] 
      , grid = GridConst { gridConst = 1.0 } }
  }

atlasBJetDataSV50 :: BJetEffData
atlasBJetDataSV50 = BJetEffData
  { bJetName = "BJet_SV50_ATLAS"
  , bJetMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "SV50 ATLAS BJet Tagging"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , bJetEfficiency = PTEtaGrid 
      { ptBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ] 
      , etaBins = [ 0.0, 1.2, 2.5 ] 
      , grid = GridConst { gridConst = 0.5 } }
  , bJetRejection = PTEtaGrid
      { ptBins = [ 20.0, 25.0, 40.0, 60.0, 90.0, 140.0, 200.0, 300.0, 500.0 ]
      , etaBins = [ 0.0, 1.2, 2.5 ]
      , grid = GridConst { gridConst = 100.0 } }
  }


atlasBJetDataJP50 :: BJetEffData
atlasBJetDataJP50 = BJetEffData
  { bJetName = "BJet_JP50_ATLAS"
  , bJetMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "JP50 ATLAS BJet Tagging"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , bJetEfficiency = PTEtaGrid 
      { ptBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ] 
      , etaBins = [ 0.0, 1.2, 2.5 ] 
      , grid =  GridFull 
          { gridData =  
              [ [ 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5 ]
              , [ 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5 ] ] } }
  , bJetRejection = PTEtaGrid
      { ptBins = [ 20.0, 25.0, 40.0, 60.0, 90.0, 140.0, 200.0, 300.0, 500.0 ]
      , etaBins = [ 0.0, 1.2, 2.5 ]
      , grid = GridConst { gridConst = 100.0 } }
  }

atlasBJetDataJP70 :: BJetEffData
atlasBJetDataJP70 = BJetEffData
  { bJetName = "BJet_JP70_ATLAS"
  , bJetMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "JP70 ATLAS BJet Tagging"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , bJetEfficiency = PTEtaGrid 
      { ptBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ] 
      , etaBins = [ 0.0, 1.2, 2.5 ] 
      , grid =  GridConst { gridConst = 0.7 } }
  , bJetRejection = PTEtaGrid
      { ptBins = [ 20.0, 25.0, 40.0, 60.0, 90.0, 140.0, 200.0, 300.0, 500.0 ]
      , etaBins = [ 0.0, 1.2, 2.5 ]
      , grid = GridConst { gridConst = 100.0 } }
  }





atlasMuonDataCB1 :: MuonEffData
atlasMuonDataCB1 = MuonEffData
  { muonName = "Muon_CB1_ATLAS"
  , muonMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "CB1 ATLAS Muon"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , muonEfficiency = PTEtaGrid 
      { ptBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ]
      , etaBins = [ -2.5, -2.25, -2.0, -1.75, -1.50, -1.25, -1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5 ]
      , grid = GridConst { gridConst = 1.0 } }
  }

atlasMuonDataCB2 :: MuonEffData
atlasMuonDataCB2 = MuonEffData
  { muonName = "Muon_CB2_ATLAS"
  , muonMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "CB2 ATLAS Muon"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , muonEfficiency = PTEtaGrid 
      { ptBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ]
      , etaBins = [ -2.5, -2.25, -2.0, -1.75, -1.50, -1.25, -1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5 ]
      , grid = GridConst { gridConst = 1.0 } }
  }

atlasMuonDataST1 :: MuonEffData
atlasMuonDataST1 = MuonEffData
  { muonName = "Muon_ST1_ATLAS"
  , muonMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ST1 ATLAS Muon"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , muonEfficiency = PTEtaGrid 
      { ptBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ]
      , etaBins = [ -2.5, -2.25, -2.0, -1.75, -1.50, -1.25, -1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5 ]
      , grid = GridConst { gridConst = 1.0 } }
  }

atlasMuonDataST2 :: MuonEffData
atlasMuonDataST2 = MuonEffData
  { muonName = "Muon_ST2_ATLAS"
  , muonMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ST2 ATLAS Muon"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , muonEfficiency = PTEtaGrid 
      { ptBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ]
      , etaBins = [ -2.5, -2.25, -2.0, -1.75, -1.50, -1.25, -1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5 ]
      , grid = GridConst { gridConst = 1.0 } }
  }


atlasJetData :: JetEffData 
atlasJetData = JetEffData
  { jetName = "Jet_ATLAS"
  , jetMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Jet"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , jetEfficiency = PTEtaGrid 
      { ptBins = [ 20.0, 30.0, 40.0, 60.0, 80.0, 120.0, 160.0, 200.0, 280.0, 360.0, 500.0, 600.0, 900.0, 1200.0, 2000.0 ] 
      , etaBins = [ -4.5, -3.6, -2.8, -2.5, -2.0, -1.2, -0.8, -0.3, 0.0, 0.3, 0.8, 1.2, 2.0, 2.5, 2.8, 3.6, 4.5 ]
      , grid = GridConst { gridConst = 1.0 }
      }
  } 

atlasTauDataCutLoose :: TauEffData 
atlasTauDataCutLoose = TauEffData 
  { tauName = "Tau_Cut_Loose_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau Cut"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "Cut"
  , tauEfficiency1Prong = PTEtaGrid
      { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
      , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
      , grid = GridConst { gridConst = 1.0 } }
  , tauRejection1Prong = PTEtaGrid 
      { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
      , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
      , grid = GridConst { gridConst = 1.0 } }
  , tauEfficiency3Prong = PTEtaGrid
      { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
      , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
      , grid = GridConst { gridConst = 1.0 } }
  , tauRejection3Prong = PTEtaGrid
      { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
      , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
      , grid = GridConst { gridConst = 1.0 } }

  }

{-
      
  , tauEffCutMSing = GridConst { gridConst = 1.0 }
  , tauEffCutTSing = GridConst { gridConst = 1.0 }
  , tauEffLikLSing = GridConst { gridConst = 1.0 }
  , tauEffLikMSing = GridConst { gridConst = 1.0 }
  , tauEffLikTSing = GridConst { gridConst = 1.0 }
  , tauEffBdtLSing = GridConst { gridConst = 1.0 }
  , tauEffBdtMSing = GridConst { gridConst = 1.0 }
  , tauEffBdtTSing = GridConst { gridConst = 1.0 }
  , tauEffCutLMult = 
  , tauEffCutMMult = GridConst { gridConst = 1.0 }
  , tauEffCutTMult = GridConst { gridConst = 1.0 }
  , tauEffLikLMult = GridConst { gridConst = 1.0 }
  , tauEffLikMMult = GridConst { gridConst = 1.0 }
  , tauEffLikTMult = GridConst { gridConst = 1.0 }
  , tauEffBdtLMult = GridConst { gridConst = 1.0 }
  , tauEffBdtMMult = GridConst { gridConst = 1.0 }
  , tauEffBdtTMult = GridConst { gridConst = 1.0 }
  , tauRejPtBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
  , tauRejEtaBins = [ 0.0, 1.3, 1.6, 2.5 ]
  , tauRejCutLSing = 
  , tauRejCutMSing = GridConst { gridConst = 1.0 }
  , tauRejCutTSing = GridConst { gridConst = 1.0 }
  , tauRejLikLSing = GridConst { gridConst = 1.0 }
  , tauRejLikMSing = GridConst { gridConst = 1.0 }
  , tauRejLikTSing = GridConst { gridConst = 1.0 }
  , tauRejBdtLSing = GridConst { gridConst = 1.0 }
  , tauRejBdtMSing = GridConst { gridConst = 1.0 }
  , tauRejBdtTSing = GridConst { gridConst = 1.0 }
  , tauRejCutLMult = 
  , tauRejCutMMult = GridConst { gridConst = 1.0 }
  , tauRejCutTMult = GridConst { gridConst = 1.0 }
  , tauRejLikLMult = GridConst { gridConst = 1.0 }
  , tauRejLikMMult = GridConst { gridConst = 1.0 }
  , tauRejLikTMult = GridConst { gridConst = 1.0 }
  , tauRejBdtLMult = GridConst { gridConst = 1.0 }
  , tauRejBdtMMult = GridConst { gridConst = 1.0 }
  , tauRejBdtTMult = GridConst { gridConst = 1.0 }
  }

-}

atlasPTThresholds :: PTThresholds
atlasPTThresholds = PTThresholds  
  { muPTMin = 20.0
  , elePTMin = 5.0
  , phoPTMin = 20.0
  , jetPTMin = 20.0
  , bJetPTMin = 20.0
  , trkPTMin = 0.5
  , tauPTMin = 5.0
  } 