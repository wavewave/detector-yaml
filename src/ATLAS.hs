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
  EfficiencyDescription { elecEfficiency :: ElectronEfficiency 
                        , phoEfficiency :: PhotonEfficiency 
                        , bJetEfficiency :: BJetEfficiency 
                        , muonEfficiency :: MuonEfficiency
                        , jetEfficiency :: JetEfficiency
                        , tauEfficiency :: TauEfficiency 
                        , ptThresholds :: PTThresholds }

data ExtFile = ExtFile { fileName :: Text }

data MetaInfo = MetaInfo { tag :: Text
                         , description :: Text
                         , comment :: Text 
                         , reference :: Text } 

data Grid = GridFull { gridData :: [ [ Scientific ] ] 
                                  }
          | GridConst { gridConst :: Scientific } 


data ElectronEfficiency = ElectronEfficiency { elecEffFile :: ExtFile }
			  
data ElectronEffData = ElectronEffDataGrid    
                            { eleName :: Text
                            , eleMetaInfo :: MetaInfo 
                            , elePtBins :: [Scientific] 
			    , eleEtaBins :: [Scientific] 
                            , eleGrid :: Grid }
                     | ElectronEffDataInterpolation
                            { eleName :: Text
                            , eleMetaInfo :: MetaInfo
                            , eleFunc :: Text }
			    
data PhotonEfficiency = PhotonEfficiency { phoEffFile :: ExtFile }

data PhotonEffData = PhotonEffDataGrid
                          { phoName :: Text
                          , phoMetaInfo :: MetaInfo
                          , phoPtBins :: [Scientific] 
                          , phoEtaBins :: [Scientific] 
                          , phoGrid :: Grid }
                   | PhotonEffDataInterpolation
                          { phoName :: Text
                          , phoMetaInfo :: MetaInfo
                          , phoFunc :: Text }
                       
data BJetEfficiency = BJetEfficiency { bJetEffFile :: ExtFile }

data BJetEffData = BJetEffData
                     -- { bJetName :: Text 
                     -- , bJetMetaInfo :: MetaInfo
                     { bTagEffPtBins :: [ Scientific ] 
                     , bTagRejPtBins :: [ Scientific ] 
                     , bTagEffEtaBins :: [ Scientific ] 
                     , bTagRejEtaBins :: [ Scientific ]
                     , bTagEffSV50 :: Grid
                     , bTagEffJP50 :: Grid
                     , bTagEffJP70 :: Grid
                     , bTagRejSV50 :: Grid
                     , bTagRejJP50 :: Grid
                     , bTagRejJP70 :: Grid
                     } 

data MuonEfficiency = MuonEfficiency { muonEffFile :: ExtFile }

data MuonEffData = MuonEffData
                        { muPtBins :: [ Scientific ] 
                        , muEtaBins :: [ Scientific ] 
                        , cB1MuEff :: Grid
                        , cB2MuEff :: Grid
                        , sT1MuEff :: Grid
                        , sT2MuEff :: Grid
                        }

data JetEfficiency = JetEfficiency { jetEffFile :: ExtFile }

data JetEffData = JetEffData 
                       { jetPtBins :: [ Scientific ]
                       , jetEtaBins :: [ Scientific ] 
                       , jetEff :: Grid
                       } 

data TauEfficiency = TauEfficiency { tauEffFile :: ExtFile }

data TauEffData = TauEffData
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

data PTThresholds = PTThresholds 
                      { muPTMin :: Scientific
                      , elePTMin :: Scientific
                      , phoPTMin :: Scientific
                      , jetPTMin :: Scientific
                      , bJetPTMin :: Scientific
                      , trkPTMin :: Scientific
                      , tauPTMin :: Scientific
                      }


mkExtFile :: ExtFile -> YamlValue
mkExtFile ExtFile {..} = 
  YObject $ [ ("Type", (YPrim . YString) "ExternalFile")
            , ("FileName", (YPrim . YString) fileName) ] 

mkElectronEfficiency :: ElectronEfficiency -> YamlValue
mkElectronEfficiency ElectronEfficiency {..} = mkExtFile elecEffFile

mkPhotonEfficiency :: PhotonEfficiency -> YamlValue
mkPhotonEfficiency PhotonEfficiency {..} = mkExtFile phoEffFile

mkBJetEfficiency :: BJetEfficiency -> YamlValue
mkBJetEfficiency BJetEfficiency {..} = mkExtFile bJetEffFile

mkMuonEfficiency :: MuonEfficiency -> YamlValue
mkMuonEfficiency MuonEfficiency {..} = mkExtFile muonEffFile

mkJetEfficiency :: JetEfficiency -> YamlValue
mkJetEfficiency JetEfficiency {..} = mkExtFile jetEffFile

mkTauEfficiency :: TauEfficiency -> YamlValue
mkTauEfficiency TauEfficiency {..} = mkExtFile tauEffFile

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
mkElectronEffData ElectronEffDataGrid {..} = 
    YObject $ [ ("Name", mkString eleName) 
              , ("Type", mkString "Grid") ] 
              <> mkMetaInfoPairs eleMetaInfo <>
              [ ("PtBins", mkInline elePtBins)
              , ("EtaBins", mkInline eleEtaBins)
              , ("EfficiencyGrid", mkGrid eleGrid )
              ]
mkElectronEffData ElectronEffDataInterpolation {..} = 
    YObject $ [ ("Name", mkString eleName)
              , ("Type", mkString "Interpolation") ]
              <> mkMetaInfoPairs eleMetaInfo <>
              [ ("Function", mkString eleFunc)
              ]


mkPhotonEffData :: PhotonEffData -> YamlValue
mkPhotonEffData PhotonEffDataGrid {..} = 
    YObject $ [ ("Name", mkString phoName)
              , ("Type", mkString "Grid") ]
              <> mkMetaInfoPairs phoMetaInfo <> 
              [ ("PtBins", mkInline phoPtBins)
              , ("EtaBins", mkInline phoEtaBins) 
              , ("EfficiencyData", mkGrid  phoGrid ) 
              ] 




mkBJetEffData :: BJetEffData -> YamlValue
mkBJetEffData BJetEffData {..} = 
    YObject $ [ ( "BtagEffPtBins", mkInline bTagEffPtBins )
              , ( "BTagRejPtBins", mkInline bTagRejPtBins )
              , ( "BTagEffEtaBins", mkInline bTagEffEtaBins )
              , ( "BTagRejEtaBins", mkInline bTagRejEtaBins ) 
              , ( "BtagEffSV50", mkGrid bTagEffSV50 )
              , ( "BtagEffJP50", mkGrid bTagEffJP50 )
              , ( "BtagEffJP70", (mkGrid  bTagEffJP70) )
              , ( "BtagRejSV50", (mkGrid  bTagRejSV50) )
              , ( "BtagRejJP50", (mkGrid  bTagRejJP50) )
              , ( "BtagRejJP70", (mkGrid  bTagRejJP70) )
              ] 

-- charm rejection

mkMuonEffData :: MuonEffData -> YamlValue
mkMuonEffData MuonEffData {..} = 
  YObject $ [ ( "MuPtBins", mkInline muPtBins ) 
            , ( "MuEtaBins", mkInline muEtaBins ) 
            , ( "CB1MuEff", (mkGrid  cB1MuEff) )
            , ( "CB2MuEff", (mkGrid  cB2MuEff) )
            , ( "ST1MuEff", (mkGrid  sT1MuEff) )
            , ( "ST2MuEff", (mkGrid  sT2MuEff) )
            ] 

mkJetEffData :: JetEffData -> YamlValue
mkJetEffData JetEffData {..} = 
  YObject $ [ ( "JetPtBins", mkInline jetPtBins )
            , ( "jetEtaBins", mkInline jetEtaBins )
            , ( "jetEff", (mkGrid  jetEff) )
            ] 

mkTauEffData :: TauEffData -> YamlValue
mkTauEffData TauEffData {..} = 
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
    YObject $ [ ( "Electron", mkElectronEfficiency elecEfficiency )  
              , ( "Photon", mkPhotonEfficiency phoEfficiency ) 
              , ( "BJet", mkBJetEfficiency bJetEfficiency )
              , ( "MuonEfficiency", mkMuonEfficiency muonEfficiency ) 
              , ( "JetEfficiency", mkJetEfficiency jetEfficiency )
              , ( "TauEfficiency", mkTauEfficiency tauEfficiency )
              , ( "PTThresholds", mkPTThresholds ptThresholds )
              ] 

atlasElecEff :: ElectronEfficiency
atlasElecEff = ElectronEfficiency { elecEffFile = ExtFile "Atlas2011_ElecEff.yaml" }

atlasPhoEff :: PhotonEfficiency
atlasPhoEff = PhotonEfficiency { phoEffFile = ExtFile "Atlas2011_PhoEff.yaml" }

atlasBJetEff :: BJetEfficiency
atlasBJetEff = BJetEfficiency { bJetEffFile = ExtFile "Atlas2011_BJetEff.yaml" }

atlasMuonEff :: MuonEfficiency
atlasMuonEff = MuonEfficiency { muonEffFile = ExtFile "Atlas2011_MuonEff.yaml" }

atlasJetEff :: JetEfficiency
atlasJetEff = JetEfficiency { jetEffFile = ExtFile "Atlas2011_JetEff.yaml" }

atlasTauEff :: TauEfficiency
atlasTauEff = TauEfficiency { tauEffFile = ExtFile "Atlas2011_TauEff.yaml" }

atlas2011Eff = EfficiencyDescription { elecEfficiency = atlasElecEff 
                                     , phoEfficiency = atlasPhoEff 
                                     , bJetEfficiency = atlasBJetEff
                                     , muonEfficiency = atlasMuonEff
                                     , jetEfficiency = atlasJetEff
                                     , tauEfficiency = atlasTauEff
                                     , ptThresholds = atlasPTThresholds 
                                     }



atlasEleDataTight :: ElectronEffData
atlasEleDataTight = ElectronEffDataGrid
  { eleName = "ElectronTightATLAS"
  , eleMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "Tight electron object 2011 ATLAS"
      , comment = "We use table from reference" 
      , reference = "arXiv:xxxx.yyyy" }
  , elePtBins = [4.0, 7.0, 10.0, 15.0, 20.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 80.0]
  , eleEtaBins = [-2.5, -2.0, -1.52, -1.37, -0.75, 0.0, 0.75, 1.37, 1.52, 2.0, 2.5]
  , eleGrid = atlasElecTightEff


  } 

atlasEleDataMedium :: ElectronEffData
atlasEleDataMedium = ElectronEffDataGrid
  { eleName = "ElectronMediumATLAS"
  , eleMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "Medium electron object 2011 ATLAS"
      , comment = "We use table from reference" 
      , reference = "arXiv:xxxx.yyyy" }
  , elePtBins = [4.0, 7.0, 10.0, 15.0, 20.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 80.0]
  , eleEtaBins = [-2.5, -2.0, -1.52, -1.37, -0.75, 0.0, 0.75, 1.37, 1.52, 2.0, 2.5]
  , eleGrid = atlasElecMediumEff
  } 

atlasEleDataLoose :: ElectronEffData
atlasEleDataLoose = ElectronEffDataGrid
  { eleName = "ElectronLooseATLAS"
  , eleMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "Loose electron object 2011 ATLAS"
      , comment = "We use table from reference" 
      , reference = "arXiv:xxxx.yyyy" }
  , elePtBins = [4.0, 7.0, 10.0, 15.0, 20.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 80.0]
  , eleEtaBins = [-2.5, -2.0, -1.52, -1.37, -0.75, 0.0, 0.75, 1.37, 1.52, 2.0, 2.5]
  , eleGrid = atlasElecLooseEff
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
atlasPhoDataLoose = PhotonEffDataGrid
  { phoName = "PhotonLooseATLAS"
  , phoMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "Loose photon object 2011 ATLAS"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , phoPtBins = [15.0, 18.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 60.0, 80.0, 100.0, 150.0, 200.0, 250.0, 300.0, 350.0, 400.0, 450.0, 500.0] 
  , phoEtaBins = [ -2.4, -2.2, -2.0, -1.8, -1.52, -1.37, -1.2, -1.0, -0.8, -0.6, -0.4, -0.2, -0.1, 0.0, 0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.37, 1.52, 1.8, 2.0, 2.2, 2.4 ] 
  , phoGrid = GridConst { gridConst = 1.0 }
  }

atlasPhoDataTight :: PhotonEffData
atlasPhoDataTight = PhotonEffDataGrid
  { phoName = "PhotonTightATLAS"
  , phoMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "Tight photon object 2011 ATLAS"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , phoPtBins = [15.0, 18.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 60.0, 80.0, 100.0, 150.0, 200.0, 250.0, 300.0, 350.0, 400.0, 450.0, 500.0] 
  , phoEtaBins = [ -2.4, -2.2, -2.0, -1.8, -1.52, -1.37, -1.2, -1.0, -0.8, -0.6, -0.4, -0.2, -0.1, 0.0, 0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.37, 1.52, 1.8, 2.0, 2.2, 2.4 ] 
  , phoGrid = GridConst { gridConst = 1.0 }
  }

atlasBJetEffData :: BJetEffData
atlasBJetEffData = BJetEffData
  { bTagEffPtBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ] 
  , bTagRejPtBins = [ 20.0, 25.0, 40.0, 60.0, 90.0, 140.0, 200.0, 300.0, 500.0 ] 
  , bTagEffEtaBins = [ 0.0, 1.2, 2.5 ] 
  , bTagRejEtaBins = [ 0.0, 1.2, 2.5 ] 
  , bTagEffSV50 = GridConst { gridConst = 0.5 } 
  , bTagEffJP50 = GridFull { gridData =  
      [ [ 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5 ]
      , [ 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5 ] ]
    }
  , bTagEffJP70 = GridConst { gridConst = 0.7 }
  , bTagRejSV50 = GridConst { gridConst = 100.0 }
  , bTagRejJP50 = GridConst { gridConst = 100.0 }
  , bTagRejJP70 = GridConst { gridConst = 100.0 }
  } 

atlasMuonEffData :: MuonEffData
atlasMuonEffData = MuonEffData
  { muPtBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ]
  , muEtaBins = [ -2.5, -2.25, -2.0, -1.75, -1.50, -1.25, -1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5 ]
  , cB1MuEff = GridConst { gridConst = 1.0 }
  , cB2MuEff = GridConst { gridConst = 1.0 }
  , sT1MuEff = GridConst { gridConst = 1.0 }
  , sT2MuEff = GridConst { gridConst = 1.0 }
  } 

atlasJetEffData :: JetEffData 
atlasJetEffData = JetEffData
  { jetPtBins = [ 20.0, 30.0, 40.0, 60.0, 80.0, 120.0, 160.0, 200.0, 280.0, 360.0, 500.0, 600.0, 900.0, 1200.0, 2000.0 ] 
  , jetEtaBins = [ -4.5, -3.6, -2.8, -2.5, -2.0, -1.2, -0.8, -0.3, 0.0, 0.3, 0.8, 1.2, 2.0, 2.5, 2.8, 3.6, 4.5 ]
  , jetEff = GridConst { gridConst = 1.0 }
  } 

atlasTauEffData :: TauEffData 
atlasTauEffData = TauEffData 
  { tauEffPtBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
  , tauEffEtaBins = [ 0.0, 1.3, 1.6, 2.5 ] 
  , tauEffCutLSing = GridConst { gridConst = 1.0 }
  , tauEffCutMSing = GridConst { gridConst = 1.0 }
  , tauEffCutTSing = GridConst { gridConst = 1.0 }
  , tauEffLikLSing = GridConst { gridConst = 1.0 }
  , tauEffLikMSing = GridConst { gridConst = 1.0 }
  , tauEffLikTSing = GridConst { gridConst = 1.0 }
  , tauEffBdtLSing = GridConst { gridConst = 1.0 }
  , tauEffBdtMSing = GridConst { gridConst = 1.0 }
  , tauEffBdtTSing = GridConst { gridConst = 1.0 }
  , tauEffCutLMult = GridConst { gridConst = 1.0 }
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
  , tauRejCutLSing = GridConst { gridConst = 1.0 }
  , tauRejCutMSing = GridConst { gridConst = 1.0 }
  , tauRejCutTSing = GridConst { gridConst = 1.0 }
  , tauRejLikLSing = GridConst { gridConst = 1.0 }
  , tauRejLikMSing = GridConst { gridConst = 1.0 }
  , tauRejLikTSing = GridConst { gridConst = 1.0 }
  , tauRejBdtLSing = GridConst { gridConst = 1.0 }
  , tauRejBdtMSing = GridConst { gridConst = 1.0 }
  , tauRejBdtTSing = GridConst { gridConst = 1.0 }
  , tauRejCutLMult = GridConst { gridConst = 1.0 }
  , tauRejCutMMult = GridConst { gridConst = 1.0 }
  , tauRejCutTMult = GridConst { gridConst = 1.0 }
  , tauRejLikLMult = GridConst { gridConst = 1.0 }
  , tauRejLikMMult = GridConst { gridConst = 1.0 }
  , tauRejLikTMult = GridConst { gridConst = 1.0 }
  , tauRejBdtLMult = GridConst { gridConst = 1.0 }
  , tauRejBdtMMult = GridConst { gridConst = 1.0 }
  , tauRejBdtTMult = GridConst { gridConst = 1.0 }
  }

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