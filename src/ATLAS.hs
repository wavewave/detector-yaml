{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ATLAS where

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

data ElectronEfficiency = ElectronEfficiency { elecEffFile :: ExtFile }
			  
data ElectronEffData = ElectronEffDataGrid    
                            { eleName :: Text
                            , eleTag :: Text
                            , eleDescr :: Text
                            , eleComment :: Text
                            , eleReference :: Text
                            , elePtBins :: [Scientific] 
			    , eleEtaBins :: [Scientific] 
                            , eleGrid :: [ [ Scientific ] ] 
			    }
                     | ElectronEffDataInterpolation
                            { eleName :: Text
                            , eleTag :: Text 
                            , eleDescr :: Text
                            , eleComment :: Text
                            , eleReference :: Text
                            , eleFunc :: Text  
			    }
                         




data EfficiencyGrid = EfficiencyGrid { effGridName :: Text
                                     , effGridData :: [ [ Scientific ] ] 
                                     }


data PhotonEfficiency = PhotonEfficiency { phoEffFile :: ExtFile }

data PhotonEffData = PhotonEffData 
                          { phoLowPtBins :: [Scientific] 
                          , phoHighPtBins :: [Scientific]
                          , phoEtaBins :: [Scientific] 
                          -- , nPhoEta :: Int
                          -- , nPhoPtLo :: Int
                          -- , nPhoPtHi :: Int
                          , loosePhoEffLow :: [ [ Scientific ] ] 
                          , tightPhoEffLow :: [ [ Scientific ] ] 
                          , loosePhoEffHi  :: [ [ Scientific ] ] 
                          , tightPhoEffHi  :: [ [ Scientific ] ] 
                          } 

data BJetEfficiency = BJetEfficiency { bJetEffFile :: ExtFile }

data BJetEffData = BJetEffData
                        { bTagEffPtBins :: [ Scientific ] 
                        , bTagRejPtBins :: [ Scientific ] 
                        , bTagEffEtaBins :: [ Scientific ] 
                        , bTagRejEtaBins :: [ Scientific ]
                        -- , nBEeta :: Int 
                        -- , nBReta :: Int
                        -- , nBEpt :: Int
                        -- , nBRpt :: Int 
                        , bTagEffSV50 :: [ [ Scientific ] ]
                        , bTagEffJP50 :: [ [ Scientific ] ]
                        , bTagEffJP70 :: [ [ Scientific ] ]
                        , bTagRejSV50 :: [ [ Scientific ] ]
                        , bTagRejJP50 :: [ [ Scientific ] ]
                        , bTagRejJP70 :: [ [ Scientific ] ]  
                        } 

data MuonEfficiency = MuonEfficiency { muonEffFile :: ExtFile }

data MuonEffData = MuonEffData
                        { muPtBins :: [ Scientific ] 
                        , muEtaBins :: [ Scientific ] 
                        -- , nMuPt :: Int
                        -- , nMuEta :: Int 
                        , cB1MuEff :: [ [ Scientific ] ] 
                        , cB2MuEff :: [ [ Scientific ] ]
                        , sT1MuEff :: [ [ Scientific ] ] 
                        , sT2MuEff :: [ [ Scientific ] ]
                        }

data JetEfficiency = JetEfficiency { jetEffFile :: ExtFile }

data JetEffData = JetEffData 
                       { jetPtBins :: [ Scientific ]
                       , jetEtaBins :: [ Scientific ] 
                       -- , nJetPt :: Int
                       -- , nJetEta :: Int
                       , jetEff :: [ [ Scientific ] ] 
                       } 

data TauEfficiency = TauEfficiency { tauEffFile :: ExtFile }

data TauEffData = TauEffData
                       { tauEffPtBins :: [ Scientific ] 
                       , tauEffEtaBins :: [ Scientific ] 
                       -- , nTEPt :: Int
                       -- , nTEEta :: Int 
                       , tauEffCutLSing :: [ [ Scientific ] ] 
                       , tauEffCutMSing :: [ [ Scientific ] ] 
                       , tauEffCutTSing :: [ [ Scientific ] ] 
                       , tauEffLikLSing :: [ [ Scientific ] ] 
                       , tauEffLikMSing :: [ [ Scientific ] ]
                       , tauEffLikTSing :: [ [ Scientific ] ] 
                       , tauEffBdtLSing :: [ [ Scientific ] ] 
                       , tauEffBdtMSing :: [ [ Scientific ] ]
                       , tauEffBdtTSing :: [ [ Scientific ] ]
                       , tauEffCutLMult :: [ [ Scientific ] ]
                       , tauEffCutMMult :: [ [ Scientific ] ]
                       , tauEffCutTMult :: [ [ Scientific ] ] 
                       , tauEffLikLMult :: [ [ Scientific ] ]
                       , tauEffLikMMult :: [ [ Scientific ] ] 
                       , tauEffLikTMult :: [ [ Scientific ] ]
                       , tauEffBdtLMult :: [ [ Scientific ] ]
                       , tauEffBdtMMult :: [ [ Scientific ] ] 
                       , tauEffBdtTMult :: [ [ Scientific ] ]
                       , tauRejPtBins :: [ Scientific ] 
                       , tauRejEtaBins :: [ Scientific ] 
                       , tauRejCutLSing :: [ [ Scientific ] ] 
                       , tauRejCutMSing :: [ [ Scientific ] ] 
                       , tauRejCutTSing :: [ [ Scientific ] ] 
                       , tauRejLikLSing :: [ [ Scientific ] ] 
                       , tauRejLikMSing :: [ [ Scientific ] ]
                       , tauRejLikTSing :: [ [ Scientific ] ] 
                       , tauRejBdtLSing :: [ [ Scientific ] ] 
                       , tauRejBdtMSing :: [ [ Scientific ] ]
                       , tauRejBdtTSing :: [ [ Scientific ] ]
                       , tauRejCutLMult :: [ [ Scientific ] ]
                       , tauRejCutMMult :: [ [ Scientific ] ]
                       , tauRejCutTMult :: [ [ Scientific ] ] 
                       , tauRejLikLMult :: [ [ Scientific ] ]
                       , tauRejLikMMult :: [ [ Scientific ] ] 
                       , tauRejLikTMult :: [ [ Scientific ] ]
                       , tauRejBdtLMult :: [ [ Scientific ] ]
                       , tauRejBdtMMult :: [ [ Scientific ] ] 
                       , tauRejBdtTMult :: [ [ Scientific ] ]
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

mkGrid :: EfficiencyGrid -> YamlValue
mkGrid EfficiencyGrid {..} = 
  YObject $ [ ("Name", (YPrim . YString) effGridName)
            , ("Data", mkWrap (map mkInline effGridData))
            ]

mkString = YPrim . YString

mkElectronEffData :: ElectronEffData -> YamlValue
mkElectronEffData ElectronEffDataGrid {..} = 
    YObject $ [ ("Name", mkString eleName)
              , ("Type", mkString "Grid")
              , ("Tag" , mkString eleTag)
              , ("Description", mkString eleDescr)
              , ("Comment", mkString eleComment)
              , ("Reference", mkString eleReference) 
              , ("ElePtBins", mkInline elePtBins)
              , ("EleEtaBins", mkInline eleEtaBins)
              , ("EfficiencyGrid", mkWrap (map mkInline eleGrid) )
              ]
mkElectronEffData ElectronEffDataInterpolation {..} = 
    YObject $ [ ("Name", mkString eleName)
              , ("Type", mkString "Interpolation")
              , ("Tag" , mkString eleTag)
              , ("Description", mkString eleDescr)
              , ("Comment", mkString eleComment)
              , ("Reference", mkString eleReference) 
              , ("Function", mkString eleFunc)
              ]


mkPhotonEffData :: PhotonEffData -> YamlValue
mkPhotonEffData PhotonEffData {..} = 
    YObject $ [ ("PhoLowPtBins", mkInline phoLowPtBins)
              , ("PhoHighPtBins", mkInline phoHighPtBins)
              , ("PhoEtaBins", mkInline phoEtaBins) 
              , ("LoosePhoEffLow", mkWrap (map mkInline loosePhoEffLow) ) 
              , ("TightPhoEffLow", mkWrap (map mkInline tightPhoEffLow) )
              , ("LoosePhoEffHi", mkWrap (map mkInline loosePhoEffLow) )
              , ("TightPhoEffHi", mkWrap (map mkInline loosePhoEffLow) )
              ] 




mkBJetEffData :: BJetEffData -> YamlValue
mkBJetEffData BJetEffData {..} = 
    YObject $ [ ( "BtagEffPtBins", mkInline bTagEffPtBins )
              , ( "BTagRejPtBins", mkInline bTagRejPtBins )
              , ( "BTagEffEtaBins", mkInline bTagEffEtaBins )
              , ( "BTagRejEtaBins", mkInline bTagRejEtaBins ) 
              , ( "BtagEffSV50", mkWrap (map mkInline bTagEffSV50) )
              , ( "BtagEffJP50", mkWrap (map mkInline bTagEffJP50) )
              , ( "BtagEffJP70", mkWrap (map mkInline bTagEffJP70) )
              , ( "BtagRejSV50", mkWrap (map mkInline bTagRejSV50) )
              , ( "BtagRejJP50", mkWrap (map mkInline bTagRejJP50) )
              , ( "BtagRejJP70", mkWrap (map mkInline bTagRejJP70) )
              ] 

mkMuonEffData :: MuonEffData -> YamlValue
mkMuonEffData MuonEffData {..} = 
  YObject $ [ ( "MuPtBins", mkInline muPtBins ) 
            , ( "MuEtaBins", mkInline muEtaBins ) 
            , ( "CB1MuEff", mkWrap (map mkInline cB1MuEff) )
            , ( "CB2MuEff", mkWrap (map mkInline cB2MuEff) )
            , ( "ST1MuEff", mkWrap (map mkInline sT1MuEff) )
            , ( "ST2MuEff", mkWrap (map mkInline sT2MuEff) )
            ] 

mkJetEffData :: JetEffData -> YamlValue
mkJetEffData JetEffData {..} = 
  YObject $ [ ( "JetPtBins", mkInline jetPtBins )
            , ( "jetEtaBins", mkInline jetEtaBins )
            , ( "jetEff", mkWrap (map mkInline jetEff) )
            ] 

mkTauEffData :: TauEffData -> YamlValue
mkTauEffData TauEffData {..} = 
  YObject $ [ ( "TauEffPtBins", mkInline tauEffPtBins )
            , ( "TauEffEtaBins", mkInline tauEffEtaBins )
            , ( "TauEffCutLSing", mkWrap (map mkInline tauEffCutLSing) )
            , ( "TauEffCutMSing", mkWrap (map mkInline tauEffCutMSing) )
            , ( "TauEffCutTSing", mkWrap (map mkInline tauEffCutTSing) )
            , ( "TauEffLikLSing", mkWrap (map mkInline tauEffLikLSing) )
            , ( "TauEffLikMSing", mkWrap (map mkInline tauEffLikMSing) )
            , ( "TauEffLikTSing", mkWrap (map mkInline tauEffLikTSing) )
            , ( "TauEffBdtLSing", mkWrap (map mkInline tauEffBdtLSing) )
            , ( "TauEffBdtMSing", mkWrap (map mkInline tauEffBdtMSing) )
            , ( "TauEffBdtTSing", mkWrap (map mkInline tauEffBdtTSing) )
            , ( "TauEffCutLMult", mkWrap (map mkInline tauEffCutLMult) )
            , ( "TauEffCutMMult", mkWrap (map mkInline tauEffCutMMult) )
            , ( "TauEffCutTMult", mkWrap (map mkInline tauEffCutTMult) )
            , ( "TauEffLikLMult", mkWrap (map mkInline tauEffLikLMult) )
            , ( "TauEffLikMMult", mkWrap (map mkInline tauEffLikMMult) )
            , ( "TauEffLikTMult", mkWrap (map mkInline tauEffLikTMult) )
            , ( "TauEffBdtLMult", mkWrap (map mkInline tauEffBdtLMult) )
            , ( "TauEffBdtMMult", mkWrap (map mkInline tauEffBdtMMult) )
            , ( "TauEffBdtTMult", mkWrap (map mkInline tauEffBdtTMult) ) 
            , ( "TauRejPtBins", mkInline tauRejPtBins )
            , ( "TauRejEtaBins", mkInline tauRejEtaBins )
            , ( "TauRejCutLSing", mkWrap (map mkInline tauRejCutLSing) )
            , ( "TauRejCutMSing", mkWrap (map mkInline tauRejCutMSing) )
            , ( "TauRejCutTSing", mkWrap (map mkInline tauRejCutTSing) )
            , ( "TauRejLikLSing", mkWrap (map mkInline tauRejLikLSing) )
            , ( "TauRejLikMSing", mkWrap (map mkInline tauRejLikMSing) )
            , ( "TauRejLikTSing", mkWrap (map mkInline tauRejLikTSing) )
            , ( "TauRejBdtLSing", mkWrap (map mkInline tauRejBdtLSing) )
            , ( "TauRejBdtMSing", mkWrap (map mkInline tauRejBdtMSing) )
            , ( "TauRejBdtTSing", mkWrap (map mkInline tauRejBdtTSing) )
            , ( "TauRejCutLMult", mkWrap (map mkInline tauRejCutLMult) )
            , ( "TauRejCutMMult", mkWrap (map mkInline tauRejCutMMult) )
            , ( "TauRejCutTMult", mkWrap (map mkInline tauRejCutTMult) )
            , ( "TauRejLikLMult", mkWrap (map mkInline tauRejLikLMult) )
            , ( "TauRejLikMMult", mkWrap (map mkInline tauRejLikMMult) )
            , ( "TauRejLikTMult", mkWrap (map mkInline tauRejLikTMult) )
            , ( "TauRejBdtLMult", mkWrap (map mkInline tauRejBdtLMult) )
            , ( "TauRejBdtMMult", mkWrap (map mkInline tauRejBdtMMult) )
            , ( "TauRejBdtTMult", mkWrap (map mkInline tauRejBdtTMult) )
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



atlasElecEffData :: ElectronEffData
atlasElecEffData = ElectronEffDataGrid
  { eleName = "Electron Tight"
  , eleTag = "ATLAS"
  , eleDescr = "Tight electron object 2011 ATLAS"
  , eleComment = "We use table from reference" 
  , eleReference = "arXiv:xxxx.yyyy"
  , elePtBins = [4.0, 7.0, 10.0, 15.0, 20.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 80.0]
  , eleEtaBins = [-2.5, -2.0, -1.52, -1.37, -0.75, 0.0, 0.75, 1.37, 1.52, 2.0, 2.5]
  , eleGrid = atlasElecTightEff


  } 
  -- , atlasElecMediumEff, atlasElecLooseEff ]


atlasElecTightEff = 
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


atlasElecMediumEff = EfficiencyGrid
  { effGridName = "Medium"
  , effGridData = 
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
atlasElecLooseEff = EfficiencyGrid 
  { effGridName = "Loose"
  , effGridData =  
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

atlasPhoEffData :: PhotonEffData
atlasPhoEffData = PhotonEffData 
  { phoLowPtBins = [15.0, 18.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 60.0, 80.0, 100.0]
  , phoHighPtBins = [150.0, 200.0, 250.0, 300.0, 350.0, 400.0, 450.0, 500.0] 
  , phoEtaBins = [ -2.4, -2.2, -2.0, -1.8, -1.52, -1.37, -1.2, -1.0, -0.8, -0.6, -0.4, -0.2, -0.1, 0.0, 0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.37, 1.52, 1.8, 2.0, 2.2, 2.4 ] 
  -- , nPhoEta = 26
  -- , nPhoPtLo = 11
  -- , nPhoPtHi = 8 
  , loosePhoEffLow = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tightPhoEffLow = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0] ]
  , loosePhoEffHi = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0] ]
  , tightPhoEffHi = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0] ]
  }

atlasBJetEffData :: BJetEffData
atlasBJetEffData = BJetEffData
  { bTagEffPtBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ] 
  , bTagRejPtBins = [ 20.0, 25.0, 40.0, 60.0, 90.0, 140.0, 200.0, 300.0, 500.0 ] 
  , bTagEffEtaBins = [ 0.0, 1.2, 2.5 ] 
  , bTagRejEtaBins = [ 0.0, 1.2, 2.5 ] 
  -- , nBEeta = 2
  -- , nBReta = 2
  -- , nBEpt = 14
  -- , nBRpt = 8
  , bTagEffSV50 = 
      [ [ 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5 ]
      , [ 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5 ] ]
  , bTagEffJP50 = 
      [ [ 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5 ]
      , [ 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5 ] ]
  , bTagEffJP70 = 
      [ [ 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7 ]
      , [ 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7 ] ]
  , bTagRejSV50 = 
      [ [ 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0 ]
      , [ 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0 ] ]
  , bTagRejJP50 = 
      [ [ 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0 ]
      , [ 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0 ] ]
  , bTagRejJP70 =
      [ [ 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0 ]
      , [ 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0 ] ] 

  } 

atlasMuonEffData :: MuonEffData
atlasMuonEffData = MuonEffData
  { muPtBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ]
  , muEtaBins = [ -2.5, -2.25, -2.0, -1.75, -1.50, -1.25, -1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5 ]
  -- , nMuPt = 8
  -- , nMuEta = 20 
  , cB1MuEff = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]  
  , cB2MuEff = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , sT1MuEff = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , sT2MuEff = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  } 

atlasJetEffData :: JetEffData 
atlasJetEffData = JetEffData
  { jetPtBins = [ 20.0, 30.0, 40.0, 60.0, 80.0, 120.0, 160.0, 200.0, 280.0, 360.0, 500.0, 600.0, 900.0, 1200.0, 2000.0 ] 
  , jetEtaBins = [ -4.5, -3.6, -2.8, -2.5, -2.0, -1.2, -0.8, -0.3, 0.0, 0.3, 0.8, 1.2, 2.0, 2.5, 2.8, 3.6, 4.5 ]
  -- , nJetPt = 14
  -- , nJetEta = 16 
  , jetEff = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  } 

atlasTauEffData :: TauEffData 
atlasTauEffData = TauEffData 
  { tauEffPtBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
  , tauEffEtaBins = [ 0.0, 1.3, 1.6, 2.5 ] 
  -- , nTEPt = 9
  -- , nTEEta = 3 
  , tauEffCutLSing = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffCutMSing = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffCutTSing = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffLikLSing =
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffLikMSing = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffLikTSing = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffBdtLSing =
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffBdtMSing = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffBdtTSing = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffCutLMult =
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffCutMMult = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffCutTMult = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffLikLMult =
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffLikMMult = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffLikTMult = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffBdtLMult =
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffBdtMMult = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauEffBdtTMult = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejPtBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
  , tauRejEtaBins = [ 0.0, 1.3, 1.6, 2.5 ]
  -- , nTRPt = 9 
  -- , nTREta = 3
  , tauRejCutLSing = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejCutMSing = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejCutTSing = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejLikLSing =
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejLikMSing = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejLikTSing = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejBdtLSing =
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejBdtMSing = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejBdtTSing = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejCutLMult =
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejCutMMult = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejCutTMult = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejLikLMult =
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejLikMMult = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejLikTMult = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejBdtLMult =
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejBdtMMult = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
  , tauRejBdtTMult = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]
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