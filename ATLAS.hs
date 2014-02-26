{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ATLAS where

import Data.Scientific
import YAML


data ElectronEfficiency = ElectronEfficiency 
			    { elePtBins :: [Scientific] 
			    , eleEtaBins :: [Scientific] 
			    , nEleEta :: Int
			    , nElePt :: Int 
			    , tightEleEff :: [ [ Scientific ] ]
			    , mediumEleEff :: [ [ Scientific ] ] 
			    , looseEleEff :: [ [ Scientific ] ] 
			    }

data PhotonEfficiency = PhotonEfficiency 
                          { phoLowPtBins :: [Scientific] 
                          , phoHighPtBins :: [Scientific]
                          , phoEtaBins :: [Scientific] 
                          , nPhoEta :: Int
                          , nPhoPtLo :: Int
                          , nPhoPtHi :: Int
                          , loosePhoEffLow :: [ [ Scientific ] ] 
                          , tightPhoEffLow :: [ [ Scientific ] ] 
                          , loosePhoEffHi  :: [ [ Scientific ] ] 
                          , tightPhoEffHi  :: [ [ Scientific ] ] 
                          } 

data BJetEfficiency = BJetEfficiency 
                        { bTagEffPtBins :: [ Scientific ] 
                        , bTagRejPtBins :: [ Scientific ] 
                        , bTagEffEtaBins :: [ Scientific ] 
                        , bTagRejEtaBins :: [ Scientific ]
                        , nBEeta :: Int 
                        , nBReta :: Int
                        , nBEpt :: Int
                        , nBRpt :: Int 
                        , bTagEffSV50 :: [ [ Scientific ] ]
                        , bTagEffJP50 :: [ [ Scientific ] ]
                        , bTagEffJP70 :: [ [ Scientific ] ]
                        , bTagRejSV50 :: [ [ Scientific ] ]
                        , bTagRejJP50 :: [ [ Scientific ] ]
                        , bTagRejJP70 :: [ [ Scientific ] ]  
                        } 

data MuonEfficiency = MuonEfficiency
                        { muPtBins :: [ Scientific ] 
                        , muEtaBins :: [ Scientific ] 
                        , nMuPt :: Int
                        , nMuEta :: Int 
                        , cB1MuEff :: [ [ Scientific ] ] 
                        , cB2MuEff :: [ [ Scientific ] ]
                        , sT1MuEff :: [ [ Scientific ] ] 
                        , sT2MuEff :: [ [ Scientific ] ]
                        }

data JetEfficiency = JetEfficiency 
                       { jetPtBins :: [ Scientific ]
                       , jetEtaBins :: [ Scientific ] 
                       , nJetPt :: Int
                       , nJetEta :: Int
                       , jetEff :: [ [ Scientific ] ] 
                       } 

data TauEfficiency = TauEfficiency
                       { tauEffPtBins :: [ Scientific ] 
                       , tauEffEtaBins :: [ Scientific ] 
                       , nTEPt :: Int
                       , nTEEta :: Int 
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
                       , nTRPt :: Int
                       , nTREta :: Int
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


data ATLASInfo = ATLASInfo { elecEff :: ElectronEfficiency 
                           , phoEff :: PhotonEfficiency 
                           , bJetEff :: BJetEfficiency 
                           , muonEff :: MuonEfficiency
                           }

mkElectronEfficiency :: ElectronEfficiency -> YamlValue
mkElectronEfficiency ElectronEfficiency {..} = 
    YObject $ [ ("ElePtBins", mkInline elePtBins)
            , ("EleEtaBins", mkInline eleEtaBins)
            , ("nEleEta", (YPrim . YInteger) nEleEta) 
            , ("nElePt" , (YPrim . YInteger) nElePt)
            , ("TightEleEff", mkWrap (map mkInline tightEleEff) ) 
            , ("MediumEleEff", mkWrap (map mkInline mediumEleEff) )
            , ("LooseEleEff", mkWrap (map mkInline looseEleEff) )
            ]

mkPhotonEfficiency :: PhotonEfficiency -> YamlValue
mkPhotonEfficiency PhotonEfficiency {..} = 
    YObject $ [ ("PhoLowPtBins", mkInline phoLowPtBins)
              , ("PhoHighPtBins", mkInline phoHighPtBins)
              , ("PhoEtaBins", mkInline phoEtaBins) 
              , ("nPhoEta", (YPrim . YInteger) nPhoEta) 
              , ("nPhoPtLo", (YPrim . YInteger) nPhoPtLo)
              , ("nPhoPtHi", (YPrim . YInteger) nPhoPtHi)
              , ("LoosePhoEffLow", mkWrap (map mkInline loosePhoEffLow) ) 
              , ("TightPhoEffLow", mkWrap (map mkInline tightPhoEffLow) )
              , ("LoosePhoEffHi", mkWrap (map mkInline loosePhoEffLow) )
              , ("TightPhoEffHi", mkWrap (map mkInline loosePhoEffLow) )
              ] 

mkBJetEfficiency :: BJetEfficiency -> YamlValue
mkBJetEfficiency BJetEfficiency {..} = 
    YObject $ [ ( "BtagEffPtBins", mkInline bTagEffPtBins )
              , ( "BTagRejPtBins", mkInline bTagRejPtBins )
              , ( "BTagEffEtaBins", mkInline bTagEffEtaBins )
              , ( "BTagRejEtaBins", mkInline bTagRejEtaBins ) 
              , ( "nBEeta", (YPrim . YInteger) nBEeta )
              , ( "nBReta", (YPrim . YInteger) nBReta ) 
              , ( "nBEpt", (YPrim . YInteger) nBEpt )
              , ( "nBRpt", (YPrim . YInteger) nBRpt )
              , ( "BtagEffSV50", mkWrap (map mkInline bTagEffSV50) )
              , ( "BtagEffJP50", mkWrap (map mkInline bTagEffJP50) )
              , ( "BtagEffJP70", mkWrap (map mkInline bTagEffJP70) )
              , ( "BtagRejSV50", mkWrap (map mkInline bTagRejSV50) )
              , ( "BtagRejJP50", mkWrap (map mkInline bTagRejJP50) )
              , ( "BtagRejJP70", mkWrap (map mkInline bTagRejJP70) )
              ] 

mkMuonEfficiency :: MuonEfficiency -> YamlValue
mkMuonEfficiency MuonEfficiency {..} = 
  YObject $ [ ( "MuPtBins", mkInline muPtBins ) 
            , ( "MuEtaBins", mkInline muEtaBins ) 
            , ( "nMuPt", (YPrim . YInteger) nMuPt) 
            , ( "nMuEta", (YPrim . YInteger) nMuEta) 
            , ( "CB1MuEff", mkWrap (map mkInline cB1MuEff) )
            , ( "CB2MuEff", mkWrap (map mkInline cB2MuEff) )
            , ( "ST1MuEff", mkWrap (map mkInline sT1MuEff) )
            , ( "ST2MuEff", mkWrap (map mkInline sT2MuEff) )
            ] 

mkJetEfficiency :: JetEfficiency -> YamlValue
mkJetEfficiency JetEfficiency {..} = 
  YObject $ [ ( "JetPtBins", mkInline jetPtBins )
            , ( "jetEtaBins", mkInline jetEtaBins )
            , ( "nJetPt", (YPrim . YInteger) nJetPt )
            , ( "nJetEta", (YPrim . YInteger) nJetEta )
            , ( "jetEff", mkWrap (map mkInline jetEff) )
            ] 

mkTauEfficiency :: TauEfficiency -> YamlValue
mkTauEfficiency TauEfficiency {..} = 
  YObject $ [ ( "TauEffPtBins", mkInline tauEffPtBins )
            , ( "TauEffEtaBins", mkInline tauEffEtaBins )
            , ( "nTEPt", (YPrim . YInteger) nTEPt )
            , ( "nTEEta", (YPrim . YInteger) nTEEta ) 
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
            , ( "nTRPt", (YPrim . YInteger) nTRPt )
            , ( "nTREta", (YPrim . YInteger) nTREta )
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


mkATLAS :: ATLASInfo -> YamlValue 
mkATLAS ATLASInfo {..} = 
    YObject $ [ ( "ElectronEfficiency", mkElectronEfficiency elecEff )  
              , ( "PhotonEfficiency", mkPhotonEfficiency phoEff ) 
              , ( "BJetEfficiency", mkBJetEfficiency bJetEff )
              , ( "MuonEfficiency", mkMuonEfficiency muonEff ) 
              ] 



atlasElecEff :: ElectronEfficiency
atlasElecEff = ElectronEfficiency 
  { elePtBins = [4.0, 7.0, 10.0, 15.0, 20.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 80.0]
  , eleEtaBins = [-2.5, -2.0, -1.52, -1.37, -0.75, 0.0, 0.75, 1.37, 1.52, 2.0, 2.5]
  , nEleEta = 10
  , nElePt = 12 
  , tightEleEff = 
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
  , mediumEleEff = 
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
  , looseEleEff = 
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

atlasPhoEff :: PhotonEfficiency
atlasPhoEff = PhotonEfficiency 
  { phoLowPtBins = [15.0, 18.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 60.0, 80.0, 100.0]
  , phoHighPtBins = [150.0, 200.0, 250.0, 300.0, 350.0, 400.0, 450.0, 500.0] 
  , phoEtaBins = [ -2.4, -2.2, -2.0, -1.8, -1.52, -1.37, -1.2, -1.0, -0.8, -0.6, -0.4, -0.2, -0.1, 0.0, 0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.37, 1.52, 1.8, 2.0, 2.2, 2.4 ] 
  , nPhoEta = 26
  , nPhoPtLo = 11
  , nPhoPtHi = 8 
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

atlasBJetEff :: BJetEfficiency
atlasBJetEff = BJetEfficiency 
  { bTagEffPtBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ] 
  , bTagRejPtBins = [ 20.0, 25.0, 40.0, 60.0, 90.0, 140.0, 200.0, 300.0, 500.0 ] 
  , bTagEffEtaBins = [ 0.0, 1.2, 2.5 ] 
  , bTagRejEtaBins = [ 0.0, 1.2, 2.5 ] 
  , nBEeta = 2
  , nBReta = 2
  , nBEpt = 14
  , nBRpt = 8
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

atlasMuonEff :: MuonEfficiency 
atlasMuonEff = MuonEfficiency
  { muPtBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ]
  , muEtaBins = [ -2.5, -2.25, -2.0, -1.75, -1.50, -1.25, -1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5 ]
  , nMuPt = 8
  , nMuEta = 20 
  

  } 
 