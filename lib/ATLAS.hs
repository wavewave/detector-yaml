{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ATLAS where

import qualified Data.HashMap.Strict as HM
--
import Detector.Type

atlas2011 :: DetectorDescription ImportList
atlas2011 = DetectorDescription 
            { detectorName = "ATLAS2011"
            , detectorDescription = "ATLAS 2011 detector description"
            , detectorReference = "arXiv:xxxx.yyyy"
            , detectorComment = "extracted the efficiencies from the plot 3,4,5 in the reference" 
            , detectorValidationInfo = "Validated on 2014/02" 
            , detectorRange = atlas2011Range
            , detectorIdentification = atlas2011Identify
            , detectorSmearing = atlas2011Smearing
            }

atlas2011Range :: RangeDescription ImportList
atlas2011Range = (RangeDescription . ImportList) 
                   [ Left (Import "Range_Full_ATLAS")
                   , Left (Import "Range_ECal_ATLAS")
                   , Left (Import "Range_MuonDetector_ATLAS")
                   , Left ( Import "Range_HCal_ATLAS")
                   , Left ( Import "Range_Muon_Combined_ATLAS")
                   , Left ( Import "Range_Muon_ID_ATLAS")
                   , Left ( Import "Range_Muon_MS_ATLAS")
                   , Left ( Import "Range_Electron_Loose_ATLAS")
                   , Left ( Import "Range_Electron_Medium_ATLAS")
                   , Left ( Import "Range_Electron_Tight_ATLAS")
                   , Left ( Import "Range_Photon_Loose_ATLAS")
                   , Left ( Import "Range_Photon_Tight_ATLAS")
                   , Left ( Import "Range_Jet_Topo_ATLAS")
                   , Left ( Import "Range_Track_ATLAS")
                   , Left ( Import "Range_BJet_ATLAS")
                   , Left ( Import "Range_Tau_ATLAS")
                   ] 
                                                

atlas2011Identify :: IdentificationDescription ImportList 
atlas2011Identify = IdentificationDescription 
  { electron = ImportList [ Left (Import "Electron_Tight_ATLAS")
                          , Left (Import "Electron_Medium_ATLAS")
                          , Left (Import "Electron_Loose_ATLAS") ]
  , photon   = ImportList [ Left (Import "Photon_Tight_ATLAS")
                          , Left (Import "Photon_Loose_ATLAS") ]
  , bJet     = ImportList [ Left (Import "BJet_SV50_ATLAS")
                          , Left (Import "BJet_JP50_ATLAS")
                          , Left (Import "BJet_JP70_ATLAS") ]
  , muon     = ImportList [ Left (Import "Muon_CB1_ATLAS")
                          , Left (Import "Muon_CB2_ATLAS")
                          , Left (Import "Muon_ST1_ATLAS") 
                          , Left (Import "Muon_ST2_ATLAS") ]
  , jet      = ImportList [ Left (Import "Jet_ATLAS") ]
  , tau      = ImportList [ Left (Import "Tau_Cut_Loose_ATLAS")
                          , Left (Import "Tau_Cut_Medium_ATLAS")
                          , Left (Import "Tau_Cut_Tight_ATLAS") 
                          , Left (Import "Tau_Lik_Loose_ATLAS") 
                          , Left (Import "Tau_Lik_Medium_ATLAS") 
                          , Left (Import "Tau_Lik_Tight_ATLAS") 
                          , Left (Import "Tau_BDT_Loose_ATLAS")
                          , Left (Import "Tau_BDT_Medium_ATLAS") 
                          , Left (Import "Tau_BDT_Tight_ATLAS") ]
  , track    = Nothing
  , ptThresholds = ImportList [Left (Import "ATLAS_PTThreshold")]
  }


atlas2011Smearing :: SmearingDescription ImportList
atlas2011Smearing = SmearingDescription 
  { smearElectron = ImportList [ Left (Import "Smear_Electron_ATLAS") ]
  , smearPhoton   = ImportList [ Left (Import "Smear_Photon_ATLAS") ]
  , smearMuon     = ImportList [ Left (Import "Smear_Muon_ATLAS") ]
  , smearJet      = ImportList [ Left (Import "Smear_TopoJet_ATLAS") ]
  , smearTrack    = ImportList [ Left (Import "Smear_Track_ATLAS") ]
  , smearTau      = ImportList [ Left (Import "Smear_Tau_ATLAS") ]
  , smearMET      = ImportList [ Left (Import "Smear_MissingET_ATLAS") ]
  }


atlasEleDataTight :: ElectronEffData
atlasEleDataTight = ElectronEffData
  { eleName = "Electron_Tight_ATLAS"
  , eleMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "Tight electron object 2011 ATLAS"
      , comment = "We use table from reference" 
      , reference = "arXiv:xxxx.yyyy" }
  , eleEfficiency = PTEtaGrid 
      { isEtaSymmetric = False
      , ptBins = [4.0, 7.0, 10.0, 15.0, 20.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 80.0]
      , etaBins = [-2.5, -2.0, -1.52, -1.37, -0.75, 0.0, 0.75, 1.37, 1.52, 2.0, 2.5]
      , grid = atlasElecTightEff
      }
  } 

atlasEleDataMedium :: ElectronEffData
atlasEleDataMedium = ElectronEffData
  { eleName = "Electron_Medium_ATLAS"
  , eleMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "Medium electron object 2011 ATLAS"
      , comment = "We use table from reference" 
      , reference = "arXiv:xxxx.yyyy" }
  , eleEfficiency = PTEtaGrid
      { isEtaSymmetric = False
      , ptBins = [4.0, 7.0, 10.0, 15.0, 20.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 80.0]
      , etaBins = [-2.5, -2.0, -1.52, -1.37, -0.75, 0.0, 0.75, 1.37, 1.52, 2.0, 2.5]
      , grid = atlasElecMediumEff
      }
  } 

atlasEleDataLoose :: ElectronEffData
atlasEleDataLoose = ElectronEffData
  { eleName = "Electron_Loose_ATLAS"
  , eleMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "Loose electron object 2011 ATLAS"
      , comment = "We use table from reference" 
      , reference = "arXiv:xxxx.yyyy" }
  , eleEfficiency = PTEtaGrid
      { isEtaSymmetric = False
      , ptBins = [4.0, 7.0, 10.0, 15.0, 20.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 80.0]
      , etaBins = [-2.5, -2.0, -1.52, -1.37, -0.75, 0.0, 0.75, 1.37, 1.52, 2.0, 2.5]
      , grid = atlasElecLooseEff
      }
  } 

atlasElecTightEff :: Grid
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

atlasElecMediumEff :: Grid
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

atlasElecLooseEff :: Grid
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
  { phoName = "Photon_Loose_ATLAS"
  , phoMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "Loose photon object 2011 ATLAS"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , phoEfficiency = PTEtaGrid 
      { isEtaSymmetric = False 
      , ptBins = [15.0, 18.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 60.0, 80.0, 100.0, 150.0, 200.0, 250.0, 300.0, 350.0, 400.0, 450.0, 500.0] 
      , etaBins = [ -2.4, -2.2, -2.0, -1.8, -1.52, -1.37, -1.2, -1.0, -0.8, -0.6, -0.4, -0.2, -0.1, 0.0, 0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.37, 1.52, 1.8, 2.0, 2.2, 2.4 ] 
      , grid = GridConst { gridConst = 1.0 } }
  }

atlasPhoDataTight :: PhotonEffData
atlasPhoDataTight = PhotonEffData
  { phoName = "Photon_Tight_ATLAS"
  , phoMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "Tight photon object 2011 ATLAS"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , phoEfficiency = PTEtaGrid 
      { isEtaSymmetric = False
      , ptBins = [15.0, 18.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 60.0, 80.0, 100.0, 150.0, 200.0, 250.0, 300.0, 350.0, 400.0, 450.0, 500.0] 
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
      { isEtaSymmetric = True
      , ptBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ] 
      , etaBins = [ 0.0, 1.2, 2.5 ] 
      , grid = GridConst { gridConst = 0.5 } }
  , bJetRejection = PTEtaGrid
      { isEtaSymmetric = True
      , ptBins = [ 20.0, 25.0, 40.0, 60.0, 90.0, 140.0, 200.0, 300.0, 500.0 ]
      , etaBins = [ 0.0, 1.2, 2.5 ]
      , grid = GridConst { gridConst = 100.0 } }
  }


atlasBJetDataJP50 :: BJetEffData
atlasBJetDataJP50 = BJetEffData
  { bJetName = "BJet_JP50_ATLAS"
  , bJetMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "JP50 ATLAS BJet Tagging\nthis is next line."
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , bJetEfficiency = PTEtaGrid 
      { isEtaSymmetric = True
      , ptBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ] 
      , etaBins = [ 0.0, 1.2, 2.5 ] 
      , grid =  GridFull 
          { gridData =  
              [ [ 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5 ]
              , [ 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5 ] ] } }
  , bJetRejection = PTEtaGrid
      { isEtaSymmetric = True
      , ptBins = [ 20.0, 25.0, 40.0, 60.0, 90.0, 140.0, 200.0, 300.0, 500.0 ]
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
      { isEtaSymmetric = True
      , ptBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ] 
      , etaBins = [ 0.0, 1.2, 2.5 ] 
      , grid =  GridConst { gridConst = 0.7 } }
  , bJetRejection = PTEtaGrid
      { isEtaSymmetric = True
      , ptBins = [ 20.0, 25.0, 40.0, 60.0, 90.0, 140.0, 200.0, 300.0, 500.0 ]
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
      { isEtaSymmetric = False
      , ptBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ]
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
      { isEtaSymmetric = False
      , ptBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ]
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
      { isEtaSymmetric = False
      , ptBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ]
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
      { isEtaSymmetric = False
      , ptBins = [ 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 70.0, 100.0 ]
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
      { isEtaSymmetric = False
      , ptBins = [ 20.0, 30.0, 40.0, 60.0, 80.0, 120.0, 160.0, 200.0, 280.0, 360.0, 500.0, 600.0, 900.0, 1200.0, 2000.0 ] 
      , etaBins = [ -4.5, -3.6, -2.8, -2.5, -2.0, -1.2, -0.8, -0.3, 0.0, 0.3, 0.8, 1.2, 2.0, 2.5, 2.8, 3.6, 4.5 ]
      , grid = GridConst { gridConst = 1.0 }
      }
  } 

atlasTauDataCutLoose :: TauEffData 
atlasTauDataCutLoose = TauEffData 
  { tauName = "Tau_Cut_Loose_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau Cut Loose"
      , comment = "We use table from reference\nThis is the next line."
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "Cut"
  , tauEfficiency = Tau1or3Prong 
      { tau1ProngEff = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      }
  }

atlasTauDataCutMedium :: TauEffData 
atlasTauDataCutMedium = TauEffData 
  { tauName = "Tau_Cut_Medium_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau Cut Medium"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "Cut"
  , tauEfficiency = Tau1or3Prong 
      { tau1ProngEff = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
          { isEtaSymmetric = True 
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      }
  }

atlasTauDataCutTight :: TauEffData 
atlasTauDataCutTight = TauEffData 
  { tauName = "Tau_Cut_Tight_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau Cut Tight"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "Cut"
  , tauEfficiency = Tau1or3Prong 
      { tau1ProngEff = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      } 
  }

atlasTauDataLikLoose :: TauEffData 
atlasTauDataLikLoose = TauEffData 
  { tauName = "Tau_Lik_Loose_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau Likelihood Loose"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "Likelihood"
  , tauEfficiency = Tau1or3Prong 
      { tau1ProngEff = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      }
  }

atlasTauDataLikMedium :: TauEffData 
atlasTauDataLikMedium = TauEffData 
  { tauName = "Tau_Lik_Medium_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau Likelihood Medium"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "Likelihood"
  , tauEfficiency = Tau1or3Prong 
      { tau1ProngEff = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      }
  }

atlasTauDataLikTight :: TauEffData 
atlasTauDataLikTight = TauEffData 
  { tauName = "Tau_Lik_Tight_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau Likelihood Tight"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "Likelihood"
  , tauEfficiency = Tau1or3Prong 
      { tau1ProngEff = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      }
  }

atlasTauDataBDTLoose :: TauEffData 
atlasTauDataBDTLoose = TauEffData 
  { tauName = "Tau_BDT_Loose_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau BDT Loose"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "BoostedDecisionTree"
  , tauEfficiency = Tau1or3Prong 
      { tau1ProngEff = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      }

  }

atlasTauDataBDTMedium :: TauEffData 
atlasTauDataBDTMedium = TauEffData 
  { tauName = "Tau_BDT_Medium_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau BDT Medium"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "BoostedDecisionTree"
  , tauEfficiency = Tau1or3Prong 
      { tau1ProngEff = PTEtaGrid
          { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
	  { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
	  , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
	  , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
	  { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
	  , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
	  , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
	  { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
	  , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
	  , grid = GridConst { gridConst = 1.0 } }
      }

  }

atlasTauDataBDTTight :: TauEffData 
atlasTauDataBDTTight = TauEffData 
  { tauName = "Tau_BDT_Tight_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau Likelihood Tight"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "BoostedDecisionTree"
  , tauEfficiency = Tau1or3Prong 
      { tau1ProngEff = PTEtaGrid
	  { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
	  , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
	  , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
	  { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
	  , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
	  , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
	  { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
	  , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
	  , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
	  { isEtaSymmetric = True
          , ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
	  , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
	  , grid = GridConst { gridConst = 1.0 } }
      }
  }


atlasPTThresholds :: PTThresholds
atlasPTThresholds = PTThresholds  
  { pTThreName = "ATLAS_PTThreshold"
  , muPTMin = 20.0
  , elePTMin = 5.0
  , phoPTMin = 20.0
  , jetPTMin = 20.0
  , bJetPTMin = 20.0
  , trkPTMin = 0.5
  , tauPTMin = 5.0
  } 

atlasSmearElectron :: SmearData TElectron
atlasSmearElectron = SmearData
                       "Smear_Electron_ATLAS"  
                       MetaInfo { tag = "ATLAS", description = "electron", comment = "table", reference = "XXX" } 
                       PTEtaInterpolation { isEtaSymmetric = True, interpol = IPConstant 1.0 } 

atlasSmearPhoton :: SmearData TPhoton
atlasSmearPhoton = SmearData
                       "Smear_Photon_ATLAS"  
                       MetaInfo { tag = "ATLAS", description = "photon", comment = "table", reference = "XXX" } 
                       PTEtaInterpolation { isEtaSymmetric = True, interpol = IPConstant 1.0 }  

atlasSmearMuon :: SmearData TMuon
atlasSmearMuon = SmearData
                    "Smear_Muon_ATLAS"  
                    MetaInfo { tag = "ATLAS", description = "muon", comment = "table", reference = "XXX" } 
                    PTEtaInterpolation { isEtaSymmetric = True, interpol = IPConstant 1.0 } 

atlasSmearTopoJet :: SmearData TJet
atlasSmearTopoJet = SmearData
                      "Smear_TopoJet_ATLAS"  
                      MetaInfo { tag = "ATLAS", description = "topojet", comment = "table", reference = "XXX" } 
                      PTEtaInterpolation
                        { isEtaSymmetric = True
                        , interpol = IPPredefinedMode3 
                            { seriesBA = [ FuncBin 0    (HM.fromList [ (-2,  9.476216187754203)
                                                                     , (-1, -0.16939888048822812)
                                                                     , ( 0,  0.01096643215740863)
                                                                     , ( 1, -0.00001147146295333292)
                                                                 , ( 2,  1.9289334367006085e-8)
                                                                     , ( 3, -1.5000987275723775e-12) ])
                                         , FuncBin 0.75 (HM.fromList [ (-2,  8.197400117302609)
                                                                     , (-1, -0.05636233086517818) 
                                                                     , ( 0,  0.010365438976501047)
                                                                     , ( 1, -0.0000020835685322585434)
                                                                     , ( 2,  2.001368792089794e-9)
                                                                     , ( 3,  6.012078707551757e-12) ])
                                         , FuncBin 1.25 (HM.fromList [ (-2,  6.446193732566276) 
                                                                     , (-1, -0.010586512399379697)
                                                                     , ( 0,  0.016592828729054618)
                                                                     , ( 1, -0.000008966344163685219)
                                                                     , ( 2,  6.867359722322189e-10)
                                                                     , ( 3,  8.093547934486339e-12) ])
                                         , FuncBin 1.75 (HM.fromList [ (-2, -0.8554617719091405)
                                                                     , (-1,  0.4246942698759444)
                                                                     , ( 0,  0.018887675539395778)
                                                                     , ( 1,  0.0000008827096751274164)
                                                                     , ( 2, -3.395818453325127e-9)
                                                                     , ( 3, -3.808692671281164e-12) ])
                                         , FuncBin 2.25 (HM.fromList [ (-2, -6.078548157133825)
                                                                     , (-1,  0.6523709619986131)
                                                                     , ( 0,  0.02566741878632772)
                                                                     , ( 1, -0.000004513118203117088)
                                                                     , ( 2,  1.5015469175097658e-9)
                                                                     , ( 3,  8.156483941322221e-12) ])
                                         , FuncBin 2.75 (HM.fromList [ (-2, -9.496101460575369)
                                                                     , (-1,  0.7866574724505102)
                                                                     , ( 0,  0.031548358061336486)
                                                                     , ( 1, -0.000007095429220069707)
                                                                     , ( 2,  2.099191724700867e-9)
                                                                     , ( 3,  1.1359410993905745e-11) ])
                                         , FuncBin 3.25 (HM.fromList [ (-2, -4.5274342332395815)
                                                                     , (-1,  0.43592198252621316)
                                                                     , ( 0,  0.040108297897736325)
                                                                     , ( 1,  0.00002082414790810237)
                                                                     , ( 2, -5.43013214981909e-9)
                                                                     , ( 3, -1.529328127051232e-11) ])
                                         , FuncBin 3.75 (HM.fromList [ (-2, -1.1160615726947604)
                                                                     , (-1,  0.34705420681539366) 
                                                                     , ( 0,  0.04184642899559342)
                                                                     , ( 1,  0.00002971363284633445)
                                                                     , ( 2, -7.670934703489684e-9 )
                                                                     , ( 3, -2.3710958935037943e-11) ])
                                         ] 
                            , etaBound = 4.0 
                            }
                        }
                     

atlasSmearTrack :: SmearData TTrack
atlasSmearTrack = SmearData
                    "Smear_Track_ATLAS"  
                    MetaInfo { tag = "ATLAS", description = "track", comment = "table", reference = "XXX" } 
                    PTEtaInterpolation { isEtaSymmetric = True, interpol = IPConstant 1.0 } 

atlasSmearTau :: SmearData TTau
atlasSmearTau = SmearData
                  "Smear_Tau_ATLAS"  
                  MetaInfo { tag = "ATLAS", description = "tau", comment = "table", reference = "XXX" } 
                  PTEtaInterpolation { isEtaSymmetric = True, interpol = IPConstant 1.0 }

atlasSmearMET :: SmearData TMET
atlasSmearMET = SmearData
                  "Smear_MissingET_ATLAS"  
                  MetaInfo { tag = "ATLAS", description = "missingET", comment = "table", reference = "XXX" } 
                  PTEtaInterpolation { isEtaSymmetric = True, interpol = IPConstant 1.0 }

