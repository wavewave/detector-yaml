{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ATLAS where

import Detector.Type

atlas2011 :: DetectorDescription (Either Import)
atlas2011 = DetectorDescription 
            { detectorName = "ATLAS2011"
            , detectorDescription = "ATLAS 2011 detector description"
            , detectorReference = "arXiv:xxxx.yyyy"
            , detectorComment = "extracted the efficiencies from the plot 3,4,5 in the reference" 
            , detectorValidationInfo = "Validated on 2014/02" 
            , detectorObject = atlas2011Object }

atlas2011Object :: ObjectDescription (Either Import)
atlas2011Object = ObjectDescription 
  { electron = Right atlasEleDataTight -- Left (Import "Electron_Loose_ATLAS") 
  , photon = Right atlasPhoDataTight --  Left (Import "Photon_Tight_ATLAS")
  , bJet = Right atlasBJetDataSV50 -- Left (Import "BJet_JP50_ATLAS")
  , muon = Right atlasMuonDataCB1 -- Left (Import "Muon_CB1_ATLAS")
  , jet = Right atlasJetData -- Left (Import "Jet_ATLAS")
  , tau = Right atlasTauDataCutLoose 
           -- Left (Import "Tau_BDT_Tight_ATLAS")
  , track = Nothing
  , ptThresholds = Right atlasPTThresholds 
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
      { ptBins = [4.0, 7.0, 10.0, 15.0, 20.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 80.0]
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
      { ptBins = [4.0, 7.0, 10.0, 15.0, 20.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 80.0]
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
      { ptBins = [4.0, 7.0, 10.0, 15.0, 20.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 80.0]
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
      { ptBins = [15.0, 18.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 60.0, 80.0, 100.0, 150.0, 200.0, 250.0, 300.0, 350.0, 400.0, 450.0, 500.0] 
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
      , description = "JP50 ATLAS BJet Tagging\nthis is next line."
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
      , description = "ATLAS Tau Cut Loose"
      , comment = "We use table from reference\nThis is the next line."
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "Cut"
  , tauEfficiency = Tau1or3Prong 
      { tau1ProngEff = PTEtaGrid
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
         , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
         , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
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
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
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
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
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
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
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
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
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
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
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
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
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
          { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
          , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
	  { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
	  , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
	  , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
	  { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
	  , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
	  , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
	  { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
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
	  { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
	  , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
	  , grid = GridConst { gridConst = 1.0 } }
      , tau1ProngRej = PTEtaGrid 
	  { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
	  , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]
	  , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngEff = PTEtaGrid
	  { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
	  , etaBins = [ 0.0, 1.3, 1.6, 2.5 ]  
	  , grid = GridConst { gridConst = 1.0 } }
      , tau3ProngRej = PTEtaGrid
	  { ptBins = [ 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
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