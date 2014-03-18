{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CMS where

import Detector.Type

cms2011 :: DetectorDescription
cms2011 = DetectorDescription 
            { detectorName = "CMS2011"
            , detectorDescription = "CMS 2011 detector description"
            , detectorReference = "arXiv:xxxx.yyyy"
            , detectorComment = "extracted the efficiencies from the plot 3,4,5 in the reference" 
            , detectorValidationInfo = "Validated on 2014/02" 
            , detectorObject = cms2011Object }

cms2011Object :: ObjectDescription 
cms2011Object = ObjectDescription 
  { electron     = Left (Import "Electron_PF_CMS")
  , photon       = Left (Import "Photon_PF_CMS")
  , bJet         = Left (Import "BJet_TCHEL_CMS")
  , muon         = Left (Import "Muon_S_CMS")
  , jet          = Left (Import "Jet_PF_CMS")
  , tau          = Left (Import "Tau_TaNCL_CMS")
  , track        = Just (Left (Import "Track_CMS"))
  , ptThresholds = Right cmsPTThresholds 
  }

cmsBTagTCHEL :: BJetEffData
cmsBTagTCHEL = BJetEffData 
  { bJetName = "BJet_TCHEL_CMS"
  , bJetMetaInfo = MetaInfo
      { tag = "CMS"
      , description = "CMS BJet Tagging TCHEL"
      , comment = "table X" 
      , reference = "arXiv:xxxx.yyyy"
      }
  , bJetEfficiency = PTEtaGrid
      { ptBins = [ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 100.0, 120.0, 240.0 ] 
      , etaBins = [ 0.0, 1.2, 2.4 ] 
      , grid = GridConst { gridConst = 0.7 } }
  , bJetRejection = PTEtaGrid
      { ptBins = [ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0, 110.0 ] 
      , etaBins = [ 0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4 ] 
      , grid = GridConst { gridConst = 100.0 } } 
  }

cmsBTagSSVHPT :: BJetEffData
cmsBTagSSVHPT = BJetEffData 
  { bJetName = "BJet_SSVHPT_CMS"
  , bJetMetaInfo = MetaInfo
      { tag = "CMS"
      , description = "CMS BJet Tagging SSVHPT"
      , comment = "table X" 
      , reference = "arXiv:xxxx.yyyy"
      }
  , bJetEfficiency = PTEtaGrid
      { ptBins = [ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 100.0, 120.0, 240.0 ] 
      , etaBins = [ 0.0, 1.2, 2.4 ] 
      , grid = GridConst { gridConst = 0.5 } }
  , bJetRejection = PTEtaGrid
      { ptBins = [ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0, 110.0 ] 
      , etaBins = [ 0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4 ] 
      , grid = GridConst { gridConst = 100.0 } } 
  }

cmsBTagSSVHEM :: BJetEffData
cmsBTagSSVHEM = BJetEffData 
  { bJetName = "BJet_SSVHEM_CMS"
  , bJetMetaInfo = MetaInfo
      { tag = "CMS"
      , description = "CMS BJet Tagging SSVHEM"
      , comment = "table X" 
      , reference = "arXiv:xxxx.yyyy"
      }
  , bJetEfficiency = PTEtaGrid
      { ptBins = [ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 100.0, 120.0, 240.0 ] 
      , etaBins = [ 0.0, 1.2, 2.4 ] 
      , grid = GridConst { gridConst = 0.825 } }
  , bJetRejection = PTEtaGrid
      { ptBins = [ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0, 110.0 ] 
      , etaBins = [ 0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4 ] 
      , grid = GridConst { gridConst = 10.0 } } 
  }

cmsMuonS :: MuonEffData
cmsMuonS = MuonEffData
  { muonName = "Muon_S_CMS"
  , muonMetaInfo = MetaInfo
      { tag = "CMS"
      , description = "CMS S Muon"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , muonEfficiency = PTEtaGrid
      { ptBins = [ 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 7.0, 9.0, 11.0, 14.0, 17.0, 20.0, 30.0, 40.0, 60.0, 100.0 ] 
      , etaBins = [ -2.4, -2.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0, 2.4 ] 
      , grid = GridConst { gridConst = 1.0 }  
      }       
  }

cmsMuonP :: MuonEffData
cmsMuonP = MuonEffData
  { muonName = "Muon_P_CMS"
  , muonMetaInfo = MetaInfo
      { tag = "CMS"
      , description = "CMS P Muon"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , muonEfficiency = PTEtaGrid
      { ptBins = [ 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 7.0, 9.0, 11.0, 14.0, 17.0, 20.0, 30.0, 40.0, 60.0, 100.0 ] 
      , etaBins = [ -2.4, -2.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0, 2.4 ] 
      , grid = GridConst { gridConst = 1.0 }  
      }       
  }

cmsMuonT :: MuonEffData
cmsMuonT = MuonEffData
  { muonName = "Muon_T_CMS"
  , muonMetaInfo = MetaInfo
      { tag = "CMS"
      , description = "CMS T Muon"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , muonEfficiency = PTEtaGrid
      { ptBins = [ 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 7.0, 9.0, 11.0, 14.0, 17.0, 20.0, 30.0, 40.0, 60.0, 100.0 ] 
      , etaBins = [ -2.4, -2.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0, 2.4 ] 
      , grid = GridConst { gridConst = 1.0 }  
      }       
  }

cmsElePF :: ElectronEffData
cmsElePF = ElectronEffData
  { eleName = "Electron_PF_CMS"
  , eleMetaInfo = MetaInfo 
      { tag = "CMS"
      , description = "electron PF CMS"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , eleEfficiency = PTEtaGrid
      { ptBins = [ 15.0, 17.5, 20.0, 30.0, 40.0, 50.0, 150.0 ] 
      , etaBins = [ -2.5, -1.56, -1.442, 0.0, 1.442, 1.56, 2.5 ] 
      , grid = GridConst { gridConst = 1.0 } }
  }

cmsEleCicSTight :: ElectronEffData
cmsEleCicSTight = ElectronEffData
  { eleName = "Electron_CicSTight_CMS"
  , eleMetaInfo = MetaInfo 
      { tag = "CMS"
      , description = "electron CicSTight CMS"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , eleEfficiency = PTEtaGrid
      { ptBins = [ 15.0, 17.5, 20.0, 30.0, 40.0, 50.0, 150.0 ] 
      , etaBins = [ -2.5, -1.56, -1.442, 0.0, 1.442, 1.56, 2.5 ] 
      , grid = GridConst { gridConst = 1.0 } }
  }

cmsEleCicLoose :: ElectronEffData
cmsEleCicLoose = ElectronEffData
  { eleName = "Electron_CicLoose_CMS"
  , eleMetaInfo = MetaInfo 
      { tag = "CMS"
      , description = "electron CicLoose CMS"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , eleEfficiency = PTEtaGrid
      { ptBins = [ 15.0, 17.5, 20.0, 30.0, 40.0, 50.0, 150.0 ] 
      , etaBins = [ -2.5, -1.56, -1.442, 0.0, 1.442, 1.56, 2.5 ] 
      , grid = GridConst { gridConst = 1.0 } }
  }

cmsEleWP80 :: ElectronEffData
cmsEleWP80 = ElectronEffData
  { eleName = "Electron_CicLoose_CMS"
  , eleMetaInfo = MetaInfo 
      { tag = "CMS"
      , description = "electron WP80 CMS"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , eleEfficiency = PTEtaGrid
      { ptBins = [ 15.0, 17.5, 20.0, 30.0, 40.0, 50.0, 150.0 ] 
      , etaBins = [ -2.5, -1.56, -1.442, 0.0, 1.442, 1.56, 2.5 ] 
      , grid = GridConst { gridConst = 1.0 } }
  }

cmsEleWP95 :: ElectronEffData
cmsEleWP95 = ElectronEffData
  { eleName = "Electron_CicLoose_CMS"
  , eleMetaInfo = MetaInfo 
      { tag = "CMS"
      , description = "electron WP95 CMS"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , eleEfficiency = PTEtaGrid
      { ptBins = [ 15.0, 17.5, 20.0, 30.0, 40.0, 50.0, 150.0 ] 
      , etaBins = [ -2.5, -1.56, -1.442, 0.0, 1.442, 1.56, 2.5 ] 
      , grid = GridConst { gridConst = 1.0 } }
  }

cmsPhoPF :: PhotonEffData
cmsPhoPF = PhotonEffData 
  { phoName = "Photon_PF_CMS"
  , phoMetaInfo = MetaInfo
      { tag = "CMS"
      , description = "Photon PF CMS"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , phoEfficiency = PTEtaGrid
      { ptBins = [ 20.0, 35.0, 45.0, 100.0 ] 
      , etaBins = [ -2.5, -1.56, -1.442, 0.0, 1.442, 1.56, 2.5 ] 
      , grid = GridConst { gridConst = 1.0 } }
  } 

cmsPhoTight :: PhotonEffData
cmsPhoTight = PhotonEffData 
  { phoName = "Photon_Tight_CMS"
  , phoMetaInfo = MetaInfo
      { tag = "CMS"
      , description = "Photon Tight CMS"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , phoEfficiency = PTEtaGrid
      { ptBins = [ 20.0, 35.0, 45.0, 100.0 ] 
      , etaBins = [ -2.5, -1.56, -1.442, 0.0, 1.442, 1.56, 2.5 ] 
      , grid = GridConst { gridConst = 1.0 } }
  } 

cmsPhoLoose :: PhotonEffData
cmsPhoLoose = PhotonEffData 
  { phoName = "Photon_Loose_CMS"
  , phoMetaInfo = MetaInfo
      { tag = "CMS"
      , description = "Photon Loose CMS"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , phoEfficiency = PTEtaGrid
      { ptBins = [ 20.0, 35.0, 45.0, 100.0 ] 
      , etaBins = [ -2.5, -1.56, -1.442, 0.0, 1.442, 1.56, 2.5 ] 
      , grid = GridConst { gridConst = 1.0 } }
  } 

cmsTrack :: TrackEffData
cmsTrack = TrackEffData
  { trackName = "Track_CMS"
  , trackMetaInfo = MetaInfo 
      { tag = "CMS"
      , description = "Track CMS"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , trackEfficiency = PTEtaGrid
      { ptBins = [ 0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 2.0, 4.0, 6.0, 8.0, 10.0, 20.0, 40.0, 60.0, 80.0, 100.0 ] 
      , etaBins = [ -2.5, -1.56, -1.442, 0.0, 1.442, 1.56, 2.5 ] 
      , grid = GridConst { gridConst = 1.0 } }
  }


cmsTauTaNCL :: TauEffData 
cmsTauTaNCL = TauEffData 
  { tauName = "Tau_TaNCL_CMS"
  , tauMetaInfo = MetaInfo 
      { tag = "CMS"
      , description = "CMS Tau TaNC Loose"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "TaNCL"
  , tauEfficiency = TauCombined 
      { tauCombEff = PTEtaGrid
          { ptBins = [ 15.0, 17.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.0, 1.5, 2.5 ] 
          , grid = GridConst { gridConst = 1.0 } 
          } 
      , tauCombRej = PTEtaGrid
          { ptBins = [ 15.0, 17.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100 ] 
          , etaBins = [ 0.0, 1.0, 1.5, 2.5 ] 
          , grid = GridConst { gridConst = 1.0 } 
          }
      }
  }

cmsTauTaNCM :: TauEffData 
cmsTauTaNCM = TauEffData 
  { tauName = "Tau_TaNCM_CMS"
  , tauMetaInfo = MetaInfo 
      { tag = "CMS"
      , description = "CMS Tau TaNC Medium"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "TaNCM"
  , tauEfficiency = TauCombined 
      { tauCombEff = PTEtaGrid
          { ptBins = [ 15.0, 17.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.0, 1.5, 2.5 ] 
          , grid = GridConst { gridConst = 1.0 } 
          } 
      , tauCombRej = PTEtaGrid
          { ptBins = [ 15.0, 17.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100 ] 
          , etaBins = [ 0.0, 1.0, 1.5, 2.5 ] 
          , grid = GridConst { gridConst = 1.0 } 
          }
      }
  }


cmsTauTaNCT :: TauEffData 
cmsTauTaNCT = TauEffData 
  { tauName = "Tau_TaNCT_CMS"
  , tauMetaInfo = MetaInfo 
      { tag = "CMS"
      , description = "CMS Tau TaNC Tight"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "TaNCT"
  , tauEfficiency = TauCombined 
      { tauCombEff = PTEtaGrid
          { ptBins = [ 15.0, 17.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.0, 1.5, 2.5 ] 
          , grid = GridConst { gridConst = 1.0 } 
          } 
      , tauCombRej = PTEtaGrid
          { ptBins = [ 15.0, 17.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100 ] 
          , etaBins = [ 0.0, 1.0, 1.5, 2.5 ] 
          , grid = GridConst { gridConst = 1.0 } 
          }
      }
  }

cmsTauHPSL :: TauEffData 
cmsTauHPSL = TauEffData 
  { tauName = "Tau_TaHPSL_CMS"
  , tauMetaInfo = MetaInfo 
      { tag = "CMS"
      , description = "CMS Tau HPS Loose"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "HPSL"
  , tauEfficiency = TauCombined 
      { tauCombEff = PTEtaGrid
          { ptBins = [ 15.0, 17.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.0, 1.5, 2.5 ] 
          , grid = GridConst { gridConst = 1.0 } 
          } 
      , tauCombRej = PTEtaGrid
          { ptBins = [ 15.0, 17.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100 ] 
          , etaBins = [ 0.0, 1.0, 1.5, 2.5 ] 
          , grid = GridConst { gridConst = 1.0 } 
          }
      }
  }

cmsTauHPSM :: TauEffData 
cmsTauHPSM = TauEffData 
  { tauName = "Tau_TaHPSM_CMS"
  , tauMetaInfo = MetaInfo 
      { tag = "CMS"
      , description = "CMS Tau HPS Medium"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "HPSM"
  , tauEfficiency = TauCombined 
      { tauCombEff = PTEtaGrid
          { ptBins = [ 15.0, 17.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.0, 1.5, 2.5 ] 
          , grid = GridConst { gridConst = 1.0 } 
          } 
      , tauCombRej = PTEtaGrid
          { ptBins = [ 15.0, 17.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100 ] 
          , etaBins = [ 0.0, 1.0, 1.5, 2.5 ] 
          , grid = GridConst { gridConst = 1.0 } 
          }
      }
  }

cmsTauHPST :: TauEffData 
cmsTauHPST = TauEffData 
  { tauName = "Tau_TaHPST_CMS"
  , tauMetaInfo = MetaInfo 
      { tag = "CMS"
      , description = "CMS Tau HPS Tight"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "HPST"
  , tauEfficiency = TauCombined 
      { tauCombEff = PTEtaGrid
          { ptBins = [ 15.0, 17.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.0, 1.5, 2.5 ] 
          , grid = GridConst { gridConst = 1.0 } 
          } 
      , tauCombRej = PTEtaGrid
          { ptBins = [ 15.0, 17.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100 ] 
          , etaBins = [ 0.0, 1.0, 1.5, 2.5 ] 
          , grid = GridConst { gridConst = 1.0 } 
          }
      }
  }

cmsTauTCT :: TauEffData 
cmsTauTCT = TauEffData 
  { tauName = "Tau_TaTCT_CMS"
  , tauMetaInfo = MetaInfo 
      { tag = "CMS"
      , description = "CMS Tau TC Tight"
      , comment = "table"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "TCT"
  , tauEfficiency = TauCombined 
      { tauCombEff = PTEtaGrid
          { ptBins = [ 15.0, 17.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100.0 ]
          , etaBins = [ 0.0, 1.0, 1.5, 2.5 ] 
          , grid = GridConst { gridConst = 1.0 } 
          } 
      , tauCombRej = PTEtaGrid
          { ptBins = [ 15.0, 17.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 70.0, 100 ] 
          , etaBins = [ 0.0, 1.0, 1.5, 2.5 ] 
          , grid = GridConst { gridConst = 1.0 } 
          }
      }
  }

cmsJetPF :: JetEffData
cmsJetPF = JetEffData
  { jetName = "Jet_PF_CMS"
  , jetMetaInfo = MetaInfo
      { tag = "CMS"
      , description = "CMS PF Jet"
      , comment = "table"
      , reference = "PFT-09-001-pas" } 
  , jetEfficiency = PTEtaGrid
      { ptBins = [ 5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 80.0, 100.0 ]
      , etaBins = [ -2.5, -1.5, 0.0, 1.5, 2.5 ] 
      , grid = GridConst { gridConst = 1.0 } } 
  }

cmsJetCalo :: JetEffData
cmsJetCalo = JetEffData
  { jetName = "Jet_Calo_CMS"
  , jetMetaInfo = MetaInfo 
      { tag = "CMS" 
      , description = "CMS Calorimeter Jet" 
      , comment = "table"
      , reference = "PFT-09-001-pas" }
  , jetEfficiency = PTEtaGrid 
      { ptBins = [ 5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 50.0, 60.0, 80.0, 100.0 ]
      , etaBins = [ -2.5, -1.5, 0.0, 1.5, 2.5 ] 
      , grid = GridConst { gridConst = 1.0 } } 
  }
 

cmsPTThresholds :: PTThresholds
cmsPTThresholds = PTThresholds 
  { pTThreName = "CMS_PTThreshold"
  , muPTMin = 20.0
  , elePTMin = 5.0
  , phoPTMin = 20.0
  , jetPTMin = 20.0
  , bJetPTMin = 20.0 
  , trkPTMin = 0.5
  , tauPTMin = 5.0 
  }

