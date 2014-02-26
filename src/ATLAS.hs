{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ATLAS where

import Data.Monoid ((<>))
import Data.Scientific
import Data.Text.Lazy (Text(..))
import YAML

class Nameable a where
  name :: a -> Text

data DetectorDescription = 
  DetectorDescription { detectorName :: Text
                      , detectorDescription :: Text
                      , detectorReference :: Text
                      , detectorComment :: Text
                      , detectorValidationInfo :: Text 
                      , detectorEfficiency :: EfficiencyDescription
                      }

instance Nameable DetectorDescription where
  name = detectorName  

data Import = Import { fileName :: Text }

data EfficiencyDescription = 
  EfficiencyDescription { electron :: Either Import ElectronEffData 
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

data ElectronEfficiency = ElectronEfficiency { elecEffFile :: Import }
			  
data ElectronEffData = ElectronEffData
                            { eleName :: Text
                            , eleMetaInfo :: MetaInfo 
                            , eleEfficiency :: PTEtaData
                            }

instance Nameable ElectronEffData where
  name = eleName
			    
data PhotonEfficiency = PhotonEfficiency { phoEffFile :: Import }

data PhotonEffData = PhotonEffData
                          { phoName :: Text
                          , phoMetaInfo :: MetaInfo
                          , phoEfficiency :: PTEtaData }

instance Nameable PhotonEffData where
  name = phoName
                       
data BJetEfficiency = BJetEfficiency { bJetEffFile :: Import }

data BJetEffData = BJetEffData
                     { bJetName :: Text 
                     , bJetMetaInfo :: MetaInfo
                     , bJetEfficiency :: PTEtaData
                     , bJetRejection :: PTEtaData
                     } 

instance Nameable BJetEffData where
  name = bJetName

data MuonEfficiency = MuonEfficiency { muonEffFile :: Import }

data MuonEffData = MuonEffData 
                     { muonName :: Text
                     , muonMetaInfo :: MetaInfo
                     , muonEfficiency :: PTEtaData }

instance Nameable MuonEffData where
  name = muonName

data JetEfficiency = JetEfficiency { jetEffFile :: Import }

data JetEffData = JetEffData 
                       { jetName :: Text 
                       , jetMetaInfo :: MetaInfo 
                       , jetEfficiency :: PTEtaData }

instance Nameable JetEffData where
  name = jetName


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
              , ( "Description", mkString detectorDescription )
              , ( "Reference", mkString detectorReference )
              , ( "Comment", mkString detectorComment )
              , ( "ValidationInfo", mkString detectorValidationInfo )
              , ( "Efficiency", makeYaml detectorEfficiency ) 
              ]

instance MakeYaml EfficiencyDescription where
  makeYaml EfficiencyDescription {..} = 
    YObject $ [ ( "Electron", importOrEmbed electron )  
              , ( "Photon", importOrEmbed photon ) 
              , ( "BJet", importOrEmbed bJet )
              , ( "Muon", importOrEmbed muon ) 
              , ( "Jet", importOrEmbed jet )
              , ( "Tau", importOrEmbed tau )
              , ( "PTThresholds", makeYaml ptThresholds )
              ] 

atlas2011Eff = EfficiencyDescription 
  { electron = Left (Import "Electron_Loose_ATLAS")
  , photon = Left (Import "Photon_Tight_ATLAS")
  , bJet = Left (Import "BJet_JP50_ATLAS")
  , muon = Left (Import "Muon_CB1_ATLAS")
  , jet = Left (Import "Jet_ATLAS")
  , tau = Right atlasTauDataCutLoose 
           -- Left (Import "Tau_BDT_Tight_ATLAS")
  , ptThresholds = atlasPTThresholds 
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
      , description = "ATLAS Tau Cut Loose"
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

atlasTauDataCutMedium :: TauEffData 
atlasTauDataCutMedium = TauEffData 
  { tauName = "Tau_Cut_Medium_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau Cut Medium"
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

atlasTauDataCutTight :: TauEffData 
atlasTauDataCutTight = TauEffData 
  { tauName = "Tau_Cut_Tight_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau Cut Tight"
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

atlasTauDataLikLoose :: TauEffData 
atlasTauDataLikLoose = TauEffData 
  { tauName = "Tau_Lik_Loose_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau Likelihood Loose"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "Likelihood"
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

atlasTauDataLikMedium :: TauEffData 
atlasTauDataLikMedium = TauEffData 
  { tauName = "Tau_Lik_Medium_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau Likelihood Medium"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "Likelihood"
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

atlasTauDataLikTight :: TauEffData 
atlasTauDataLikTight = TauEffData 
  { tauName = "Tau_Lik_Tight_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau Likelihood Tight"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "Likelihood"
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

atlasTauDataBDTLoose :: TauEffData 
atlasTauDataBDTLoose = TauEffData 
  { tauName = "Tau_BDT_Loose_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau BDT Loose"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "BoostedDecisionTree"
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

atlasTauDataBDTMedium :: TauEffData 
atlasTauDataBDTMedium = TauEffData 
  { tauName = "Tau_BDT_Medium_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau BDT Medium"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "BoostedDecisionTree"
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

atlasTauDataBDTTight :: TauEffData 
atlasTauDataBDTTight = TauEffData 
  { tauName = "Tau_BDT_Tight_ATLAS"
  , tauMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "ATLAS Tau Likelihood Tight"
      , comment = "We use table from reference"
      , reference = "arXiv:xxxx.yyyy" }
  , tauTagMethod = "BoostedDecisionTree"
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