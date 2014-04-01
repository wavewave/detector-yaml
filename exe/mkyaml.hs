{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

import qualified Data.Text as ST
-- import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder 
import qualified Data.Text.Lazy.IO as TIO
import           System.Directory
import           System.FilePath
--
import YAML.Builder
--
import ATLAS
import CMS
import Detector.Type

data YamlBox = forall a. (MakeYaml a, Nameable a) => MkYamlBox a 

main :: IO ()
main = do 
  let f (MkYamlBox x) = 
       TIO.writeFile (ST.unpack (name x) <.> "yaml") $ toLazyText (buildYaml 0 (makeYaml 0 x))
  
  cdir <- getCurrentDirectory
  createDirectoryIfMissing True (cdir </> "top-level" ) 
  createDirectoryIfMissing True (cdir </> "object" )

  setCurrentDirectory (cdir </> "top-level")
  mapM_ f [ MkYamlBox atlas2011, MkYamlBox cms2011 ] 

  setCurrentDirectory (cdir </> "object")
  mapM_ f [ MkYamlBox atlasEleDataTight
          , MkYamlBox atlasEleDataMedium
          , MkYamlBox atlasEleDataLoose
          , MkYamlBox atlasPhoDataTight
          , MkYamlBox atlasPhoDataLoose
          , MkYamlBox atlasBJetDataSV50
          , MkYamlBox atlasBJetDataJP50
          , MkYamlBox atlasBJetDataJP70
          , MkYamlBox atlasMuonDataCB1
          , MkYamlBox atlasMuonDataCB2
          , MkYamlBox atlasMuonDataST1
          , MkYamlBox atlasMuonDataST2
          , MkYamlBox atlasJetData
          , MkYamlBox atlasTauDataCutLoose
          , MkYamlBox atlasTauDataCutMedium
          , MkYamlBox atlasTauDataCutTight
          , MkYamlBox atlasTauDataLikLoose
          , MkYamlBox atlasTauDataLikMedium
          , MkYamlBox atlasTauDataLikTight
          , MkYamlBox atlasTauDataBDTLoose
          , MkYamlBox atlasTauDataBDTMedium
          , MkYamlBox atlasTauDataBDTTight
          , MkYamlBox atlasPTThresholds
          , MkYamlBox atlasSmearElectron
          , MkYamlBox atlasSmearMuon
          , MkYamlBox atlasSmearPhoton
          , MkYamlBox atlasSmearTopoJet
          , MkYamlBox atlasSmearTrack
          , MkYamlBox atlasSmearTau
          , MkYamlBox atlasSmearMET
          ]


  mapM_ f [ MkYamlBox cmsBTagTCHEL
          , MkYamlBox cmsBTagSSVHPT
          , MkYamlBox cmsBTagSSVHEM
          , MkYamlBox cmsMuonS
          , MkYamlBox cmsMuonP
          , MkYamlBox cmsMuonT
          , MkYamlBox cmsElePF
          , MkYamlBox cmsEleCicSTight
          , MkYamlBox cmsEleCicLoose
          , MkYamlBox cmsEleWP80
          , MkYamlBox cmsEleWP95
          , MkYamlBox cmsPhoPF
          , MkYamlBox cmsPhoTight
          , MkYamlBox cmsPhoLoose
          , MkYamlBox cmsTrack
          , MkYamlBox cmsTauTaNCL
          , MkYamlBox cmsTauTaNCM
          , MkYamlBox cmsTauTaNCT
          , MkYamlBox cmsTauHPSL
          , MkYamlBox cmsTauHPSM
          , MkYamlBox cmsTauHPST
          , MkYamlBox cmsTauTCT
          , MkYamlBox cmsJetPF
          , MkYamlBox cmsJetCalo
          , MkYamlBox cmsPTThresholds
          , MkYamlBox cmsSmearElectron
          , MkYamlBox cmsSmearMuon
          , MkYamlBox cmsSmearPhoton
          , MkYamlBox cmsSmearTopoJet
          , MkYamlBox cmsSmearTrack
          , MkYamlBox cmsSmearTau
          , MkYamlBox cmsSmearMET
          ]
 

