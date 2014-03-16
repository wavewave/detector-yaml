{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}


import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder 
import qualified Data.Text.Lazy.IO as TIO
import           System.FilePath
--
import ATLAS
import CMS
import Detector
import YAML.Builder

atlas2011 :: DetectorDescription
atlas2011 = DetectorDescription 
            { detectorName = "ATLAS2011"
            , detectorDescription = "ATLAS 2011 detector description"
            , detectorReference = "arXiv:xxxx.yyyy"
            , detectorComment = "extracted the efficiencies from the plot 3,4,5 in the reference" 
            , detectorValidationInfo = "Validated on 2014/02" 
            , detectorObject = atlas2011Object }

cms2011 :: DetectorDescription
cms2011 = DetectorDescription 
            { detectorName = "CMS2011"
            , detectorDescription = "CMS 2011 detector description"
            , detectorReference = "arXiv:xxxx.yyyy"
            , detectorComment = "extracted the efficiencies from the plot 3,4,5 in the reference" 
            , detectorValidationInfo = "Validated on 2014/02" 
            , detectorObject = cms2011Object }



data YamlBox = forall a. (MakeYaml a, Nameable a) => MkYamlBox a 

main :: IO ()
main = do 
  let f (MkYamlBox x) = 
       TIO.writeFile (T.unpack (name x) <.> "yaml") $ toLazyText (buildYaml 0 (makeYaml x))
  
  mapM_ f [ MkYamlBox atlas2011
          , MkYamlBox atlasEleDataTight
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
          ]


  mapM_ f [ MkYamlBox cms2011
          , MkYamlBox cmsBTagTCHEL
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
          ]
 

