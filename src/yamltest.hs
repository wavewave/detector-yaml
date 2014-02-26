{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}


import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder 
import qualified Data.Text.Lazy.IO as TIO
import           System.FilePath
--
import ATLAS
import YAML

atlas2011 = DetectorDescription 
            { detectorName = "ATLAS2011"
            , detectorDescription = "ATLAS 2011 detector description"
            , detectorReference = "arXiv:xxxx.yyyy"
            , detectorComment = "extracted the efficiencies from the plot 3,4,5 in the reference" 
            , detectorValidationInfo = "Validated on 2014/02" 
            , detectorEfficiency = atlas2011Eff }

-- atlas2011elec_tight_yaml = mkElectronEffData atlasEleDataTight
-- atlas2011pho_tight_yaml = mkPhotonEffData atlasPhoDataTight
-- atlas2011bjet_sv50_yaml= mkBJetEffData atlasBJetDataSV50
-- atlas2011muon_cb1_yaml = mkMuonEffData atlasMuonDataCB1
-- atlas2011jet_yaml = mkJetEffData atlasJetData
-- atlas2011tau_yaml = mkTauEffData atlasTauDataCutLoose

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
 

