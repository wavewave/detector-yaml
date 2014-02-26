{-# LANGUAGE OverloadedStrings #-}

import           Data.Text.Lazy.Builder 
import qualified Data.Text.Lazy.IO as TIO
--
import ATLAS
import YAML

atlas2011yaml = mkDetector $ DetectorDescription 
                           { detectorName = "ATLAS2011"
                           , detectorDescription = "Topo jets used in .."
                           , detectorReference = "arXiv:xxxx.yyyy"
                           , detectorComment = "extracted the efficiencies from the plot 3,4,5 in the reference" 
                           , detectorValidationInfo = "Validated on 2014/02" 
                           , detectorEfficiency = atlas2011Eff }


atlas2011elec_tight_yaml = mkElectronEffData atlasEleDataTight
atlas2011pho_tight_yaml = mkPhotonEffData atlasPhoDataTight
atlas2011bjet_sv50_yaml= mkBJetEffData atlasBJetDataSV50
atlas2011muon_cb1_yaml = mkMuonEffData atlasMuonDataCB1
atlas2011jetyaml = mkJetEffData atlasJetEffData
atlas2011tauyaml = mkTauEffData atlasTauEffData






main :: IO ()
main = do 
  let f (x,y) = TIO.writeFile x $ toLazyText (buildYaml 0 y)
  
  mapM_ f [ ("Atlas2011.yaml", atlas2011yaml)
          , ("Atlas2011_ElecEff_Tight.yaml", atlas2011elec_tight_yaml)
          , ("Atlas2011_PhoEff_Tight.yaml", atlas2011pho_tight_yaml)
          , ("Atlas2011_BJetEff_SV50.yaml", atlas2011bjet_sv50_yaml)
          , ("Atlas2011_MuonEff.yaml", atlas2011muon_cb1_yaml)
          , ("Atlas2011_JetEff.yaml", atlas2011jetyaml) 
          , ("Atlas2011_TauEff.yaml", atlas2011tauyaml)
          ]
  

