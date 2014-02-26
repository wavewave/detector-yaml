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


atlas2011elecyaml = mkElectronEffData atlasElecEffData
atlas2011phoyaml = mkPhotonEffData atlasPhoEffData
atlas2011bjetyaml= mkBJetEffData atlasBJetEffData
atlas2011muonyaml = mkMuonEffData atlasMuonEffData
atlas2011jetyaml = mkJetEffData atlasJetEffData
atlas2011tauyaml = mkTauEffData atlasTauEffData






main :: IO ()
main = do 
  let f (x,y) = TIO.writeFile x $ toLazyText (buildYaml 0 y)
  
  mapM_ f [ ("Atlas2011.yaml", atlas2011yaml)
          , ("Atlas2011_ElecEff.yaml", atlas2011elecyaml)
          , ("Atlas2011_PhoEff.yaml", atlas2011phoyaml)
          , ("Atlas2011_BJetEff.yaml", atlas2011bjetyaml)
          , ("Atlas2011_MuonEff.yaml", atlas2011muonyaml)
          , ("Atlas2011_JetEff.yaml", atlas2011jetyaml) 
          , ("Atlas2011_TauEff.yaml", atlas2011tauyaml)
          ]
  

