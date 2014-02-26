{-# LANGUAGE OverloadedStrings #-}

import           Data.Text.Lazy.Builder 
import qualified Data.Text.Lazy.IO as TIO
--
import ATLAS
import YAML

atlas2011 = mkDetector $ DetectorDescription 
                           { detectorName = "ATLAS2011"
                           , detectorDescription = "Topo jets used in .."
                           , detectorReference = "arXiv:xxxx.yyyy"
                           , detectorComment = "extracted the efficiencies from the plot 3,4,5 in the reference" 
                           , detectorEfficiency = atlas2011Eff }

atlas2011Eff = EfficiencyDescription { elecEfficiency = atlasElecEff 
                                     , phoEfficiency = atlasPhoEff 
                                     , bJetEfficiency = atlasBJetEff
                                     , muonEfficiency = atlasMuonEff
                                     , jetEfficiency = atlasJetEff
                                     , tauEfficiency = atlasTauEff
                                     , ptThresholds = atlasPTThresholds 
                                     }



main :: IO ()
main = do 
  TIO.putStrLn $ toLazyText (buildYaml 0 atlas2011)
