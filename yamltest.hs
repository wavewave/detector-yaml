{-# LANGUAGE OverloadedStrings #-}

import           Data.Text.Lazy.Builder 
import qualified Data.Text.Lazy.IO as TIO
--
import ATLAS
import YAML

atlas2011 = 
  YObject [ ( "ATLAS2011", mkATLAS (ATLASInfo { elecEfficiency = atlasElecEff 
                                   , phoEfficiency = atlasPhoEff 
                                   , bJetEfficiency = atlasBJetEff
                                   , muonEfficiency = atlasMuonEff
                                   , jetEfficiency = atlasJetEff
                                   , tauEfficiency = atlasTauEff
                                   , ptThresholds = atlasPTThresholds 
                                   }) ) ]



main :: IO ()
main = do 
  TIO.putStrLn $ toLazyText (buildYaml 0 atlas2011)
