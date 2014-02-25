import           Data.Text.Lazy.Builder 
import qualified Data.Text.Lazy.IO as TIO
--
import ATLAS
import YAML

testvalue = mkATLAS (ATLASInfo { elecEff = atlasElecEff 
                               , phoEff = atlasPhoEff 
                               , bJetEff = atlasBJetEff
                               , muonEff = atlasMuonEff
                               })



main :: IO ()
main = do 
  TIO.putStrLn $ toLazyText (buildYaml 4 testvalue)
