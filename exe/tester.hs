{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Attoparsec.Text as A
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.Monoid (mconcat)
-- import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO as TIO
-- 
import YAML.Builder
import YAML.Parser
--
-- import ATLAS
import Detector.Parser
import Detector.Type




atlasEleTest :: ElectronEffData
atlasEleTest = ElectronEffData
  { eleName = "Electron_Medium_ATLAS"
  , eleMetaInfo = MetaInfo 
      { tag = "ATLAS"
      , description = "Medium electron object 2011 ATLAS"
      , comment = "We use table from reference" 
      , reference = "arXiv:xxxx.yyyy" }
  , eleEfficiency = PTEtaInterpolation
      { isEtaSymmetric = True
      , interpol = IPPredefinedMode3 
          { seriesBA = [ FuncBin 0    (HM.fromList [ (-2,  9.476216187754203)
                                                   , (-1, -0.16939888048822812)
                                                   , ( 0,  0.01096643215740863)
                                                   , ( 1, -0.00001147146295333292)
                                                   , ( 2,  1.9289334367006085e-8)
                                                   , ( 3, -1.5000987275723775e-12) ])
                       , FuncBin 0.75 (HM.fromList [ (-2,  8.197400117302609)
                                                   , (-1, -0.05636233086517818) 
                                                   , ( 0,  0.010365438976501047)
                                                   , ( 1, -0.0000020835685322585434)
                                                   , ( 2,  2.001368792089794e-9)
                                                   , ( 3,  6.012078707551757e-12) ])
                       , FuncBin 1.25 (HM.fromList [ (-2,  6.446193732566276) 
                                                   , (-1, -0.010586512399379697)
                                                   , ( 0,  0.016592828729054618)
                                                   , ( 1, -0.000008966344163685219)
                                                   , ( 2,  6.867359722322189e-10)
                                                   , ( 3,  8.093547934486339e-12) ])
                       , FuncBin 1.75 (HM.fromList [ (-2, -0.8554617719091405)
                                                   , (-1,  0.4246942698759444)
                                                   , ( 0,  0.018887675539395778)
                                                   , ( 1,  0.0000008827096751274164)
                                                   , ( 2, -3.395818453325127e-9)
                                                   , ( 3, -3.808692671281164e-12) ])
                       , FuncBin 2.25 (HM.fromList [ (-2, -6.078548157133825)
                                                   , (-1,  0.6523709619986131)
                                                   , ( 0,  0.02566741878632772)
                                                   , ( 1, -0.000004513118203117088)
                                                   , ( 2,  1.5015469175097658e-9)
                                                   , ( 3,  8.156483941322221e-12) ])
                       , FuncBin 2.75 (HM.fromList [ (-2, -9.496101460575369)
                                                   , (-1,  0.7866574724505102)
                                                   , ( 0,  0.031548358061336486)
                                                   , ( 1, -0.000007095429220069707)
                                                   , ( 2,  2.099191724700867e-9)
                                                   , ( 3,  1.1359410993905745e-11) ])
                       , FuncBin 3.25 (HM.fromList [ (-2, -4.5274342332395815)
                                                   , (-1,  0.43592198252621316)
                                                   , ( 0,  0.040108297897736325)
                                                   , ( 1,  0.00002082414790810237)
                                                   , ( 2, -5.43013214981909e-9)
                                                   , ( 3, -1.529328127051232e-11) ])
                       , FuncBin 3.75 (HM.fromList [ (-2, -1.1160615726947604)
                                                   , (-1,  0.34705420681539366) 
                                                   , ( 0,  0.04184642899559342)
                                                   , ( 1,  0.00002971363284633445)
                                                   , ( 2, -7.670934703489684e-9 )
                                                   , ( 3, -2.3710958935037943e-11) ])
                       ] 
          , etaBound = 4.0 
          }
      }
  } 

main :: IO ()
main = do
  let txt = (TLB.toLazyText . buildYaml 0 . makeYaml 0) atlasEleTest -- atlasEleDataMedium 
  TIO.putStrLn txt

  let r = A.parseOnly p_yaml ((mconcat . TL.toChunks) txt)
  print r
  case r of
    Left err -> print err
    Right (PYObject kvlst) -> do 
      let med = getElectronEffData kvlst
      F.forM_ med $ \ed -> let txt' = (TLB.toLazyText . buildYaml 0 . makeYaml 0) ed 
                           in TIO.putStrLn txt'
      -- print med
    _ -> putStrLn "parse failed as an object"