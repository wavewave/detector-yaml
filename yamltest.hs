{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

import qualified Data.Yaml as Y
import qualified Data.ByteString.Char8 as B

phoLowPtBins :: Y.Value
phoLowPtBins = (Y.array . map Y.Number) [ 15.0, 18.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 60.0, 80.0, 100.0 ]  

main :: IO ()
main = do 
  let testobj = Y.object [ ("ElePtBins", Y.String "test" )  
                         , ("EleEtaBins", Y.Number 10.0)
                         , ("nEleeta", Y.String "test")
                         , ("nElept", Y.Number 12)
                         , ("TightEleEff", Y.String "test")
                         , ("MediumEleEff", Y.String "test")
                         , ("LooseEleEff", Y.String "test")
                         , ("PhoLowPtBins", phoLowPtBins )
                         , ("PhoHighPtBins", Y.String "test")
                         , ("PhoEtaBins", Y.String "test")
                         ] 

  B.putStrLn $ Y.encode testobj 
  