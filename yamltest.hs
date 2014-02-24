{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

import Data.Yaml 
-- import Data.Aeson.Encode.Pretty 
-- import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B

-- import Text.Libyaml

data Coord = Coord { x :: Double, y :: Double } deriving Generic

instance ToJSON Coord 

main :: IO ()
main = do 
  putStrLn "yaml test" 
  let testobj = object [ ("test", String "test" )  
                       , ("abc", Number 10.0)
                       ] 
      -- jsonconf = defConfig { confCompare = compare
      --                     } 
   
  print testobj

  print (toJSON (Coord 10 30))

  -- B.writeFile "test2.json" (encodePretty' jsonconf (Coord 10 30))

  -- B.writeFile "test.json" (encodePretty (Coord 10 30))
  B.putStrLn $ encode testobj 
  