{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad ((<=<))
import Data.Attoparsec.Text
import qualified Data.Text.IO as TIO
-- 
import Detector.Parser
import YAML.Parser
import System.Environment 

-- | testing with a given file
parseFile :: FilePath -> IO (Either String PYaml)
parseFile fp = do    
    txt <- TIO.readFile fp 
    return (parseOnly p_yaml txt)

main :: IO ()
main = do 
  args <- getArgs
  let fp = args !! 0  
  r <- parseFile fp
  case r of 
    Left err -> print err
    Right (PYObject kvlst) -> do  
      print (getDetectorDescription kvlst)
  



 -- "Name") kvlst
 --            desc <- (maybeText <=< find "Description") kvlst
 --            ref <- (maybeText <=< find "Reference") kvlst
 --            return (name,desc,ref)
 

    Right _ -> putStrLn "not an object"

