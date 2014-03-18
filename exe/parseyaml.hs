{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad ((<=<))
import           Data.Attoparsec.Text
import qualified Data.Text.IO as TIO
import           System.Environment 
-- 
import           Detector.Parser
import           YAML.Parser

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
      -- print kvlst
      print (getDetectorDescription kvlst)
    Right _ -> putStrLn "not an object"
