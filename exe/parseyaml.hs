{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad ((<=<))
import           Control.Monad.Trans.Maybe
import           System.Directory
import           System.Environment 
import           System.FilePath
-- 
import           Detector.Parser
import           YAML.Parser


-- | main function 
main :: IO ()
main = do 
  bdir <- getCurrentDirectory
  args <- getArgs
  let fp = args !! 0  
  r <- parseFile (bdir </> "top-level" </> fp <.> "yaml")
  case r of 
    Left err -> print err
    Right (PYObject kvlst) -> do 
      let mdd = getDetectorDescription kvlst
      case mdd of
        Nothing -> putStrLn "parsing failed"
        Just dd -> do 
          print dd
          putStrLn "======================"
          putStrLn "======================"
          putStrLn "======================"
          mdd' <- runMaybeT (importDetectorDescription (bdir </> "object") dd)
          print mdd'     
    Right _ -> putStrLn "not an object"
