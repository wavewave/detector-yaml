{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad ((<=<))
import           Control.Monad.Trans.Maybe
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO as TIO
import           System.Directory
import           System.Environment 
import           System.FilePath
-- 
import           Detector.Parser
-- import           Detector.Type
import           YAML.Parser
import           YAML.Builder

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
          (TIO.putStrLn . toLazyText . buildYaml 0 . makeYaml 0) dd
          putStrLn "======================"
          putStrLn "======================"
          putStrLn "======================"
          mdd' <- runMaybeT (importDetectorDescription (bdir </> "object") dd)
          case mdd' of
            Just dd' -> (TIO.putStrLn . toLazyText . buildYaml 0 . makeYaml 0) dd'
            Nothing -> return ()
    Right _ -> putStrLn "not an object"
