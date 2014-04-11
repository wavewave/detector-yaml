{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad ((<=<))
import           Control.Monad.Trans.Either
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
      let edd = getDetectorDescription kvlst
      case edd of
        Left err -> putStrLn err
        Right dd -> do 
          (TIO.putStrLn . toLazyText . buildYaml 0 . makeYaml 0) dd
          putStrLn "======================"
          putStrLn "======================"
          putStrLn "======================"
          edd' <- runEitherT (importDetectorDescription (bdir </> "object") dd)
          case edd' of
            Right dd' -> (TIO.putStrLn . toLazyText . buildYaml 0 . makeYaml 0) dd'
            Left err -> putStrLn err
    Right _ -> putStrLn "not an object"
