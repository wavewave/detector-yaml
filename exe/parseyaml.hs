{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad ((<=<))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State
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
  r <- flip runStateT [] . runEitherT $ do 
    PYObject kvlst <- (EitherT . liftIO) (parseFile (bdir </> "top-level" </> fp <.> "yaml"))
    dd <- getDetectorDescription kvlst 
    liftIO $ do
      (TIO.putStrLn . toLazyText . buildYaml 0 . makeYaml 0) dd
      putStrLn "======================"
      putStrLn "======================"
      putStrLn "======================"
    dd' <- importDetectorDescription (bdir </> "object") dd
    liftIO $ (liftIO . TIO.putStrLn . toLazyText . buildYaml 0 . makeYaml 0) dd'
  case r of 
    (Left err, strs) -> putStrLn err >> print strs
    (Right _, _) -> return ()

{-
  r <- parseFile (bdir </> "top-level" </> fp <.> "yaml")
  case r of 
    Left err -> print err
    Right (PYObject kvlst) -> do 
      let edd = getDetectorDescription kvlst
      case edd of
        Left err -> putStrLn err
        Right dd -> do 
    Right _ -> putStrLn "not an object"
-}