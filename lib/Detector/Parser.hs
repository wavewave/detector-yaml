{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Detector.Parser where

import           Control.Monad ((<=<))
import qualified Data.List as L
import qualified Data.Text as T
--
import           Detector.Type
-- 
import           YAML.Parser

find :: T.Text -> [(T.Text,PYaml)] -> Maybe PYaml
find key = L.lookup key  

maybeText :: PYaml -> Maybe T.Text 
maybeText (PYText txt) = Just txt
maybeText _ = Nothing

-- | get a detector description from parsed YAML object.
getDetectorDescription :: [(T.Text, PYaml)] -> Maybe DetectorDescription
getDetectorDescription kvlst = do
    xs <- mapM (maybeText <=< flip find kvlst) 
      ["Name", "Description", "Reference"
      , "Comment", "ValidationInfo"]
    case xs of
      [ nm, dsc, ref, cmt, vinfo] -> do
        -- getObjectDescription      
        return DetectorDescription 
               { detectorName = nm
               , detectorDescription = dsc
               , detectorReference = ref
               , detectorComment = cmt
               , detectorValidationInfo = vinfo
               , detectorObject = undefined }
      _ -> Nothing

