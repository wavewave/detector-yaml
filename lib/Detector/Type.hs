{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Detector.Type (
  module Detector.Type.Common
, module Detector.Type.Identification
, module Detector.Type.PTEtaData
, module Detector.Type.Smearing
, DetectorDescription (..)
, IdentificationDescription (..)
) where

import           Data.Functor.Identity
import           Data.Text (Text)
-- 
import           YAML.Builder
-- 
import           Detector.Type.Common
import           Detector.Type.Identification
import           Detector.Type.PTEtaData
import           Detector.Type.Smearing
-- 
import Prelude hiding (lines)

data DetectorDescription m = 
  DetectorDescription { detectorName :: Text
                      , detectorDescription :: Text
                      , detectorReference :: Text
                      , detectorComment :: Text
                      , detectorValidationInfo :: Text 
                      , detectorIdentification :: IdentificationDescription m
                      , detectorSmearing :: SmearingDescription m
                      }

deriving instance Show (DetectorDescription (Either Import))
deriving instance Show (DetectorDescription Identity)


instance Nameable (DetectorDescription m) where
  name = detectorName  

instance MakeYaml (DetectorDescription ImportList) where
  makeYaml n DetectorDescription {..} = 
    YObject $ [ ( "Name", mkString (n+defIndent) detectorName )
              , ( "Class", mkString (n+defIndent) "TopLevel" )
              , ( "Description", mkString (n+defIndent) detectorDescription)
              , ( "Reference", mkString (n+defIndent) detectorReference)
              , ( "Comment", mkString (n+defIndent) detectorComment )
              , ( "ValidationInfo", mkString (n+defIndent) detectorValidationInfo )
              , ( "Identification", makeYaml (n+defIndent) detectorIdentification ) 
              , ( "Smearing", makeYaml (n+defIndent) detectorSmearing )
              ]

instance MakeYaml (DetectorDescription []) where
  makeYaml n DetectorDescription {..} = 
    YObject $ [ ( "Name", mkString (n+defIndent) detectorName )
              , ( "Class", mkString (n+defIndent) "TopLevel" )
              , ( "Description", mkString (n+defIndent) detectorDescription)
              , ( "Reference", mkString (n+defIndent) detectorReference)
              , ( "Comment", mkString (n+defIndent) detectorComment )
              , ( "ValidationInfo", mkString (n+defIndent) detectorValidationInfo )
              , ( "Object", makeYaml (n+defIndent) detectorIdentification )
              , ( "Smearing", makeYaml (n+defIndent) detectorSmearing )
              ]

