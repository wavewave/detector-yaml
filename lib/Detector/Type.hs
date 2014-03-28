{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Detector.Type (
  module Detector.Type.Common
, module Detector.Type.Efficiency
, DetectorDescription (..)
, ObjectDescription (..)
) where

-- import           Control.Applicative
-- import           Data.Foldable
import           Data.Functor.Identity
import           Data.Monoid ((<>))
-- import           Data.Scientific
import           Data.Text (Text)
-- import qualified Data.Text.Lazy as L
-- import           Data.Traversable
-- 
import           YAML.Builder
-- 
import           Detector.Type.Common
import           Detector.Type.Efficiency
-- 
import Prelude hiding (lines)

data DetectorDescription m = 
  DetectorDescription { detectorName :: Text
                      , detectorDescription :: Text
                      , detectorReference :: Text
                      , detectorComment :: Text
                      , detectorValidationInfo :: Text 
                      , detectorObject :: ObjectDescription m
                      -- , detectorSmearing :: SmearingDescription m
                      }

deriving instance Show (DetectorDescription (Either Import))
deriving instance Show (DetectorDescription Identity)

instance (Show a) => Show (Identity a) where
  show x = show (runIdentity x)

instance Nameable (DetectorDescription m) where
  name = detectorName  

data ObjectDescription m = 
  ObjectDescription 
  { electron :: m ElectronEffData 
  , photon :: m PhotonEffData 
  , bJet :: m BJetEffData 
  , muon :: m MuonEffData
  , jet :: m JetEffData 
  , tau :: m TauEffData 
  , track :: Maybe (m TrackEffData) 
  , ptThresholds :: m PTThresholds
  }

deriving instance Show (ObjectDescription (Either Import))
deriving instance Show (ObjectDescription Identity)

{-
data SmearingDescription m =
  SmearingDescription 
  { smearJet :: m JetSmearData } 

deriving instance Show (SmearingDescription (Either Import))
deriving instance Show (SmearingDescription Identity)
-}			    

instance MakeYaml (DetectorDescription ImportList) where
  makeYaml n DetectorDescription {..} = 
    YObject $ [ ( "Name", mkString (n+defIndent) detectorName )
              , ( "Class", mkString (n+defIndent) "TopLevel" )
              , ( "Description", mkString (n+defIndent) detectorDescription)
              , ( "Reference", mkString (n+defIndent) detectorReference)
              , ( "Comment", mkString (n+defIndent) detectorComment )
              , ( "ValidationInfo", mkString (n+defIndent) detectorValidationInfo )
              , ( "Object", makeYaml (n+defIndent) detectorObject ) 
              ]

instance MakeYaml (DetectorDescription []) where
  makeYaml n DetectorDescription {..} = 
    YObject $ [ ( "Name", mkString (n+defIndent) detectorName )
              , ( "Class", mkString (n+defIndent) "TopLevel" )
              , ( "Description", mkString (n+defIndent) detectorDescription)
              , ( "Reference", mkString (n+defIndent) detectorReference)
              , ( "Comment", mkString (n+defIndent) detectorComment )
              , ( "ValidationInfo", mkString (n+defIndent) detectorValidationInfo )
              , ( "Object", makeYaml (n+defIndent) detectorObject ) 
              ]


instance MakeYaml (ObjectDescription (Either Import)) where
  makeYaml n ObjectDescription {..} = 
    YObject $ [ ( "Electron", importOrEmbed (n+defIndent) electron)  
              , ( "Photon", importOrEmbed (n+defIndent) photon) 
              , ( "BJet", importOrEmbed (n+defIndent) bJet)
              , ( "Muon", importOrEmbed (n+defIndent) muon) 
              , ( "Jet", importOrEmbed (n+defIndent) jet)
              , ( "Tau", importOrEmbed (n+defIndent) tau) ] 
              <> maybe [] (\trk -> [("Track", importOrEmbed (n+defIndent) trk)]) track
              <> [ ( "PTThresholds", importOrEmbed (n+defIndent) ptThresholds) ]

 
instance MakeYaml (ObjectDescription ImportList) where
  makeYaml n ObjectDescription {..} = 
      YObject $ [ ( "Electron", importOrEmbed' (n+defIndent) electron)  
                , ( "Photon", importOrEmbed' (n+defIndent) photon) 
                , ( "BJet", importOrEmbed' (n+defIndent) bJet)
                , ( "Muon", importOrEmbed' (n+defIndent) muon) 
                , ( "Jet", importOrEmbed' (n+defIndent) jet)
                , ( "Tau", importOrEmbed' (n+defIndent) tau) ] 
                <> maybe [] (\trk -> [("Track", importOrEmbed' (n+defIndent) trk)]) track
                <> [ ( "PTThresholds", importOrEmbed' (n+defIndent) ptThresholds) ]
    where importOrEmbed' m = YIArray . fmap (importOrEmbed m) . unImportList

instance MakeYaml (ObjectDescription []) where
  makeYaml n ObjectDescription {..} = 
      YObject $ [ ( "Electron", embed' (n+defIndent) electron)  
                , ( "Photon", embed' (n+defIndent) photon) 
                , ( "BJet", embed' (n+defIndent) bJet)
                , ( "Muon", embed' (n+defIndent) muon) 
                , ( "Jet", embed' (n+defIndent) jet)
                , ( "Tau", embed' (n+defIndent) tau) ] 
                <> maybe [] (\trk -> [("Track", embed' (n+defIndent) trk)]) track
                <> [ ( "PTThresholds", embed' (n+defIndent) ptThresholds) ]
    where embed' m = YIArray . fmap (makeYaml m)
