{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Detector.Type.Smearing where

import           Data.Functor.Identity
import           Data.Monoid ((<>))
import           Data.Text (Text)
--
import           YAML.Builder
--
import           Detector.Type.Common
 

data SmearingDescription m =
  SmearingDescription 
  { smearJet :: m JetSmearData } 

deriving instance Show (SmearingDescription (Either Import))
deriving instance Show (SmearingDescription Identity)
			    

instance MakeYaml (SmearingDescription ImportList) where
  makeYaml n SmearingDescription {..} = 
      YObject $ [ ("Jet", importOrEmbed' (n+defIndent) smearJet) ] 
    where importOrEmbed' m = YIArray . fmap (importOrEmbed m) . unImportList

instance MakeYaml (SmearingDescription []) where
  makeYaml n SmearingDescription {..} = 
      YObject $ [ ("Jet", embed' (n+defIndent) smearJet) ] 
    where embed' m = YIArray . fmap (makeYaml m)

data JetSmearData = JetSmearData 
                    { jetSmearName :: Text
                    , jetSmearMetaInfo :: MetaInfo
                    , jetSmearSmearing :: PTEtaData
                    }
                  deriving (Show)

instance Nameable JetSmearData where
  name = jetSmearName

instance MakeYaml JetSmearData where
  makeYaml n JetSmearData {..} = 
    YObject $ [ ("Name", mkString (n+defIndent) jetSmearName) ] 
              <> mkMetaInfoPairs (n+defIndent) jetSmearMetaInfo 
              <> [ ("Smearing", makeYaml (n+defIndent) jetSmearSmearing) ] 
