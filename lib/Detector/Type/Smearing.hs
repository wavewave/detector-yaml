{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Detector.Type.Smearing where

import           Data.Functor.Identity
import           Data.Monoid ((<>), mempty)
import           Data.Text (Text)
--
import           YAML.Builder
--
import           Detector.Type.Common
 
data TElectron
data TMuon
data TPhoton
data TJet
data TTrack
data TTau
data TMET

data SmearingDescription m =
  SmearingDescription 
  { smearElectron :: m (SmearData TElectron)
  , smearMuon     :: m (SmearData TMuon)
  , smearPhoton   :: m (SmearData TPhoton)
  , smearJet      :: m (SmearData TJet)
  , smearTrack    :: m (SmearData TTrack)
  , smearTau      :: m (SmearData TTau)
  , smearMET      :: m (SmearData TMET)
  } 

deriving instance Show (SmearingDescription (Either Import))
deriving instance Show (SmearingDescription Identity)
			    
instance MakeYaml (SmearingDescription ImportList) where
  makeYaml n sd = 
      (YObject . mk "Electron"  smearElectron
               . mk "Muon"      smearMuon
               . mk "Photon"    smearPhoton
               . mk "Jet"       smearJet
               . mk "Track"     smearTrack
               . mk "Tau"       smearTau
               . mk "MissingET" smearMET ) mempty
    where importOrEmbed' m = YIArray . fmap (importOrEmbed m) . unImportList
          mk txt f = ((txt, importOrEmbed' (n+defIndent) (f sd)) :) 

instance MakeYaml (SmearingDescription []) where
  makeYaml n sd = 
      (YObject . mk "Electron"  smearElectron
               . mk "Muon"      smearMuon
               . mk "Photon"    smearPhoton
               . mk "Jet"       smearJet
               . mk "Track"     smearTrack
               . mk "Tau"       smearTau
               . mk "MissingET" smearMET ) mempty
    where embed' m = YIArray . fmap (makeYaml m)
          mk txt f = ((txt, embed' (n+defIndent) (f sd)) :)

data SmearData a = SmearData 
                   { smearName :: Text
                   , smearMetaInfo :: MetaInfo
                   , smearSmearing :: PTEtaData
                   }
                   deriving (Show)

instance Nameable (SmearData a) where
  name = smearName

instance MakeYaml (SmearData a) where
  makeYaml n SmearData {..} = 
    YObject $ [ ("Name", mkString (n+defIndent) smearName) ] 
              <> mkMetaInfoPairs (n+defIndent) smearMetaInfo 
              <> [ ("Smearing", makeYaml (n+defIndent) smearSmearing) ] 
