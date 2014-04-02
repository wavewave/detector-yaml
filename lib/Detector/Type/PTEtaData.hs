{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Detector.Type.PTEtaData where

import           Control.Applicative
import           Data.Function (on)
import qualified Data.HashMap.Strict as HM
import           Data.List (sortBy)
import           Data.Scientific
-- import           Data.Text (Text)
-- 
import           YAML.Builder

data Grid = GridFull { gridData :: [ [ Scientific ] ] 
                                  }
          | GridConst { gridConst :: Scientific } 
          deriving (Show)

instance MakeYaml Grid where
  makeYaml n GridFull {..} = 
    YObject $ [ ("Type", mkString (n+defIndent) "Full")
              , ("Data", mkWrap (map mkInline gridData))
              ]
  makeYaml n GridConst {..} = 
    YObject $ [ ("Type", mkString (n+defIndent) "Const")
              , ("Data", (YPrim . YNumber) gridConst)
              ]


data FuncBin a = FuncBin { binStart :: Scientific
                         , binContent :: a } 

deriving instance (Show a) => Show (FuncBin a)

instance MakeYaml (HM.HashMap Int Scientific) where
  makeYaml _ hm = let lst = (sortBy (compare `on` fst) . HM.toList) hm 
                      lst' = map ((\x y -> [x,y]) <$> YPrim . YInteger . fst <*> YPrim . YNumber . snd) lst
                  in YLArray Wrapped (map (YLArray Inline) lst')

instance (MakeYaml a) => MakeYaml (FuncBin a) where
  makeYaml n FuncBin {..} = 
    YObject $ [ ("BinStart", (YPrim . YNumber) binStart) 
              , ("BinContent", makeYaml (n+defIndent) binContent) ] 
  

data Interpolation = IPConstant { ipConstant :: Scientific }  
                   | IPPredefinedMode1 
                     { seriesBA :: [ FuncBin (HM.HashMap Int Scientific) ]  
                     , etaBound :: Scientific
                     } 
                   | IPPredefinedMode2
                     { seriesBA :: [ FuncBin (HM.HashMap Int Scientific) ]
                     , etaBound :: Scientific
                     }
                   | IPPredefinedMode3
                     { seriesBA :: [ FuncBin (HM.HashMap Int Scientific) ] 
                     , etaBound :: Scientific
                     }
                   deriving (Show)

instance MakeYaml Interpolation where
    makeYaml n IPConstant {..} = 
      YObject $ [ ("Type", mkString (n+defIndent) "Constant") 
                , ("Value", (YPrim . YNumber) ipConstant) ]
    makeYaml n IPPredefinedMode1 {..} = 
      YObject $ [ ("Type", mkString (n+defIndent) "PredefinedMode1") 
                , ("EtaBinContent", (YIArray . map (makeYaml (n+defIndent))) seriesBA)
                , ("EtaBound", (YPrim . YNumber) etaBound) ]
    makeYaml n IPPredefinedMode2 {..} = 
      YObject $ [ ("Type", mkString (n+defIndent) "PredefinedMode2") 
                , ("EtaBinContent", (YIArray . map (makeYaml (n+defIndent))) seriesBA)
                , ("EtaBound", (YPrim . YNumber) etaBound) ] 
    makeYaml n IPPredefinedMode3 {..} = 
      YObject $ [ ("Type", mkString (n+defIndent) "PredefinedMode3") 
                , ("EtaBound", (YPrim . YNumber) etaBound)
                , ("EtaBinContent", (YIArray . map (makeYaml (n+defIndent))) seriesBA) ] 

data PTEtaData = PTEtaGrid 
                   { ptBins :: [Scientific]
                   , etaBins :: [Scientific]
                   , isEtaSymmetric :: Bool
                   , grid :: Grid } 
                | PTEtaInterpolation 
                   { interpol :: Interpolation 
                   , isEtaSymmetric :: Bool
                   } 
               
deriving instance Show PTEtaData

instance MakeYaml PTEtaData where 
  makeYaml n PTEtaGrid {..} = 
    YObject $ [ ("Type", mkString (n+defIndent) "Grid" )
              , ("PtBins", mkInline ptBins)
              , ("EtaBins", mkInline etaBins)
              , ("IsEtaSymmetric", makeYaml (n+defIndent) isEtaSymmetric)
              , ("Grid", makeYaml (n+defIndent) grid ) ]
  makeYaml n PTEtaInterpolation {..} =
    YObject $ [ ("Type", mkString (n+defIndent) "Interpolation")
              , ("IsEtaSymmetric", makeYaml (n+defIndent) isEtaSymmetric)
              , ("Interpolation", makeYaml (n+defIndent) interpol) ] 


