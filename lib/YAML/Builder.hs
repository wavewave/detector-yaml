{-# LANGUAGE OverloadedStrings #-}

module YAML.Builder where

import qualified Data.Foldable as F
import           Data.List (intersperse)
import           Data.Monoid ((<>), mconcat, mempty)
import           Data.Scientific
import qualified Data.String as S (IsString(..), fromString)
import qualified Data.Text.Lazy as T
import qualified Data.Text as ST
import           Data.Text.Lazy.Builder 

data ListStyle = Inline | Wrapped 

data StringStyle = Plain | DoubleQuote

data YamlValue = YObject [(T.Text, YamlValue)]
               | YLArray ListStyle [YamlValue]
               | YIArray [YamlValue]
               | YPrim YamlPrimValue 

data YamlPrimValue = YNumber Scientific
                   | YInteger Int 
                   | YString StringStyle T.Text
                   | YLiteralBlock Int [T.Text]
                   | YBool Bool 
                   | YNull 

isObject :: YamlValue -> Bool 
isObject (YObject _) = True
isObject _ = False

isPrim :: YamlValue -> Bool 
isPrim (YPrim _) = True
isPrim _ = False 

newLine :: Builder
newLine = fromLazyText "\n"

instance S.IsString YamlPrimValue where
  fromString str = let b = any (`elem` [ ',', ':' ]) str
                   in if b then YString DoubleQuote (T.pack str) else YString Plain (T.pack str) 

instance S.IsString YamlValue where
  fromString = YPrim . S.fromString

class MakeYaml a where
  makeYaml :: Int -> a -> YamlValue

defIndent :: Int
defIndent = 4 

buildYaml :: Int -> YamlValue -> Builder 
buildYaml n (YObject m) = (mconcat . intersperse (newLine <> makeIndent n) . map (buildPair n) ) m
buildYaml n (YLArray sty xs) = buildList sty n xs 
buildYaml n (YIArray xs) = buildItemList n xs
buildYaml _ (YPrim p) = buildPrim p 

buildItemList :: Int -> [YamlValue] -> Builder
buildItemList n (x:xs) = makeIndent n <> newLine
                         <> makeIndent n <> fromLazyText "- " 
                         <> buildYaml (n+2) x <> newLine
                         <> mconcat (map buildItem xs)
  where buildItem y = 
          makeIndent n <> fromLazyText "- " <> buildYaml (n+2) y <> newLine
buildItemList _ _ = mempty

buildList :: ListStyle -> Int -> [YamlValue] -> Builder 
buildList Inline n xs = fromLazyText "[ "
                        <> (mconcat . intersperse (fromLazyText ", ") . map (buildYaml n)) xs 
                        <> fromLazyText " ]"
buildList Wrapped n xs = newLine <> makeIndent n 
                         <> fromLazyText "[ " 
                         <> ( mconcat 
                            . intersperse (newLine <> makeIndent n <> fromLazyText ", ")
                            . map (buildYaml n)) xs 
                         <> fromLazyText " ]"

buildPrim :: YamlPrimValue -> Builder
buildPrim (YNumber s) = scientificBuilder s 
buildPrim (YInteger s) = (fromLazyText . T.pack . show) s
buildPrim (YString Plain txt) = fromLazyText txt
buildPrim (YString DoubleQuote txt) = fromLazyText "\"" <> fromLazyText txt <> fromLazyText "\""
buildPrim (YLiteralBlock n txts) = 
    (fromLazyText "|\n") 
    <> ( F.fold 
       . intersperse (fromLazyText "\n")
       . map (\txt -> makeIndent n <> fromLazyText txt) ) txts
buildPrim (YBool b) = (fromLazyText . T.pack . show) b 
buildPrim YNull = mempty 


buildPair :: Int -> (T.Text, YamlValue) -> Builder
buildPair n (k,v) = fromLazyText k 
                    <> fromLazyText ": "
                    <> (if isObject v then newLine <> makeIndent (n+defIndent) else mempty)
                    <> buildYaml (n+defIndent) v

makeIndent :: Int -> Builder 
makeIndent n = mconcat (replicate n (fromString " "))

mkInline :: [Scientific] -> YamlValue 
mkInline = YLArray Inline . map (YPrim . YNumber)  

mkWrap :: [YamlValue] -> YamlValue 
mkWrap = YLArray Wrapped 

makeLiteralBlock :: Int -> T.Text -> YamlPrimValue
makeLiteralBlock n txt = YLiteralBlock n (T.lines txt)

mkString :: Int -> ST.Text -> YamlValue
mkString n stxt = 
  let txt = T.fromChunks [stxt]
      len = (length . ST.lines) stxt 
  in if len <= 1 
       then if T.any (`elem` [':',',']) txt
              then (YPrim . YString DoubleQuote) txt
              else (YPrim . YString Plain) txt
       else (YPrim . makeLiteralBlock n) txt
