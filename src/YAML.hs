{-# LANGUAGE OverloadedStrings #-}

module YAML where

-- import           Data.Attoparsec
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List (intersperse)
import           Data.Monoid ((<>), mconcat, mempty)
import           Data.Scientific
import qualified Data.String as S (IsString(..), fromString)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder 

-- import qualified Data.Text.Lazy.IO as TIO
-- import qualified Data.Vector as V

data ListStyle = Inline | Wrapped 

data YamlValue = YObject [(T.Text, YamlValue)]
               | YLArray ListStyle [YamlValue]
               | YIArray [YamlValue]
               | YPrim YamlPrimValue 

data YamlPrimValue = YNumber Scientific
                   | YInteger Int 
                   | YString T.Text
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
  fromString str = YString (T.pack str) 

instance S.IsString YamlValue where
  fromString = YPrim . S.fromString

defIndent :: Int
defIndent = 4 

buildYaml :: Int -> YamlValue -> Builder 
buildYaml n (YObject m) = (mconcat . intersperse (newLine <> makeIndent n) . map (buildPair n) ) m
buildYaml n (YLArray sty xs) = buildList sty n xs 
buildYaml n (YIArray xs) = buildItemList n xs
buildYaml n (YPrim p) = buildPrim p 

buildItemList :: Int -> [YamlValue] -> Builder
buildItemList n (x:xs) = makeIndent n <> newLine
                         <> makeIndent n <> fromLazyText "- " 
                         <> buildYaml (n+2) x <> newLine
                         <> mconcat (map buildItem xs)
  where buildItem x = 
          makeIndent n <> fromLazyText "- " <> buildYaml (n+2) x <> newLine
buildItemList n _ = mempty

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
                         -- <> makeIndent n <> fromLazyText "] "

buildPrim (YNumber s) = scientificBuilder s 
buildPrim (YInteger s) = (fromLazyText . T.pack . show) s
buildPrim (YString txt) = fromLazyText txt
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

