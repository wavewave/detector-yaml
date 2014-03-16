{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where 

import Control.Applicative 
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text 
-- import qualified Data.ByteString.Char8 as B
-- import Data.Char (isSpace)
import Data.Monoid
import qualified Data.Text as T

{-
trim :: T.Text -> T.Text
trim xs = dropSpaceTail "" $ dropWhile T.isSpace xs
  where
    dropSpaceTail maybeStuff "" = ""
    dropSpaceTail maybeStuff (x:xs)
      | isSpace x = dropSpaceTail (x:maybeStuff) xs
      | null maybeStuff = x : dropSpaceTail "" xs
      | otherwise = reverse maybeStuff ++ x : dropSpaceTail "" xs
-}

p_list :: Parser a -> Parser [a]
p_list p = char '[' 
           *> (p `sepBy` (char ','))
           <* char ']'


p_commentOut :: (Monoid a) => Parser a -> Parser a
p_commentOut p = mconcat <$> (p `sepBy` p_comment) 

p_text :: [Char] -> Parser T.Text
p_text delim = 
    T.strip <$> takeTill (\x -> x `elem` delim) 
   

p_textNoComment :: [Char] -> Parser T.Text
p_textNoComment delim = 
    T.strip <$> p_commentOut content
    -- (content `sepBy` p_comment)
  where content = takeTill (\x -> x `elem` '#':delim)
        

p_keyvalue :: Parser a -> Parser b -> Parser (a,b)
p_keyvalue pk pv = do k <- pk 
                      char ':' 
                      v <- pv
                      return (k,v)

p_comment :: Parser T.Text
p_comment = char '#' *> takeTill (== '\n') <* char '\n'







test :: IO ()
test = do 
    let r1 = parseOnly (p_list (p_textNoComment [',',']']))
               "[ hello #this is a comment \n , world ]"
        r2 = parseOnly 
               (p_keyvalue (p_text ['\n',':']) (p_text ['\n']))
               " hello : world "
    print r1
    print r2