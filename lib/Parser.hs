{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where 

import Control.Applicative 
import Control.Monad (replicateM)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text 
-- import qualified Data.ByteString.Char8 as B
-- import Data.Char (isSpace)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
--
-- import Debug.Trace

data PYaml = PYObject [ (T.Text, T.Text) ]
           deriving (Show, Eq)

p_text :: [Char] -> Parser T.Text
p_text delim = 
    T.strip <$> takeTill (\x -> x `elem` delim) 

p_comment :: Parser T.Text
p_comment = char '#' *> takeTill (== '\n') <* char '\n'

p_commentOut :: (Monoid a) => Parser a -> Parser a
p_commentOut p = mconcat <$> (p `sepBy` p_comment) 

p_textNoComment :: [Char] -> Parser T.Text
p_textNoComment delim = 
    T.strip <$> p_commentOut content
  where content = takeTill (\x -> x `elem` '#':delim)
        
p_list :: Parser a -> Parser [a]
p_list p = char '[' 
           *> (p `sepBy` (char ','))
           <* char ']'

p_itemlist :: Int -> Parser a -> Parser [a]
p_itemlist n p = many line
  where line = replicateM n (char ' ') 
               *> char '-'
               *> p  
               <* ((p_comment *> return ()) 
                   <|> (char '\n' *> return ()))

p_keyvalue :: Parser a -> Parser b -> Parser (a,b)
p_keyvalue pk pv = do k <- pk 
                      char ':' 
                      v <- pv
                      return (k,v)

p_linebreaker :: Parser ()
p_linebreaker = (p_comment >> return ()) 
                <|> (char '\n' >> return ())

p_object :: Int -> Parser PYaml 
p_object n = do 
    spcs <- Data.Attoparsec.Text.takeWhile (== ' ')
    let m = T.length spcs
    kvlst <- 
      content `sepBy` (p_linebreaker >> spaces (n+m)) 
    return (PYObject kvlst)
  where 
    content = p_keyvalue (p_text ['\n','#',':']) (p_text ['\n','#']) 
    spaces x = replicateM x (char ' ')



test :: FilePath -> IO ()
test fp = do    
    txt <- TIO.readFile fp 
    {- 
    let r1 = parseOnly (p_list (p_textNoComment [',',']']))
               "[ hello #this is a comment \n , world ]"
        r2 = parseOnly 
               (p_keyvalue (p_text ['\n',':']) (p_text ['\n']))
               " hello : world "
        r3 = parseOnly 
               (p_itemlist 2 (p_text ['\n','#']))
               "  - test \n  - test2  \n  - test3 #hello \n"
        r4 = parseOnly (p_object 4)
               "      test : okay \n      test2 : true \n" 
    -} 
    print (parseOnly (p_object 0) txt)
    -- print r1
    -- print r2
    -- print r3
    -- print r4