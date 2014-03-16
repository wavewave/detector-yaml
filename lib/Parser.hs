{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where 

import Control.Applicative 
import Control.Monad (replicateM)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text 
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
--
import Prelude hiding (takeWhile)
-- import Debug.Trace

data PYaml = PYObject [ (T.Text, PYaml) ]
           | PYText T.Text 
           deriving (Show, Eq)

p_text :: [Char] -> Parser T.Text
p_text delim = 
    T.strip <$> takeTill (\x -> x `elem` delim) 

p_indent :: Parser Int 
p_indent = T.length <$> takeWhile (== ' ') 


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
               <* p_linebreaker
-- ((p_comment *> return ()) 
--                    <|> (char '\n' *> return ()))

p_keyvalue :: (Int -> Parser b) 
           -> Parser (T.Text,b)
p_keyvalue pv = do 
    (n1,k) <- p_key 
    char ':' 
    spcs <- Data.Attoparsec.Text.takeWhile (== ' ')
    v <- (try (char '\n' *> (p_indent >>= pv)))
          <|> (let n2 = T.length spcs + 1
               in pv (n1 + n2))
    return (k,v)

p_key :: Parser (Int,T.Text)
p_key = do
    c1 <- notChar ' ' 
    txt' <- takeTill (`elem` [':','\n','#'])
    let txt = c1 `T.cons` txt' 
    return (T.length txt, T.strip txt) 

p_ptext :: Parser T.Text
p_ptext = do 
    c1 <- notChar ' ' 
    txt' <- takeTill (`elem` [':','#','\n'] )
    let txt = c1 `T.cons` txt' 
    return (T.strip txt) 


p_linebreaker :: Parser ()
p_linebreaker = (p_comment >> return ()) 
                <|> (char '\n' >> return ())

p_object :: Int -> Parser PYaml 
p_object n = do 
    kvlst <- content `sepBy` (p_linebreaker >> spaces n)
    if (not.null) kvlst 
      then return (PYObject kvlst) 
      else PYText <$> p_ptext


  where 
    content = p_keyvalue p_object
    spaces x = replicateM x (char ' ')



test :: FilePath -> IO ()
test fp = do    
    txt <- TIO.readFile fp 
    print (parseOnly (p_indent >>= p_object) txt)
    let content = p_keyvalue (const (p_text ['\n','#']))

    print (parseOnly (replicateM 4 (char ' ') *> content) txt) 
