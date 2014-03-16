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
import Prelude hiding (takeWhile,dropWhile)
-- import Debug.Trace

-- | parsed yaml 
data PYaml = PYObject [ (T.Text, PYaml) ]
           | PYText T.Text
           | PYList [T.Text]
           deriving (Show, Eq)

-- | generic text parser with designated delimiters
p_text :: [Char] -> Parser T.Text
p_text delim = 
    T.strip <$> takeTill (\x -> x `elem` delim) 

-- | detect indentation
p_indent :: Parser Int 
p_indent = T.length <$> takeWhile (== ' ') 

-- | comment parser
p_comment :: Parser T.Text
p_comment = char '#' *> takeTill (== '\n') <* char '\n'

-- | combinator for getting rid of comment out of a parser
p_commentOut :: (Monoid a) => Parser a -> Parser a
p_commentOut p = mconcat <$> (p `sepBy` p_comment) 

-- | no comment text
p_textNoComment :: [Char] -> Parser T.Text
p_textNoComment delim = 
    T.strip <$> p_commentOut content
  where content = takeTill (\x -> x `elem` '#':delim)
        

-- | convenient function for sepBy with indentation and comment
p_sepBy1CommentAndIndent :: Int -> Parser a -> Parser [a]
p_sepBy1CommentAndIndent n p = p `sepBy1` p_sep
  where spaces x = replicateM x (char ' ')
        p_sep = many1 p_emptyline >> spaces n

-- | parser for a list with [ ] flow mode
p_list :: Parser a -> Parser [a]
p_list p = char '[' 
           *> (p `sepBy1` (char ','))
           <* char ']'

-- | list with flow with indentation and - 
p_itemlist :: Int -> Parser a -> Parser [a]
p_itemlist n p = p_sepBy1CommentAndIndent n line
  where line = char '-' *> skipSpace *> p  


-- | indentation-aware key value pair parser
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

-- | parse key text. First output is the total length of text 
-- consumed before ':'.  
p_key :: Parser (Int,T.Text)
p_key = do
    c1 <- satisfy (notInClass [ ' ', '#', '\n', '-' ])
    txt' <- takeTill (`elem` [':','\n','#'])
    let txt = c1 `T.cons` txt' 
    return (T.length txt, T.strip txt) 

-- | primary text
p_ptext :: Parser T.Text
p_ptext = do 
    c1 <- satisfy (notInClass [ ' ', '#', '\n', '-' ]) 
    txt' <- takeTill (`elem` [':','#','\n',','] )
    let txt = c1 `T.cons` txt' 
    return (T.strip txt) 

-- | line breaker (either a simple newline or comment)
p_linebreaker :: Parser ()
p_linebreaker = (p_comment >> return ()) 
                <|> (char '\n' >> return ())

-- | empty line
p_emptyline :: Parser ()
p_emptyline = takeTill (/= ' ')>> p_linebreaker

-- | generic parsed yaml object
p_object :: Int -> Parser PYaml 
p_object n = do 
    try (PYList <$> p_list (p_text [',',']']))
    <|> try (PYList <$> p_itemlist n (p_text ['\n','#']))
    <|> try (do kvlst <- p_sepBy1CommentAndIndent n content 
                return (PYObject kvlst))
    <|> PYText <$> p_ptext
  where 
    content = p_keyvalue p_object

p_yaml :: Parser PYaml
p_yaml = do
  many p_emptyline >> p_indent >>= p_object

-- | testing with a given file
test :: FilePath -> IO ()
test fp = do    
    txt <- TIO.readFile fp 
    print (parseOnly p_yaml txt)

