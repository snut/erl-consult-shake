module Development.Shake.Erl.Consult
  ( ErlTerm(..)
  , consultFile
  , consultString
  , consult
  , pretty
  , pretties
  ) where

import Text.ParserCombinators.Parsec hiding ( (<|>), many, optional)
import Control.Applicative

import qualified Data.Text as Text
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Lazy as B

import Development.Shake.Erl.Types

-- doesn't handle files with first line comment followed by blank line

consultFile :: FilePath -> IO (Either ParseError [ErlTerm])
consultFile path = readFile path >>= (return . parse consult path . stripComments)

--maybeConsultFile path = 

consultString = parse consult "(consult string)" . stripComments

stripComments = unlines . filter (not . null . trimL) . filter (not . headMatch '%' . trimL) . lines

headMatch x (y:ys) | x == y    = True
                   | otherwise = headMatch x ys
headMatch _ [] = False

trimL = dropWhile (`elem` " \t")

eol = char '\n' <?> "end of line"

termSeq = sepBy erlTerm sepTerm
endTerm = optional spaces >> char '.' >> optional comment >> optional eol
sepTerm = optional spaces >> char ',' >> optional comment

consult :: GenParser Char st [ErlTerm]
consult = endBy erlTerm endTerm

erlTerm  = spaces *> erlTerm'  <* optional comment
erlTerm' = tuple
       <|> list
       <|> estring
       <|> real
       <|> integer
       <|> atom
       <|> sbinary
       <|> binary
       <?> "erlang term"

tuple   = ErlTuple <$> between (char '{') (char '}') termSeq
list    = ErlList  <$> between (char '[') (char ']') termSeq
estring = ErlString . Text.pack . concat <$> many1 (between (char '"') (char '"') (many stringChar) <* spaces)
real    = ErlReal . read <$> realContent
integer = ErlInt . read <$> ((:) <$> sign <*> many1 digit)
atom    = ErlAtom <$> atomContent
sbinary = ErlBinary . B.pack . map (fromIntegral.fromEnum) <$> between (try $ string "<<\"") (string "\">>") (many stringChar)
binary  = ErlBinary . B.pack . map read <$> between (string "<<") (string ">>") bytes

bytes = sepBy (spaces *> many1 digit) (spaces *> char ',')

comment = spaces *> optional (char '%' *> many1 (noneOf "\n\r")) *> spaces

-- Erlang allows unary +, Haskell does not
sign = leadSign <$> optional (char '+' <|> char '-')
  where leadSign (Just '-') = '-'
        leadSign _ = ' '

realContent = try $ do
  s <- sign
  pref <- many1 digit
  point <- char '.'
  suff <- many1 digit
  return $ s : pref ++ point : suff

realContentA = (++) <$> ((:) <$> sign <*> many1 digit) <*> ((:) <$> char '.' <*> many1 digit)

stringChar :: GenParser Char st Char
stringChar = try escaped
          <|> (noneOf "\"")
          <?> "character, escape sequence or end of string"

escaped = char '\\' *> escapable
escapable =  char '"'
         <|> char 'n' *> pure '\n'
         <|> char 't' *> pure '\t'
         <|> char 'r' *> pure '\r'

atomContent =  between (char '\'') (char '\'') (many (noneOf "'"))
           <|> ((:) <$> oneOf ['a' .. 'z'] <*> many (oneOf ('_' : ['a' .. 'z'] ++ ['0' .. '9'])))

