module Development.Shake.Erl.Types where

import Data.List (intersperse)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)

import qualified Data.Text as Text
import Data.Text (Text)

data ErlTerm = ErlTuple [ErlTerm]
             | ErlList [ErlTerm]
             | ErlString Text
             | ErlReal Double
             | ErlInt Int
             | ErlAtom String
             | ErlBinary ByteString
             deriving (Eq, Show, Read)

pretty :: ErlTerm -> String
pretty = (++ ".") . prettyIndent 0

pretties :: [ErlTerm] -> String
pretties = unlines . map pretty

prettyIndent n (ErlTuple xs) = wrap "{" "}" $ prettySeq n xs
prettyIndent n (ErlList xs)  = wrap "[" "]" $ prettySeq n xs
prettyIndent _ (ErlString s) = wrap "\"" "\"" . escape $ Text.unpack s
prettyIndent _ (ErlAtom s) = s
prettyIndent _ (ErlReal r) = show r
prettyIndent _ (ErlInt i) = show i
prettyIndent n (ErlBinary xs) = wrap "<<" ">>" . concat . intersperse "," . map show . B.unpack $ xs

wrap before after s = concat [before, s, after]

prettySeq n xs = concat $ if totalLen > 64
    then " " : intersperse line ss
    else intersperse comma ss
  where
    ss = map (prettyIndent (succ n)) xs
    totalLen = length . concat $ ss
    line = '\n' : replicate (n*2) ' ' ++ ", "
    comma = ", "

escape ('\n':xs) = '\\' : 'n' : escape xs
escape ('\t':xs) = '\\' : 't' : escape xs
escape ('\r':xs) = escape xs
escape (x:xs) = x : escape xs
escape [] = []
