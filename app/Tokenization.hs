module Tokenization
  ( Token(..)
  , Symbol
  , WordT(..)
  , Sentence
  , tokenize
  , wordsInSentence
  ) where

import Data.Char (isLetter, toLower)
import qualified Data.Set as S

type Symbol = Char

newtype WordT = WordT String
  deriving (Eq, Ord)

instance Show WordT where
  show (WordT s) = s

data Token = W WordT | Punct Symbol
  deriving (Show)

type Sentence = [Token]

isWordChar :: Char -> Bool
isWordChar c = isLetter c || c == '\'' || c == '-'

tokenize :: String -> [Token]
tokenize = go []
  where
    go acc [] = reverse acc
    go acc (c:cs)
      | c == ' ' = go acc cs
      | isWordChar c =
          let (w, rest) = span isWordChar (c:cs)
          in go (W (WordT w) : acc) rest
      | otherwise = go (Punct c : acc) cs

wordsInSentence :: Sentence -> S.Set WordT
wordsInSentence toks =
  S.fromList [ WordT (map toLower s) | W (WordT s) <- toks ]
