module Normalization (normalizeSpaces, splitSentences) where

import Data.List (foldl')

normalizeSpaces :: String -> String
normalizeSpaces = collapse . map replaceNewlineTab
  where
    replaceNewlineTab c
      | c `elem` ['\t','\n','\r'] = ' '
      | otherwise = c

    collapse = reverse . foldl' f []
    f [] ' ' = [' ']
    f acc@(' ':_) ' ' = acc
    f acc c = c : acc

splitSentences :: String -> [String]
splitSentences s = map trim $ go s ""
  where
    isTerm c = c `elem` ".!?"
    go [] acc
      | null acc  = []
      | otherwise = [acc]
    go (c:cs) acc
      | isTerm c =
          let acc' = acc ++ [c]
           in acc' : go (dropWhile (== ' ') cs) ""
      | otherwise = go cs (acc ++ [c])

    trim = dropWhile (== ' ')
         . reverse
         . dropWhile (== ' ')
         . reverse
