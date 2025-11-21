module Analysis (countWordSentenceHits, findMaxWords) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (foldl')

import Tokenization (WordT, Sentence, wordsInSentence)

countWordSentenceHits :: [Sentence] -> M.Map WordT Int
countWordSentenceHits sents = M.map S.size mset
  where
    indexed = zip [0..] sents

    insertSent m (idx, sent) =
      foldl'
        (\acc w -> M.insertWith S.union w (S.singleton idx) acc)
        m
        (S.toList $ wordsInSentence sent)

    mset = foldl' insertSent M.empty indexed

findMaxWords :: M.Map WordT Int -> (Int, [WordT])
findMaxWords m
  | M.null m  = (0, [])
  | otherwise = (mx, map fst $ filter ((== mx) . snd) (M.toList m))
  where
    mx = maximum (M.elems m)
