module Main where

import Data.Char (isLetter, toLower)
import Data.List (foldl', intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory (doesFileExist)
import System.IO (writeFile)

type Symbol = Char
newtype WordT = WordT String deriving (Eq, Ord)

instance Show WordT where
  show (WordT s) = s

data Token = W WordT | Punct Symbol deriving (Show)
type Sentence = [Token]

normalizeSpaces :: String -> String
normalizeSpaces = collapse . map replaceNewlineTab
  where
    replaceNewlineTab c | c `elem` ['\t','\n','\r'] = ' '
                        | otherwise = c
    collapse = reverse . foldl' f []
    f [] ' ' = [' ']
    f acc@(' ':_) ' ' = acc
    f acc c = c : acc

splitSentences :: String -> [String]
splitSentences s = map trim $ go s ""
  where
    isTerm c = c `elem` ".!?"
    go [] acc | null acc = []
              | otherwise = [acc]
    go (c:cs) acc
      | isTerm c = let acc' = acc ++ [c] in acc' : go (dropWhile (== ' ') cs) ""
      | otherwise = go cs (acc ++ [c])
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

isWordChar :: Char -> Bool
isWordChar c = isLetter c || c == '\'' || c == '-'

tokenize :: String -> [Token]
tokenize = go []
  where
    go acc [] = reverse acc
    go acc (c:cs)
      | c == ' ' = go acc cs
      | isWordChar c =
          let (w,rest) = span isWordChar (c:cs)
          in go (W (WordT w) : acc) rest
      | otherwise = go (Punct c : acc) cs

wordsInSentence :: Sentence -> S.Set WordT
wordsInSentence toks = S.fromList [ WordT (map toLower s) | W (WordT s) <- toks ]

countWordSentenceHits :: [Sentence] -> M.Map WordT Int
countWordSentenceHits sents = M.map S.size mset
  where
    indexed = zip [0..] sents
    insertSent m (idx, sent) =
      foldl' (\acc w -> M.insertWith S.union w (S.singleton idx) acc) m (S.toList $ wordsInSentence sent)
    mset = foldl' insertSent M.empty indexed

findMaxWords :: M.Map WordT Int -> (Int, [WordT])
findMaxWords m
  | M.null m = (0, [])
  | otherwise = (mx, map fst $ filter ((== mx) . snd) $ M.toList m)
  where
    mx = maximum (M.elems m)

parseSentence :: String -> Sentence
parseSentence = tokenize

main :: IO ()
main = do
  let path = "book.txt"
  exists <- doesFileExist path

  if not exists
    then putStrLn "Помилка: файл book.txt не знайдено у поточній папці."
    else do
      input <- readFile path
      let norm = normalizeSpaces input
      writeFile "normalized.txt" norm

      let rawSents = splitSentences norm
          sents = map parseSentence rawSents
          counts = countWordSentenceHits sents
          (mx, wordsMax) = findMaxWords counts

      putStrLn $ "Кількість речень у тексті: " ++ show (length sents)
      putStrLn $ "Найбільша кількість речень, в яких трапляється одне й те саме слово: " ++ show mx
      putStrLn $ "Слова, що зустрічаються у " ++ show mx ++ " реченнях:"
      putStrLn $ intercalate ", " (map (\(WordT w) -> w) wordsMax)
      putStrLn "\nНормалізований текст збережено у файлі normalized.txt"
