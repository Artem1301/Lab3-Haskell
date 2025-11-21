module Main where

import System.Directory (doesFileExist)
import System.IO (writeFile)
import Data.List (intercalate)

import Normalization (normalizeSpaces, splitSentences)
import Tokenization (tokenize, WordT(..), Sentence, Token)
import Analysis (countWordSentenceHits, findMaxWords)

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
          sents = map tokenize rawSents
          counts = countWordSentenceHits sents
          (mx, wordsMax) = findMaxWords counts

      putStrLn $ "Кількість речень у тексті: " ++ show (length sents)
      putStrLn $ "Найбільша кількість речень, в яких трапляється одне й те саме слово: " ++ show mx
      putStrLn $ "Слова, що зустрічаються у " ++ show mx ++ " реченнях:"
      putStrLn $ intercalate ", " (map (\(WordT w) -> w) wordsMax)
      putStrLn "\nНормалізований текст збережено у файлі normalized.txt"
