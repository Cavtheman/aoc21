import Data.List
import qualified Data.Map.Strict as M

import Data.Maybe

-- Lifted off the internet
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

digitToSegments :: M.Map Int String
digitToSegments = M.fromList [
  (0, "abcefg"), (1, "cf"), (2, "acdeg"), (3, "acdfg"), (4, "bcdf"),
  (5, "abdfg"), (6, "abdefg"), (7, "acf"), (8, "abcdefg"), (9, "abcdfg")]

getEasyDigits :: [String] -> [Maybe Int]
getEasyDigits scrambled =
  map getEasyDigit scrambled
  where getEasyDigit digit = case length digit of
                               2 -> Just 1
                               3 -> Just 7
                               4 -> Just 4
                               7 -> Just 8
                               _ -> Nothing


readDigits :: FilePath -> IO ([[String]], [[String]])
readDigits filename = do
  strings <- fmap lines . readFile $ filename
  let splitDigits = map (wordsWhen (== '|')) strings
      signatures = map ((map sort) . words . head) splitDigits
      outputs = map ((map sort) . words . head . tail) splitDigits
  return $ (signatures, outputs)


sumSimpleDigits :: [[String]] -> Int
sumSimpleDigits input =
  length $ concat $ map ((filter isJust) . getEasyDigits) input

findLen :: Int -> [String] -> String
findLen n str = head $ filter ((n ==) . length) str

easyMappings :: [String] -> M.Map Int String
easyMappings signatures = M.fromList [(1, findLen 2 signatures),
                                      (7, findLen 3 signatures),
                                      (4, findLen 4 signatures),
                                      (8, findLen 7 signatures)]

--deduceNine :: [String] -> M.Map Int String -> M.Map Int String
deduceNine signatures mappings =
  let four  = mappings M.! 4
      seven = mappings M.! 7
      fourSeven = sort $ union seven four
      nineMap
      --nineMap = filter (\x -> length (filter (\y -> y `elem` fourSeven) x) == 1) signatures
  --in M.fromList [(9, fourSeven)]
  in nineMap
  --in map (\x -> x \\ fourSeven) signatures


main :: IO ()
main = do
  (signatures, outputs) <- readDigits "input.txt"
  let mappings = easyMappings $ head signatures
      nineMapping = deduceNine (head signatures) mappings

  print $ sumSimpleDigits outputs
  print mappings
  print nineMapping
