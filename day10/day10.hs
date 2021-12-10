import Data.List
import qualified Data.Map.Strict as M

corruptPoints :: M.Map Char Int
corruptPoints = M.fromList [(')', 3), (']', 57), ('}', 1197), ('>',25137)]

incompletePoints :: M.Map Char Int
incompletePoints = M.fromList [(')', 1), (']', 2), ('}', 3), ('>',4)]

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

fixIncompletes :: [Char] -> String -> Either Char [Char]
fixIncompletes expected line =
  case line of
    [] -> return expected
    '(':xs -> fixIncompletes (')':expected) xs
    '[':xs -> fixIncompletes (']':expected) xs
    '{':xs -> fixIncompletes ('}':expected) xs
    '<':xs -> fixIncompletes ('>':expected) xs
    x:xs   -> case expected of
                y:ys | x == y -> fixIncompletes ys xs
                _ -> Left x -- First corrupt char

autoPoints :: [String] -> [Int]
autoPoints autocompletes =
  sort [foldl (\acc x -> acc * 5 +  incompletePoints M.! x) 0 auto | auto <- autocompletes]


main :: IO ()
main = do
  input <- readLines "input.txt"
  let results = map (fixIncompletes []) input
      (corrupted, incomplete) = foldr (\x (corr, inc) -> case x of
                                                         Right x' -> (corr, x':inc)
                                                         Left x'  -> (x':corr, inc)) ([],[]) results
      corruptResults = map (\x -> corruptPoints M.! x) corrupted
      incompleteResults = autoPoints incomplete
  print $ sum corruptResults
  print $ incompleteResults !! (length incompleteResults `div` 2)
