--module Day2 where

readWords :: FilePath -> IO [String]
readWords = fmap words . readFile

splitValues :: IO [String] -> IO ([String], [Int])
splitValues wordsRead =


--readDepths :: IO [String] -> IO (String, Int)
--readDepths inputLines = do
--  input <- inputLines
--  --command <- input !! 0
--  value <- input !! 1
--  valueInt <- read value
--  return ("", value)
--  --return $ fmap words input

--test filename =
--  readDepths $ readLines filename

main :: IO ()
main = do
  content <- test "input.txt"
  print content
--  rawContent <- readLines "input.txt"
--  print "test"
  --splitContent <- readDepths rawContent
  --print splitContent
