import qualified Data.Map as Map

-- Lifted off the internet
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

-- Dumb initial solutions for part 1
dumbReadFish :: FilePath -> IO [Int]
dumbReadFish filename = do
  strings <- fmap (wordsWhen (== ',')) . readFile $ filename
  return $ map read strings

dumbOneDay :: [Int] -> [Int]
dumbOneDay (0:xs) = 6:8:dumbOneDay xs
dumbOneDay (x:xs) = x-1:dumbOneDay xs
dumbOneDay [] = []

dumbNDays :: [Int] -> Int -> [Int]
dumbNDays fish 0 = fish
dumbNDays fish n = dumbNDays (dumbOneDay fish) (n-1)

dumbNumFish :: FilePath -> Int -> IO Int
dumbNumFish filename days = do
  firstFish <- dumbReadFish filename
  let lastFish = dumbNDays firstFish days
  return $ length lastFish


-- Significantly better version for part 2
readFish :: FilePath -> IO (Map.Map Int Int)
readFish filename = do
  strings <- fmap (wordsWhen (== ',')) . readFile $ filename
  let ints = map read strings
      toMap [] = Map.empty
      toMap (x:xs) = Map.insertWith (+) x 1 (toMap xs)
  return $ toMap ints

oneDay :: Map.Map Int Int -> Map.Map Int Int
oneDay fish =
  let folder 0 val acc = Map.insertWith (+) 8 val $ Map.insertWith (+) 6 val acc
      folder key val acc = Map.insertWith (+) (key - 1) val acc
  in Map.foldrWithKey folder Map.empty fish


nDays :: Map.Map Int Int -> Int -> Map.Map Int Int
nDays fish 0 = fish
nDays fish n = nDays (oneDay fish) (n-1)

numFish :: FilePath -> Int -> IO Int
numFish filename days = do
  firstFish <- readFish filename
  let lastFish = nDays firstFish days
  return $ fst $ Map.mapAccum (\acc x -> (acc+x, x)) 0 lastFish
