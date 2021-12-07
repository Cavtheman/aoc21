
-- Lifted off the internet
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

readCrabs :: FilePath -> IO [Int]
readCrabs filename = do
  strings <- fmap (wordsWhen (== ',')) . readFile $ filename
  return $ map read strings

findMinFuelUse1 :: [Int] -> Int
findMinFuelUse1 crabs =
  let potFuelVals = [minimum crabs..maximum crabs]
      fuelVals = map (\i -> sum $ map (\crab -> abs $ crab - i) crabs) potFuelVals
  in minimum fuelVals

findMinFuelUse2 :: [Int] -> Int
findMinFuelUse2 crabs =
  let potFuelVals = [minimum crabs..maximum crabs]
      sumFun n = (n * (n+1)) `div` 2
      fuelVals = map (\i -> sum $ map (\crab -> sumFun $ abs $ crab - i ) crabs) potFuelVals
  in minimum fuelVals

main :: IO ()
main = do
  crabs <- readCrabs "input.txt"
  print $ findMinFuelUse1 crabs
  print $ findMinFuelUse2 crabs
