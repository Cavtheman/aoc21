readLines :: FilePath -> IO ([String], [Int])
readLines filename = do
  strings <- fmap lines . readFile $ filename
  let dirs = map (head . words) strings
      nums = map (read . head . tail . words) strings
  return (dirs, nums)

calcPosition1 :: Int -> Int -> [String] -> [Int] -> Int
calcPosition1 depth dist [] [] = depth * dist
calcPosition1 depth dist (dir:dirs) (amount:amounts) =
  case dir of
    "forward" -> calcPosition1 depth (dist + amount) dirs amounts
    "down"    -> calcPosition1 (depth + amount) dist dirs amounts
    "up"      -> calcPosition1 (depth - amount) dist dirs amounts
    _         -> error "Invalid direction"
calcPosition1 _ _ _ _ = error "Input lists had different length"

calcPosition2 :: Int -> Int -> Int -> [String] -> [Int] -> Int
calcPosition2 depth dist _ [] [] = depth * dist
calcPosition2 depth dist aim (dir:dirs) (amount:amounts) =
  case dir of
    "forward" -> calcPosition2 (depth + aim * amount) (dist + amount) aim dirs amounts
    "down"    -> calcPosition2 depth dist (aim + amount) dirs amounts
    "up"      -> calcPosition2 depth dist (aim - amount) dirs amounts
    _         -> error "Invalid direction"
calcPosition2 _ _ _ _ _ = error "Input lists had different length"

main :: IO ()
main = do
  (dirs, amounts) <- readLines "inputSimple.txt"
  let part1 = calcPosition1 0 0 dirs amounts
  let part2 = calcPosition2 0 0 0 dirs amounts
  print part1
  print part2
