import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!))

splitByFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitByFirst predicate lst =
  let helper _acc [] = error "Not found"
      helper acc (x:xs) | predicate x = (reverse acc, xs)
      helper acc (x:xs) = helper (x:acc) xs
  in helper [] lst

readPolymers :: FilePath -> IO (String, Map String Char)
readPolymers filename = do
  contents <- fmap lines . readFile $ filename
  let ([template], rules) = splitByFirst ((==) "") contents
      rules' = M.fromList $ map ((\[x,_,y] -> (x, head y)) . words) rules
  return (template, rules')

expandPolymer :: Char -> Char -> Map String Char -> Map Char Int -> Int -> Map Char Int
expandPolymer _ _ _ charCounts 0 = charCounts
expandPolymer x y rules charCounts _n | (x:[y]) `M.notMember` rules =
                                        M.insertWith (+) y 1 $ M.insertWith (+) x 1 charCounts
expandPolymer x y rules charCounts n =
  let midChar = rules ! (x:[y])
      leftMap = expandPolymer x midChar rules charCounts (n-1)
      rightMap = expandPolymer midChar y rules leftMap (n-1)
  in M.insertWith (+) midChar 1 $ rightMap

expandFullPolymer :: String
                  -> Map String Char
                  -> Map (Char, Char) (Map Char Int)
                  -> Map Char Int
                  -> Int
                  -> Map Char Int
expandFullPolymer [] _rules _cachedCalcs charCount _n = charCount
expandFullPolymer (x:[]) _rules _cachedCalcs charCount _n = M.insertWith (+) x 1 charCount
expandFullPolymer (x:y:xs) rules cachedCalcs charCount n =
  if (x,y) `M.member` cachedCalcs
  then let thisRes = cachedCalcs ! (x,y)
           --newCharCount = M.unionWith (+) thisRes charCount
           recCall = expandFullPolymer (y:xs) rules cachedCalcs charCount n
       in M.unionWith (+) thisRes recCall
  else let thisRes = M.insertWith (+) x 1 $ expandPolymer x y rules charCount n
           --newCharCount = M.unionWith (+) thisRes charCount
           newCachedCalcs = M.insert (x,y) thisRes cachedCalcs
           recCall = expandFullPolymer (y:xs) rules newCachedCalcs charCount n
       in M.unionWith (+) thisRes recCall
--expandFullPolymer polymer rules cachedCalcs charCount n =
  --let helper [] charCount = charCount
  --    helper (x:[]) charCount = M.insertWith (+) x 1 charCount
  --    helper (x:y:xs) charCount =
  --      if (x,y) `M.member` cachedCalcs
  --      then
  --        M.unionWith (M.unionWith (+))
  --        (cachedCalcs ! (x,y))
  --        (expandFullPolymer (y:xs) rules cachedCalcs n)
  --        --M.unionWith (+)
  --        --(M.insertWith (+) x (cachedCalcs ! (x,y)) charCount)
  --        --(expandFullPolymer (y:xs) rules cachedCalcs n)
  --      else
  --        let expanded = M.insertWith (+) x 1 $ expandPolymer x y rules charCount n
  --            newCachedCalcs = M.insertWith (M.unionWith (+)) expanded
  --        in M.unionWith (+) expanded (expandFullPolymer (y:xs) rules newCachedCalcs n)
  --      --else
  --      --  let expanded = M.insertWith (+) x 1 $ expandPolymer x y rules charCount n
  --      --      newCacheVal = expanded ! x
  --      --      newCachedCalcs = M.insertWith (+) (x,y) newCacheVal cachedCalcs
  --      --  in M.unionWith (+)
  --      --     expanded
  --      --     (expandFullPolymer (y:xs) rules newCachedCalcs n)
  --in helper polymer mempty

main :: IO ()
main = do
  (template, rules) <- readPolymers "inputSimple.txt"
  let fullyExpanded = expandFullPolymer template rules mempty mempty 40
      (mostCommon, leastCommon) = M.foldr (\x (maxAcc, minAcc) -> case (x > maxAcc, x < minAcc) of
                                                                    (True, _) -> (x, minAcc)
                                                                    (_, True) -> (maxAcc, x)
                                                                    _ -> (maxAcc, minAcc)
                                          ) (0,maxBound :: Int) fullyExpanded
  print $ fullyExpanded
  print $ "Part 1: " ++ show (mostCommon - leastCommon)
  print $ expandPolymer 'N' 'N' rules mempty 1
