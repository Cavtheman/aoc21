import Data.Char as C
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!))

type Cave = String
type CaveMap = Map Cave [Cave]

-- Lifted off the internet
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

readCaves :: FilePath -> IO [(Cave, Cave)]
readCaves filename = do
  contents <- fmap lines . readFile $ filename
  let temp = map (wordsWhen ((==) '-')) contents
      mapping = map (\[first, second] -> (first, second)) temp
  return mapping

genMap :: [(Cave, Cave)] -> CaveMap
genMap [] = mempty
genMap ((x,y):mappings) =
  let temp = M.insertWith (\new existing -> new ++ existing) x [y] (genMap mappings)
  in M.delete "end" $ M.insertWith (\new existing -> new ++ existing) y [x] temp

isBigCave :: Cave -> Bool
isBigCave s = foldr (&&) True $ map C.isUpper s

traverseCaves :: CaveMap -> [Cave] -> Cave -> Cave -> [[Cave]]
traverseCaves _caveMap traversed start end | start == end = [end:traversed]
traverseCaves caveMap traversed start end =
  let newCaveMap = M.map (\x -> if isBigCave start then x else filter ((/=) start) x) caveMap
  in [path | option <- newCaveMap ! start
           , path <- traverseCaves newCaveMap (start:traversed) option end]

main :: IO ()
main = do
  result <- readCaves "input.txt"
  let caveMap = genMap result
      traversals = traverseCaves caveMap [] "start" "end"
  --print traversals
  print $ length traversals
