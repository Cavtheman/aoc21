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

-- Part 1
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

-- Part 2
data Restricted a = Once a | Twice a
  deriving (Show, Eq)

goTo :: Cave -> Restricted [Cave] -> Restricted [Cave]
goTo cave (Once traversed) | isBigCave cave = Once $ cave:traversed
goTo cave (Twice traversed) | isBigCave cave = Twice $ cave:traversed

goTo cave (Once traversed) = if cave `elem` traversed
                             then Twice $ cave:traversed
                             else Once $ cave:traversed
goTo cave (Twice traversed) = if cave `elem` traversed
                              then Twice traversed
                              else Twice $ cave:traversed

validCave :: Cave -> Restricted [Cave] -> Bool
validCave cave _traversed | isBigCave cave = True
validCave "start" _traversed = False
validCave _cave (Once _traversed) = True
validCave cave (Twice traversed) = cave `notElem` traversed

traverseCavesTwice :: CaveMap -> Restricted [Cave] -> Cave -> Cave -> [Restricted [Cave]]
traverseCavesTwice _caveMap traversed start end | start == end = [goTo start traversed]
traverseCavesTwice caveMap traversed start end =
  let newTraversed = goTo start traversed
  in [path | option <- caveMap ! start, validCave option newTraversed
           , path <- traverseCavesTwice caveMap newTraversed option end]

main :: IO ()
main = do
  input <- readCaves "input.txt"
  let caveMap = genMap input
      traversals = traverseCaves caveMap [] "start" "end"
      nTraversals = traverseCavesTwice caveMap (Once []) "start" "end"
  print $ length traversals
  print $ length nTraversals
