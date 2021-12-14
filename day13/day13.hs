import Data.List

data Dir = Horizontal
         | Vertical
  deriving (Eq, Show)
type Fold = (Dir, Int)
type Paper = [(Int, Int)]

-- Lifted off the internet
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

splitByFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitByFirst predicate lst =
  let helper _acc [] = error "Not found"
      helper acc (x:xs) | predicate x = (reverse acc, xs)
      helper acc (x:xs) = helper (x:acc) xs
  in helper [] lst

--readPaper :: FilePath -> IO ([Fold], Paper)
readPaper :: FilePath -> IO ([Fold], Paper)
readPaper filename = do
  contents <- fmap lines . readFile $ filename
  let readDir "x" = Vertical
      readDir "y" = Horizontal
      readDir _   = error "Invalid fold direction"
      (dots, folds) = splitByFirst ((==) "") contents
      dots' = map ((\[x,y] -> (x,y)) . map read . wordsWhen ((==) ',')) dots
      folds' = map ((\[x,y] -> (readDir x,read y)) . (\x -> wordsWhen ((==) '=') x) . drop 11) folds
  return (folds', dots')

makeFold :: Fold -> Paper -> Paper
makeFold (ax, pos) dots =
  let mapFun (x,y) = case ax of
                       Horizontal | y > pos -> (x, 2 * pos - y)
                       Vertical   | x > pos -> (2 * pos - x, y)
                       _ -> (x,y)
  in (nub . map mapFun) dots

makeAllFolds :: [Fold] -> Paper -> Paper
makeAllFolds [] paper = paper
makeAllFolds (x:xs) paper = makeAllFolds xs $ makeFold x paper

--dotArr :: (Int, Int) -> Paper -> String
dotArr (height, width) paper =
  let strList = [case (x < width - 1, (x,y) `elem` paper) of
                   (True, True)   -> "#"
                   (True, False)  -> " "
                   (False, True)  -> "#\n"
                   (False, False) -> " \n"
                | y <- [0..height-1]
                , x <- [0..width-1]]
  in concat strList

main :: IO ()
main = do
  --(folds, _paper) <- readPaper "inputSimple.txt"
  (folds, paper) <- readPaper "input.txt"
  let singleFold = makeFold (head folds) paper
      allFolds = makeAllFolds folds paper
  print $ "Number of dots: " ++ show (length singleFold)
  putStr $ dotArr (6,40) allFolds
  --print paper
