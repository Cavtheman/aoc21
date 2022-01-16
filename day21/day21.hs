import Data.List

readPositions :: FilePath -> IO (Int, Int)
readPositions filename = do
  contents <- fmap lines . readFile $ filename
  let [p1, p2] = map (read . pure . last) contents
  return (p1, p2)

type Player = (Int, Int)

playGame :: Player -> Player -> Int -> Int -> Int
playGame (_, losingScore) (_, winningScore) numRolls _ | winningScore >= 1000 = numRolls * losingScore
playGame rollPlayer waitPlayer numRolls lastRoll =
  let (place, score) = rollPlayer
      newPlace = (place + lastRoll + lastRoll + 2 + lastRoll + 3) `mod` 10 + 1
  in playGame waitPlayer (newPlace, score + newPlace) (numRolls + 3) (lastRoll + 3)

main :: IO ()
main = do
  (p1, p2) <- readPositions "input.txt"
  let temp = playGame (p1, 0) (p2, 0) 0 0
  print (p1, p2)
  print temp
  print $ sort [ sum [r1,r2,r3] | r1 <- [1,2,3], r2 <- [1,2,3], r3 <- [1,2,3] ]
