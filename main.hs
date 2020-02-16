import Data.Function
import Data.List
import Data.List.Split

data SudokuValue = Val Int | Pos [Int] deriving (Show, Eq, Ord)

{-
  Step 1: Establish polymorphic data value
-}

testPuzzle :: [[Int]]
testPuzzle = [
    [5,3,0,0,7,0,0,0,0]
  , [6,0,0,1,9,5,0,0,0]
  , [0,9,8,0,0,0,0,6,0]
  , [8,0,0,0,6,0,0,0,3]
  , [4,0,0,8,0,3,0,0,1]
  , [7,0,0,0,2,0,0,0,6]
  , [0,6,0,0,0,0,2,8,0]
  , [0,0,0,4,1,9,0,0,5]
  , [0,0,0,0,8,0,0,7,9]
  ]

processPuzzleInitial :: [[Int]] -> [[SudokuValue]]
processPuzzleInitial values =
  let
    processRow row = map (\d -> if (d == 0) then Pos [1..9] else Val d) row
  in map processRow values

processRow :: [SudokuValue] -> [SudokuValue]
processRow row =
  let
    complete = filter (> 0) $ map (\sv -> case sv of
      Val x -> x
      Pos _ -> 0) row
  in
    map (\v -> case v of
      Val x -> Val x
      Pos xs -> Pos (xs \\ complete)) row

regionize :: [[SudokuValue]] -> [[SudokuValue]]
regionize sudoku =
  let
    cs = concatMap (chunksOf 3) sudoku
  in [
      concat $ cs!!0 : cs!!3 : cs!!6 : []
    , concat $ cs!!1 : cs!!4 : cs!!7 : []
    , concat $ cs!!2 : cs!!5 : cs!!8 : []

    , concat $ cs!!9 : cs!!12 : cs!!15 : []
    , concat $ cs!!10 : cs!!13 : cs!!16 : []
    , concat $ cs!!11 : cs!!14 : cs!!17 : []

    , concat $ cs!!18 : cs!!21 : cs!!24 : []
    , concat $ cs!!19 : cs!!22 : cs!!25 : []
    , concat $ cs!!20 : cs!!23 : cs!!26 : []
    ]


getRegionIndex :: Int -> Int
getRegionIndex n = (3 * (n `div` 9)) + n `mod` 3

iterateThroughSudoku :: [[SudokuValue]] -> [[SudokuValue]]
iterateThroughSudoku sudoku =
  let
    horizontal = map processRow
    vertical = transpose . map processRow . transpose
    regional = regionize . map processRow . regionize
    processed = regional $ vertical $ horizontal sudoku
  in
    map (\row -> map (\i -> case i of
      Val x -> Val x
      Pos (x:[]) -> Val x
      Pos x -> Pos x
    ) row) processed

isSolved :: [[SudokuValue]] -> Bool
isSolved sudoku =
  all (== True) $ map (\i -> case i of
      Val _ -> True
      Pos _ -> False
    ) $ concat sudoku

display :: [[SudokuValue]] -> String
display sudoku =
  intercalate "\n" $ map (\row -> concatMap (\i -> case i of
    Val x -> show x
    Pos _ -> "_"
  ) row) sudoku

main :: IO ()
main = do
  let sudoku = processPuzzleInitial testPuzzle
  let it1 = iterateThroughSudoku sudoku
  print it1
  let it2 = iterateThroughSudoku it1
  print it2
  let it3 = iterateThroughSudoku it2
  print it3
  let it4 = iterateThroughSudoku it3
  print it4
  let it5 = iterateThroughSudoku it4
  print it5
  let it6 = iterateThroughSudoku it5
  print it6
  let it7 = iterateThroughSudoku it6
  print it7
  let it8 = iterateThroughSudoku it7
  print it8
  let it9 = iterateThroughSudoku it8
  print it9
  let it10 = iterateThroughSudoku it9
  print it10
  putStrLn $ display it10
  --print $ iterateThroughSudoku sudoku
