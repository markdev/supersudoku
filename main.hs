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
    createSudokuValue val =
      if (val == 0)
        then Pos [1..9]
        else Val val
    processRow row = map createSudokuValue row
  in map processRow values

processRow :: [SudokuValue] -> [SudokuValue]
processRow row =
  let
    getCompleteValue (Val x) = x
    getCompleteValue _ = 0
    completeValues = filter (> 0) $ map getCompleteValue row
    prunePossible completeValues val = case val of
      Val x -> Val x
      Pos xs -> Pos (xs \\ completeValues)
  in
    map (prunePossible completeValues) row

regionize :: [[SudokuValue]] -> [[SudokuValue]]
regionize sudoku =
  let
    cs = concatMap (chunksOf 3) sudoku
    construct (a,b,c) = concat $ cs!!a : cs!!b : cs!!c : []
  in map construct [(0,3,6), (1,4,7), (2,5,8), (9,12,15), (10,13,16), (11,14,17), (18,21,24), (19,22,25), (20,23,26)]

solve :: [[SudokuValue]] -> String
solve sudoku =
  let
    horizontal = map processRow
    vertical = transpose . map processRow . transpose
    regional = regionize . map processRow . regionize
    processed = regional $ vertical $ horizontal sudoku
    detectCertain val = case val of
      Val x -> Val x
      Pos (x:[]) -> Val x
      Pos x -> Pos x
    newSudoku = map (\row -> map detectCertain row) processed
  in
    if isSolved newSudoku
      then display newSudoku
      else solve newSudoku

isSudokuVal :: SudokuValue -> Bool
isSudokuVal (Val _) = True
isSudokuVal _ = False

isSolved :: [[SudokuValue]] -> Bool
isSolved sudoku =
  all isSudokuVal $ concat sudoku

display :: [[SudokuValue]] -> String
display sudoku =
  let
    showVals val = case val of
      Val x -> show x
      Pos _ -> "_"
  in intercalate "\n" $ map (\row -> concatMap showVals row) sudoku

main :: IO ()
main = do
  let sudoku = processPuzzleInitial testPuzzle
  putStrLn $ solve sudoku
