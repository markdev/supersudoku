data SudokuValue = Val Int | Pos [Int] deriving (Show, Eq, Ord)

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

testPuzzleProcessed :: [[SudokuValue]]
testPuzzleProcessed = [
    [Val 5, Val 3, Pos [1,2,3,4,5,6,7,8,9], Pos [1,2,3,4,5,6,7,8,9], Val 7,Pos [1,2,3,4,5,6,7,8,9],Pos [1,2,3,4,5,6,7,8,9], Pos [1,2,3,4,5,6,7,8,9], Pos [1,2,3,4,5,6,7,8,9]]
  -- , [6,0,0,1,9,5,0,0,0]
  -- , [0,9,8,0,0,0,0,6,0]
  -- , [8,0,0,0,6,0,0,0,3]
  -- , [4,0,0,8,0,3,0,0,1]
  -- , [7,0,0,0,2,0,0,0,6]
  -- , [0,6,0,0,0,0,2,8,0]
  -- , [0,0,0,4,1,9,0,0,5]
  -- , [0,0,0,0,8,0,0,7,9]
  ]

-- rowProcess :: [Int] -> [Int]
-- rowProcess ints =
--   let
--     knockouts = filter
--     in expression
--
-- setPossiblities :: [[Int]] -> [[Int]]
-- setPossiblities unprocessedInts =
--   map rowProcess unprocessedInts

main = do
  putStrLn "Sudoku"
