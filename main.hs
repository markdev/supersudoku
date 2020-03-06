import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe (catMaybes)
import Boards
import Control.Lens((.~), element)

data SudokuValue = Val Int | Pos [Int] deriving (Show, Eq, Ord)
type Row = [SudokuValue]
type Column = [SudokuValue]
type Region = [SudokuValue]
type Board = [Row]
type RowIndex = Int
type ColIndex = Int
type PossibleValues = [Int]
type PossibilityTuple = (RowIndex, ColIndex, PossibleValues)
type Delta = Int
data BoardDelta = SudokuBoard Board Delta
data Dimension = RowA | ColA | RegionA
data BoardPossibility = Dead | Active Board | Solved Board
data SolutionClass = NoSolution | Solution Board | MultipleSolutions [Board] deriving (Show, Eq)
--data Range = NoRange | Range (Int Int [Int])

tp2 :: Board
tp2 = nextIteration $ process testPuzzle2


solve2 :: Delta -> Board -> SolutionClass
solve2 delta board =
  let
    (newBoard, newDelta) = nextIterationWDelta board
  in if isSolved board then Solution board
    else if not (isValidBoard board) then NoSolution
      else if delta > 0 then solve2 newDelta newBoard
        else pickSolutions $ map (solve2 1) (splitFirst board (allPossiblities board))
-- extract pickSolutions, addConcatMap!!!!

leSolve :: Board -> SolutionClass
leSolve board = solve2 1 board

--splitFirst tp2 $ allPossiblities tp2



pickSolutions :: [SolutionClass] -> SolutionClass
pickSolutions sclist
  | length uniqueSolutions == 0 = NoSolution
  | length uniqueSolutions == 1 = head allSingleSolutions
  | length uniqueSolutions > 1 = MultipleSolutions $ map extractSolution uniqueSolutions
  where
    uniqueSolutions = nub allSingleSolutions
    allSingleSolutions = filter isSolution sclist
    extractSolution (Solution x) = x
    isSolution scListItem =
      case scListItem of
        NoSolution -> False
        Solution _ -> True
        MultipleSolutions _ -> False

nextIterationWDelta :: Board -> (Board, Delta)
nextIterationWDelta sudoku =
  let
    horizontal = map processRow
    vertical = transpose . map processRow . transpose
    regional = regionize . map processRow . regionize
    processed = regional $ vertical $ horizontal sudoku
    detectCertain val = case val of
      Val x -> Val x
      Pos (x:[]) -> Val x
      Pos x -> Pos x
    newBoard = map (\row -> map detectCertain row) processed
    oldSolvedCells = numberOfSolvedCells sudoku
    newSolvedCells = numberOfSolvedCells newBoard
    delta = newSolvedCells - oldSolvedCells
  in (newBoard, delta)

{-
  Step 1: Establish polymorphic data value
-}

{-
  TODO
  - knight's quest http://learnyouahaskell.com/a-fistful-of-monads
  - createHypotheticalBoard -> board -> row -> col -> val -> board
  - getSmallestRange -> board -> (row, col, [val])
  - showNextIteration Board -> IO String (shows original board and next board)
  - transformBoard -> Board -> Dimension -> Value -> Value -> Board
-}

-- getSmallestRange :: Board -> Range
-- getSmallestRange board =
-- getAllPossiblities :: Board -> [(Int, Int, [Int])]
-- getAllPossiblities board =
--   let
--     getPos = case sudVal of
--       Val _ -> []
--       Pos x -> x
--     filter (/= []) $ map getPos $ concat board
--   in expression

process :: [[Int]] -> Board
process values =
  let
    createSudokuValue val =
      if (val == 0)
        then Pos [1..9]
        else Val val
    processRow row = map createSudokuValue row
  in map processRow values

nextIteration :: Board -> Board
nextIteration sudoku =
  let
    horizontal = map processRow
    vertical = transpose . map processRow . transpose
    regional = regionize . map processRow . regionize
    processed = regional $ vertical $ horizontal sudoku
    detectCertain val = case val of
      Val x -> Val x
      Pos (x:[]) -> Val x
      Pos x -> Pos x
  in map (\row -> map detectCertain row) processed

regionize :: Board -> Board
regionize sudoku =
  let
    cs = concatMap (chunksOf 3) sudoku
    construct (a,b,c) = concat $ cs!!a : cs!!b : cs!!c : []
  in map construct [(0,3,6), (1,4,7), (2,5,8), (9,12,15), (10,13,16), (11,14,17), (18,21,24), (19,22,25), (20,23,26)]

processRow :: Row -> Row
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

-- getNewValue :: Int -> Int -> Board -> SudokuValue
-- getNewValue rowIndex colIndex board =
--   let
--     possibilities = getPossibilities rowIndex colIndex board
--   in case board !! rowIndex !! colIndex of
--     Val x -> Val x
--     Pos _ -> case possibilities of
--       (x:[]) -> Val x
--       _ -> Pos possibilities
--
-- getNewRow :: Int -> Board -> Row
-- getNewRow rowIndex board =
--   let
--     row = board !! rowIndex
--     getNewRow' 9 newRow = newRow
--     getNewRow' n newRow =
--       getNewRow' (n+1) (newRow ++ [getNewValue rowIndex n board])
--   in getNewRow' 0 []
--
-- getNewBoard :: Board -> Board
-- getNewBoard board =
--   let
--     getNewBoard' 9 newBoard = newBoard
--     getNewBoard' n newBoard =
--       getNewBoard' (n+1) (newBoard ++ [getNewRow n board])
--   in getNewBoard' 0 []

getPossibilities :: RowIndex -> ColIndex -> Board -> Maybe PossibilityTuple
getPossibilities rowIndex colIndex board =
  let
    row = board !! rowIndex
    col = (transpose board) !! colIndex
    reg = concat ((chunksOf 3 (transpose $ (chunksOf 3 board) !! (rowIndex `div` 3))) !! (colIndex `div` 3))
    getPossibilities series = [1..9] \\ (filter (> 0) $ map getValue series)
    intersections =
      getPossibilities row `intersect`
      getPossibilities col `intersect`
      getPossibilities reg
  in
    case board !! rowIndex !! colIndex of
      Val _ -> Nothing
      Pos _ -> Just (rowIndex, colIndex, intersections)

allPossiblities :: Board -> [PossibilityTuple]
allPossiblities board =
  let coords = [ (r,c) | r <- [0..8], c <- [0..8]]
  in catMaybes $ map (\(r,c) -> getPossibilities r c board) coords

splitFirst :: Board -> [PossibilityTuple] -> [Board]
splitFirst board (x:xs) = createNewBoards board x
splitFirst board _ = [board]

createNewBoards :: Board -> PossibilityTuple -> [Board]
createNewBoards board (r,c,ps) =
  let
    prevRows = take (r) board
    lastRows = drop (r+1) board
    theRow = board !! r
    newValue = Val 3
    newRow newValue = (take (c) theRow) ++ [newValue] ++ (drop (c+1) theRow)
    createNewBoard value = prevRows ++ [newRow (Val value)] ++ lastRows
  in
    map createNewBoard ps

allNewBoards board = concatMap (createNewBoards board) (allPossiblities board)

getValue :: SudokuValue -> Int
getValue sudVal = case sudVal of
  Val x -> x
  _ -> 0

isValidRow :: Int -> Board -> Bool
isValidRow rowIndex board =
  let
    row = board !! rowIndex
    values = filter (> 0) $ map getValue row
  in values == nub values

isValidRow' :: Row -> Bool
isValidRow' row =
  let
    values = filter (> 0) $ map getValue row
  in values == nub values

isValidCol :: Int -> Board -> Bool
isValidCol colIndex board =
  let
    col = (transpose board) !! colIndex
    values = filter (> 0) $ map getValue col
  in values == nub values

isValidCol' :: Column -> Bool
isValidCol' column =
  let
    values = filter (> 0) $ map getValue column
  in values == nub values

isValidReg :: Int -> Int -> Board -> Bool
isValidReg rowIndex colIndex board =
  let
    reg = concat ((chunksOf 3 (transpose $ (chunksOf 3 board) !! (rowIndex `div` 3))) !! (colIndex `div` 3))
    values = filter (> 0) $ map getValue reg
  in values == nub values

isValidReg' :: Region -> Bool
isValidReg' region =
  let
    values = filter (> 0) $ map getValue region
  in values == nub values

isValidBoard :: Board -> Bool
isValidBoard board =
  let
    rowValidity = map isValidRow' board
    colValidity = map isValidCol' (transpose board)
    regionValidity = map isValidReg' (regionize board)
  in
    all (== True) $ (rowValidity ++ colValidity ++ regionValidity)

numberOfSolvedCells :: Board -> Delta
numberOfSolvedCells board =
  sum $ map (\ cell -> case cell of
    Val _ -> 1
    _ -> 0
    ) $ concat board

isSudokuVal :: SudokuValue -> Bool
isSudokuVal (Val _) = True
isSudokuVal _ = False

isSolved :: Board -> Bool
isSolved sudoku =
  all isSudokuVal $ concat sudoku

display :: Board -> String
display sudoku =
  let
    showVals val = case val of
      Val x -> show x
      Pos _ -> "_"
    addHorizontalLine = intercalate ["-----------"] . chunksOf 3
    addVerticalLine = intercalate ["|"] . chunksOf 3
  in unlines $ lines $ intercalate "\n"
    $ addHorizontalLine
    $ map (\row -> concat
      $ addVerticalLine
      $ map showVals row)
      sudoku

iterize :: Board -> Delta -> IO String
iterize sudoku prevSolved = do
  if (isSolved sudoku)
    then
      return $ display sudoku
    else do
      let nextSudoku = nextIteration sudoku
      let newSolved = numberOfSolvedCells nextSudoku
      if prevSolved == newSolved
        then do
          putStrLn "This puzzle is not solvable without branching"
          return "FAILURE"
        else do
          putStrLn "\n"
          putStrLn $ display sudoku
          putStrLn $ show newSolved
          putStrLn "\n"
          iterize nextSudoku newSolved

showAllPossiblities :: Board -> IO ()
showAllPossiblities board =
  mapM_ putStr $
    intersperse "===========\n" $
    map display $ allNewBoards board

solve :: Board -> String
solve sudoku =
  let
    newSudoku = nextIteration sudoku
  in
    if isSolved newSudoku
      then display newSudoku
      else solve newSudoku

main :: IO ()
main = do
  let sudoku = process testPuzzle2
  completed <- iterize sudoku 0
  putStrLn "COMPLETED:"
  putStrLn "\n"
  putStrLn completed
