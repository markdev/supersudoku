import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe (catMaybes)
import Boards
import Control.Lens((.~), element)
import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime)

data SudokuValue = Val Int | Pos [Int] deriving (Show, Eq, Ord)
data BoardDelta = SudokuBoard Board Delta
data Dimension = RowA | ColA | RegionA
data BoardPossibility = Dead | Active Board | Solved Board
data SolutionClass = NoSolution | Solution Board | MultipleSolutions [Board] deriving (Show, Eq)
data Difficulty = Easy | Medium | Hard | Expert
type Row = [SudokuValue]
type Column = [SudokuValue]
type Region = [SudokuValue]
type Board = [Row]
type RowIndex = Int
type ColIndex = Int
type PossibleValues = [Int]
type PossibilityTuple = (RowIndex, ColIndex, PossibleValues)
type Delta = Int
type Depth = Int

tp2 :: Board
tp2 = nextIteration $ process testPuzzle2

solve' :: Delta -> Board -> SolutionClass
solve' delta board =
  let
    (newBoard, newDelta) = nextIterationWDelta board
  in if isSolved board then Solution board
    else if not (isValidBoard board) then NoSolution
      else if delta > 0 then solve' newDelta newBoard
        else pickSolutions $ map (solve' 1) (splitSmallest board (allPossiblities board))

solve :: Board -> SolutionClass
solve board = solve' 1 board

pickSolutionsWithDepth :: [(SolutionClass, Depth)] -> (SolutionClass, Depth)
pickSolutionsWithDepth sclist
  | length uniqueSolutions == 0 = (NoSolution, 0)
  | length uniqueSolutions == 1 = head allSingleSolutions2
  | length uniqueSolutions > 1 = (MultipleSolutions (map extractSolution uniqueSolutions), 0)
  where
    uniqueSolutions = nub allSingleSolutions
    allSingleSolutions = map (\(sc, _) -> sc) $ filter isSolution sclist
    allSingleSolutions2 = filter isSolution sclist
    extractSolution (Solution x) = x
    isSolution (sol, _) = case sol of
        NoSolution -> False
        Solution _ -> True
        MultipleSolutions _ -> False

solveWithDepth :: Delta -> Depth -> Board -> (SolutionClass, Depth)
solveWithDepth delta depth board =
  let
    (newBoard, newDelta) = nextIterationWDelta board
    boards = (splitSmallest board (allPossiblities board))
    solveSplitBoards = map (solveWithDepth 1 (depth+1)) boards
  in if isSolved board then (Solution board, depth)
    else if not (isValidBoard board) then (NoSolution, depth)
      else if delta > 0 then (solveWithDepth newDelta depth newBoard)
        else (pickSolutionsWithDepth solveSplitBoards)

solveWithDepthWrapper :: Board  -> (SolutionClass, Depth)
solveWithDepthWrapper board = solveWithDepth 1 0 board



----------------------- Begin Dead Code

-- solveWithDepthIO :: Delta -> Depth -> Board -> IO (SolutionClass, Depth)
-- solveWithDepthIO delta depth board = do
--   let (newBoard, newDelta) = nextIterationWDelta board
--   let boards = (splitSmallest board (allPossiblities board))
--   let solveSplitBoards = map (solveWithDepthIO 1 (depth+1)) boards
--   if isSolved board
--     then do
--       putStrLn "It is solved!"
--       print board
--       putStrLn $ "Depth: " ++ (show depth)
--       return (Solution board, depth)
--     else do
--       if not (isValidBoard board)
--         then do
--           return (NoSolution, depth)
--         else do
--           if (delta > 0)
--             then do
--               putStrLn "Going to the next iteration"
--               print newBoard
--               putStrLn $ "Depth: " ++ (show depth)
--               return (solveWithDepthIO newDelta depth newBoard)
--             else do
--               putStrLn "Time to split the boards"
--               print boards
--               return (pickSolutionsWithDepth solveSplitBoards)
--
-- solveWithDepthIOWrapper :: Board -> IO (SolutionClass, Depth)
-- solveWithDepthIOWrapper board = do
--   solveWithDepthIO 1 0 board

  -------------------- End Dead Code




splitFirst :: Board -> [PossibilityTuple] -> [Board]
splitFirst board (x:xs) = createNewBoards board x
splitFirst board _ = [board]

splitSmallest :: Board -> [PossibilityTuple] -> [Board]
splitSmallest board possibilities@(x:xs) =
  let
    pos = head $ sortBy (compare `on` (length . (\(_,_,c) -> c))) $ possibilities
  in createNewBoards board pos
splitSmallest board _ = [board]

pruneHorizontal :: Board -> Board
pruneHorizontal board = map processRow board

pruneVertical :: Board -> Board
pruneVertical board = (transpose . map processRow . transpose) board

pruneRegional :: Board -> Board
pruneRegional board = (regionize . map processRow . regionize) board

cleanPossibilities :: Board -> Board
cleanPossibilities board =
  pruneRegional $
  pruneVertical $
  pruneHorizontal board

process :: [[Int]] -> Board
process values =
  let
    createSudokuValue val =
      if (val == 0)
        then Pos [1..9]
        else Val val
    processRow row = map createSudokuValue row
  in cleanPossibilities $ map processRow values

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

detectCertain :: SudokuValue -> SudokuValue
detectCertain val = case val of
  Val x -> Val x
  Pos (x:[]) -> Val x
  Pos x -> Pos x

nextIterationWDelta :: Board -> (Board, Delta)
nextIterationWDelta board =
  let
    newBoard = map (\row -> map detectCertain row) $ cleanPossibilities board
    oldSolvedCells = numberOfSolvedCells board
    newSolvedCells = numberOfSolvedCells newBoard
    delta = newSolvedCells - oldSolvedCells
  in (newBoard, delta)

nextIteration :: Board -> Board
nextIteration board =
  map (\row -> map detectCertain row) $ cleanPossibilities board

regionize :: Board -> Board
regionize board =
  let
    cs = concatMap (chunksOf 3) board
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

displaySolution :: SolutionClass -> String
displaySolution solutionClass =
  case solutionClass of
    NoSolution -> "There are no solutions \n"
    Solution x -> "There is one solution \n\n" ++ display x
    MultipleSolutions xs -> "There are multiple solutions \n\n" ++ (intercalate "\n===========\n\n" $ map display xs)

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


createRotation ::  Int -> Int -> [Int] -> [Int]
createRotation a b row =
  let
    createRotation' a b [] ret = ret
    createRotation' a b row@(x:xs) ret
      | x == a = createRotation' a b (tail row) (ret ++ [b])
      | x == b = createRotation' a b (tail row) (ret ++ [a])
      | otherwise = createRotation' a b (tail row) (ret ++ [x])
  in createRotation' a b row []

rotateRows :: [[Int]] -> Int -> Int -> [[Int]]
rotateRows rawBoard a b =
  map (createRotation a b) rawBoard

testRand :: Int -> IO [Int]
testRand n = sequence $ take n $ repeat $ randomRIO (1,9::Int)

getNums :: Int -> [[Int]]
getNums n =
  let
    g = mkStdGen 1
  in take n $
    nub $
    filter (\(a:b:_) -> a /= b) $
    chunksOf 2 $
    ((randomRs (1, 9) g) :: [Int])

newBoard :: [[Int]] -> [[Int]] -> [[Int]]
newBoard board nums =
  foldl (\acc (a:b:_) -> rotateRows acc a b) board nums

maskBoardOnce :: RowIndex -> ColIndex -> [[Int]] -> [[Int]]
maskBoardOnce ri ci board =
  let
    prevRows = take (ri - 1) board
    lastRows = drop (ri) board
    prevCols = take (ci - 1) $ board !! (ri - 1)
    lastCols = drop (ci) $ board !! (ri - 1)
    changedRow = prevCols ++ [0] ++ lastCols
    newBoard = prevRows ++ [changedRow] ++ lastRows
  in
    case solve (process newBoard) of
      NoSolution -> board
      Solution _ -> newBoard
      MultipleSolutions _ -> board

maskBoard :: [[Int]] -> [[Int]] -> [[Int]]
maskBoard board masks =
  foldl (\acc (a:b:_) -> maskBoardOnce a b acc) board masks


generateSudoku :: [[Int]] -> Difficulty -> Depth -> [[Int]] -> [[Int]]
generateSudoku board difficulty depth masks =
  let
    difficultyThreshold = case difficulty of
      Easy -> 0
      Medium -> 1
      Hard -> 3
      Expert -> 6
    boardNew = maskBoard board [head masks]
    (sc, d) = solveWithDepthWrapper $ process boardNew
  in
    if (d > difficultyThreshold)
      then board
      else generateSudoku boardNew difficulty depth (tail masks)


generateSudokuWrapper :: Difficulty -> [[Int]]
generateSudokuWrapper difficulty =
  let
    board = completeBoard1
    rs = getNums 20
    boardNew = newBoard board rs
    masks = getNums 70
  in
    generateSudoku board difficulty 0 masks


main :: IO ()
main = do
  let sudoku = process multiSolutions
  let completed = solve sudoku
  let g = mkStdGen 1
  print g
  let rs = getNums 20
  let masks = getNums 72
  print rs
  putStrLn "COMPLETED:"
  putStrLn "\n"
  putStrLn $ displaySolution completed
  putStrLn " HERE IS AN EXISTING BOARD "
  let board = completeBoard1
  putStrLn $ show board
  putStrLn " NOW TO GENERATE A NEW BOARD "
  let boardNew = newBoard board rs
  putStrLn $ show boardNew
  putStrLn " NOW WE WILL MASK THE BOARD "
  if masks /= []
    then do
      addMaskAndDisplay (length masks) boardNew masks
    else do
      putStrLn "nope"
  -- let maskedBoard = maskBoard board masks
  -- putStrLn $ show maskedBoard
  -- putStrLn " NOW IT BECOMES THE BOARD "
  -- let maskedBoard = maskBoard board masks
  -- print $ process maskedBoard
addMaskAndDisplay :: Int -> [[Int]] -> [[Int]] -> IO ()
addMaskAndDisplay originalMasks board masks = do
  if masks /= []
    then do
      let boardNew = maskBoard board [head masks]
      putStrLn "Here we goooooo"
      putStrLn $ show $ originalMasks - (length masks)
      putStrLn $ show boardNew
      let pb = process boardNew
      putStrLn $ show $ solveWithDepthWrapper pb
      addMaskAndDisplay originalMasks boardNew (tail masks)
    else do
      putStrLn "nope"
