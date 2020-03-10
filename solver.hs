module Solver (
    generateBoard
  , displayOne
  , generateBoards
  , processLine
  , toBoard
  , solve
  , display
  , Difficulty
) where

import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe (catMaybes)
import Boards
import Control.Lens((.~), element)
import System.Random

data SudokuValue = Val Int | Pos [Int] deriving (Show, Eq, Ord)
data BoardDelta = SudokuBoard Board Delta
data Dimension = RowA | ColA | RegionA
data BoardPossibility = Dead | Active Board | Solved Board
data SolutionClass = NoSolution | Solution Board | MultipleSolutions [Board] deriving (Show, Eq)
data Difficulty = Easy | Medium | Hard | Expert deriving (Show)
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

processLine :: String -> [Int]
processLine str =
  map (\c -> read [c] :: Int) str

generateBoard :: Difficulty -> StdGen -> Board
generateBoard difficulty g =
  let
    board = completeBoard1
    masks = getRandomValues 81 g
    rs = getRandomValues 20 g
    boardNew = rotate board rs
    rootBoard = completeBoard1
  in
    toBoard $ applyMasks masks difficulty $ rotate' rs $ rootBoard

generateBoards :: Difficulty -> [StdGen] -> [Board]
generateBoards difficulty gs =
  map (generateBoard difficulty) gs

generateBoardFile :: Difficulty -> Int -> IO ()
generateBoardFile difficulty n = do
  putStrLn "Enter the file name:\n"
  let gs = [mkStdGen 1]
  let boards = generateBoards difficulty gs
  let content = intercalate "\n\n" $ map toText boards
  writeFile "sudokuSolutions.txt" content
  putStrLn $ content

toText :: Board -> String
toText board =
  let
    showSv sv = case sv of
      Val x -> show x
      Pos _ -> "0"
  in intercalate "\n" (map (\row -> concatMap (\sv -> showSv sv) row) board)

solve :: Board -> SolutionClass
solve board = solve' 1 board

solveMultiple :: [Board] -> [SolutionClass]
solveMultiple boards = map solve boards

display :: SolutionClass -> String
display solutionClass =
  case solutionClass of
    NoSolution -> "There are no solutions \n"
    Solution x -> "There is one solution \n\n" ++ displayOne x ++ "\n\n"
    MultipleSolutions xs -> "There are multiple solutions \n\n" ++ (intercalate "\n===========\n\n" $ map displayOne xs)

displayOne :: Board -> String
displayOne sudoku =
  let
    showVals val = case val of
      Val x -> show x
      Pos _ -> "_"
    addHorizontalLine = intercalate ["-----------"] . chunksOf 3
    addVerticalLine = intercalate ["|"] . chunksOf 3
  in intercalate "\n"
    $ addHorizontalLine
    $ map (\row -> concat
      $ addVerticalLine
      $ map showVals row)
      sudoku

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

tp2 :: Board
tp2 = nextIteration $ toBoard testPuzzle2

tp56 = createMaskedBoard completeBoard1 56
tp57 = createMaskedBoard completeBoard1 57

solve' :: Delta -> Board -> SolutionClass
solve' delta board =
  let
    (rotate, newDelta) = nextIterationWDelta board
  in if isSolved board then Solution board
    else if not (isValidBoard board) then NoSolution
      else if delta > 0 then solve' newDelta rotate
        else pickSolutions $ map (solve' 1) (splitSmallest board (allPossiblities board))

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
    (rotate, newDelta) = nextIterationWDelta board
    boards = (splitSmallest board (allPossiblities board))
    solveSplitBoards = map (solveWithDepth 1 (depth+1)) boards
  in if isSolved board then (Solution board, depth)
    else if not (isValidBoard board) then (NoSolution, depth)
      else if delta > 0 then (solveWithDepth newDelta depth rotate)
        else (pickSolutionsWithDepth solveSplitBoards)

solveWithDepthWrapper :: Board  -> (SolutionClass, Depth)
solveWithDepthWrapper board = solveWithDepth 1 0 board

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

toBoard :: [[Int]] -> Board
toBoard values =
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
    rotate = map (\row -> map detectCertain row) $ cleanPossibilities board
    oldSolvedCells = numberOfSolvedCells board
    newSolvedCells = numberOfSolvedCells rotate
    delta = newSolvedCells - oldSolvedCells
  in (rotate, delta)

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

iterize :: Board -> Delta -> IO String
iterize sudoku prevSolved = do
  if (isSolved sudoku)
    then
      return $ displayOne sudoku
    else do
      let nextSudoku = nextIteration sudoku
      let newSolved = numberOfSolvedCells nextSudoku
      if prevSolved == newSolved
        then do
          putStrLn "This puzzle is not solvable without branching"
          return "FAILURE"
        else do
          putStrLn "\n"
          putStrLn $ displayOne sudoku
          putStrLn $ show newSolved
          putStrLn "\n"
          iterize nextSudoku newSolved

showAllPossiblities :: Board -> IO ()
showAllPossiblities board =
  mapM_ putStr $
    intersperse "===========\n" $
    map displayOne $ allNewBoards board

testRand :: Int -> IO [Int]
testRand n = sequence $ take n $ repeat $ randomRIO (1,9::Int)

getRandomValues :: Int -> StdGen -> [[Int]]
getRandomValues n g =
  take n $
  nub $
  filter (\(a:b:_) -> a /= b) $
  chunksOf 2 $
  ((randomRs (1, 9) g) :: [Int])

createMaskedBoard :: [[Int]] -> Int -> Board
createMaskedBoard board numberOfMasks =
  let
    g = mkStdGen 1
    masks = getRandomValues numberOfMasks g
  in toBoard $ mask board masks


generateSudokuWrapper :: Difficulty -> StdGen -> [[Int]]
generateSudokuWrapper difficulty g =
  let
    board = completeBoard1
    rs = getRandomValues 20 g
    boardNew = rotate board rs
    masks = getRandomValues 70 g
  in
    applyMasks masks difficulty board

applyMasks :: [[Int]] -> Difficulty -> [[Int]] -> [[Int]]
applyMasks masks difficulty board =
  let
    difficultyThreshold = case difficulty of
      Easy -> 0
      Medium -> 1
      Hard -> 3
      Expert -> 6
    boardNew = mask board [head masks]
    (sc, d) = solveWithDepthWrapper $ toBoard boardNew
  in
    if (d > difficultyThreshold)
      then board
      else applyMasks (tail masks) difficulty boardNew

mask :: [[Int]] -> [[Int]] -> [[Int]]
mask board masks = foldl (\acc (a:b:_) -> maskBoardOnce a b acc) board masks

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
    case solve (toBoard newBoard) of
      NoSolution -> board
      Solution _ -> newBoard
      MultipleSolutions _ -> board

rotate' :: [[Int]] -> [[Int]] -> [[Int]]
rotate' coords board =
  foldl (\acc (a:b:_) -> rotateRows acc a b) board coords

rotate :: [[Int]] -> [[Int]] -> [[Int]]
rotate board coords =
  foldl (\acc (a:b:_) -> rotateRows acc a b) board coords
