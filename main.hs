import Data.List
import Data.List.Split
import Boards
import System.Random
import System.IO
import Solver

genBoard :: Difficulty -> IO ()
genBoard difficulty = do
  g <- newStdGen
  let board = generateBoard difficulty g
  putStrLn $ displayOne board

genBoards :: Difficulty -> Int -> IO ()
genBoards difficulty n = do
  gs <- sequence $ replicate 5 newStdGen
  let boards = generateBoards difficulty gs
  putStrLn $ intercalate "\n\n============\n\n"
    $ map displayOne boards

solveOneBoard :: IO ()
solveOneBoard = do
  putStrLn "Enter row 1:"
  r1 <- processLine <$> getLine
  putStrLn "Enter row 2:"
  r2 <- processLine <$> getLine
  putStrLn "Enter row 3:"
  r3 <- processLine <$> getLine
  putStrLn "Enter row 4:"
  r4 <- processLine <$> getLine
  putStrLn "Enter row 5:"
  r5 <- processLine <$> getLine
  putStrLn "Enter row 6:"
  r6 <- processLine <$> getLine
  putStrLn "Enter row 7:"
  r7 <- processLine <$> getLine
  putStrLn "Enter row 8:"
  r8 <- processLine <$> getLine
  putStrLn "Enter row 9:"
  r9 <- processLine <$> getLine
  let board = toBoard [r1,r2,r3,r4,r5,r6,r7,r8,r9]
  let solved = solve board
  putStrLn $ display solved

solveMultipleBoards :: IO ()
solveMultipleBoards = do
  putStrLn "Enter the file name:\n"
  fileName <- getLine
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  let splitBoards = splitWhen (== "") $ lines contents
  let sudokus = (map . map . map) (read . pure) splitBoards :: [[[Int]]]
  let boards = map toBoard sudokus
  let solutions = map solve boards
  putStrLn $ unlines $ map display solutions
  hClose handle
