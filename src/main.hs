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
  board <- solveOneBoard' []
  let solved = solve $ toBoard board
  putStrLn $ display solved

solveOneBoard' :: [[Int]] -> IO ([[Int]])
solveOneBoard' strings = do
  if length strings >= 9
    then do
      return strings
    else do
      putStrLn $ "Enter row #" ++ (show (length strings))
      r <- map (\c -> read [c] :: Int) <$> getLine
      if (length r == 9)
        then do
          solveOneBoard' (strings ++ [r])
        else do
          putStrLn "That is not a valid board"
          solveOneBoard' strings

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
