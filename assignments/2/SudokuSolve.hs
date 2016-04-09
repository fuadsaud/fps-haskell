module SudokuSolve where

import Solver
import Data.Char (intToDigit)
import Data.List (nub,transpose,elemIndex,sort,union,(\\))

data SudokuConfig = SudokuConfig [Int] deriving Eq

instance Show SudokuConfig where
    show (SudokuConfig grid) = concat ("\n":[ row ++ "\n" | row <- rows ])
      where
        rows = [ formattedRow row | row <- partition 9 cells ]
        formattedRow r = concat [ (concat triple) ++ " " | triple <- partition 3 r ]
        cells = [ (representation cell):" " | cell <- grid ]
        representation cell | cell == 0 = '_'
                            | otherwise = intToDigit cell

instance Config SudokuConfig where
    successors config@(SudokuConfig grid) =
      case nextBlankCell config of
        Nothing    -> []
        Just index -> fillCell index
      where
        fillCell index =
          [
            let
              (ini, rest) = splitAt index grid
              newConfig = SudokuConfig (ini ++ (newValue:(tail rest)))
            in
              newConfig | newValue <- validValuesForCell config index ]

sudokuConfigFromList :: Integral a => [a] -> SudokuConfig
sudokuConfigFromList grid =
    if validConfig config
      then config
      else error "invalid board"
  where
    config = SudokuConfig $ map fromIntegral grid

listFromSudokuConfig :: SudokuConfig -> [Int]
listFromSudokuConfig (SudokuConfig grid) = grid

sudokuSolve :: SudokuConfig -> (Maybe SudokuConfig)
sudokuSolve = solve isSudokuGoal

isSudokuGoal :: SudokuConfig -> Bool
isSudokuGoal config = noBlanks && rowsSatisfy && columnsSatisfy && boxesSatisfy
  where
    noBlanks       = nextBlankCell config == Nothing
    rowsSatisfy    = and . map nonupleIsGoal . allRows    $ config
    columnsSatisfy = and . map nonupleIsGoal . allColumns $ config
    boxesSatisfy   = and . map nonupleIsGoal . allBoxes   $ config

nonupleIsGoal :: [Int] -> Bool
nonupleIsGoal xs
              | length xs == 9 = sort xs == [1..9]

validValuesForCell :: SudokuConfig -> Int -> [Int]
validValuesForCell config index = possibleValues \\ invalidValues
  where
    row = indexRow config index
    col = indexColumn config index
    box = indexBox config index
    possibleValues = [1..9]
    invalidValues  = row `union` col `union` box

validNonuple :: [Int] -> Bool
validNonuple xs | length xs == 9 = length (nub nonBlankCells) == length nonBlankCells
  where
    nonBlankCells = (filter (/= 0) xs)

validConfig :: SudokuConfig -> Bool
validConfig config = all validNonuple $ concat [allRows config, allColumns config, allBoxes config]

indexRow ::SudokuConfig -> Int -> [Int]
indexRow config i | i >= 0 && i < 9 * 9 = row config (i `div` 9)
  where
    row (SudokuConfig grid) r | r >= 0 && r <= 9 = take 9 (drop (r * 9) grid)

indexColumn :: SudokuConfig -> Int -> [Int]
indexColumn config i | i >= 0 && i <= 9 * 9 = column config (i `rem` 9)
  where
    column config c | c >= 0 && c <= 9 = (allColumns config) !! c

indexBox :: SudokuConfig -> Int -> [Int]
indexBox (SudokuConfig grid) i | i >= 0 && i <= 9 * 9 = box (point i)
  where
    box (i, j) = [ grid !! (index (ii + offset i, jj + offset j)) | jj <- [0..2], ii <- [0..2] ]
    offset idx = 3 * (idx `div` 3)
    point index = (index - 9 * (index `div` 9), index `div` 9)
    index (i, j) = i + j * 9

allRows :: SudokuConfig -> [[Int]]
allRows (SudokuConfig grid) = partition 9 grid

allColumns :: SudokuConfig -> [[Int]]
allColumns = (transpose . allRows)

allBoxes :: SudokuConfig -> [[Int]]
allBoxes = map concat . concatMap makeTriples . transpose . map makeTriples . allRows
  where
    makeTriples = partition 3

nextBlankCell :: SudokuConfig -> (Maybe Int)
nextBlankCell (SudokuConfig grid) = elemIndex 0 grid

partition :: Int -> [a] -> [[a]]
partition 0 xs = []
partition n [] = []
partition n xs = y:(partition n ys)
  where (y, ys) = splitAt n xs
