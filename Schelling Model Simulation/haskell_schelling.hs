-- Nida Nur Efe

-- Import necessary libraries
import System.Environment (getArgs)
import qualified Data.Vector.Unboxed as V
import Data.List (find)
import System.Random (mkStdGen, randomRs)

-- Define the types for Index and Grid
type Index = (Int, Int)
type Grid = V.Vector Int

-- Define the similarity threshold for happiness
similarityThreshold :: Float
similarityThreshold = 0.4  -- 40% similarity threshold

-- Define the size of the grid and other parameters
nR, nC, nSteps, neighborhoodSize :: Int 
nR = 200 -- Number of rows
nC = 200 -- Number of columns
nSteps = 20 -- Number of steps to simulate
neighborhoodSize = 5 -- Size of the neighborhood to consider

-- All possible indices in the grid
allIndices :: [Index]
allIndices = [ (i, j) | i <- [0..nR-1], j <- [0..nC-1] ] -- All indices in the grid

-- Convert 1D index to 2D index
fromIndex :: Int -> Index
fromIndex idx = (idx `div` nC, idx `mod` nC) -- Convert by dividing by number of columns for row index and taking modulus for column index

-- Convert 2D index to 1D index
toIndex :: Index -> Int
toIndex (i, j) = i * nC + j -- Convert by multiplying row index by number of columns and adding column index

-- Get the value of a specific index in the grid
get :: Grid -> Index -> Int
get grid idx = grid V.! toIndex idx  -- Get by converting the index to 1D

-- Update the value of a specific index in the grid
set :: Grid -> Index -> Int -> Grid
set grid idx val = grid V.// [(toIndex idx, val)] -- Update by converting the index to 1D

-- Get the neighbors of a given index in the grid and return a list of integers
findNeighbors :: Grid -> Index -> [Int]
findNeighbors grid (x, y) =
  [ get grid (i, j) -- Get the value at the neighbor index
  | i <- [max 0 (x - neighborhoodSize) .. min (nR - 1) (x + neighborhoodSize)] -- Iterate over the rows in the neighborhood
  , j <- [max 0 (y - neighborhoodSize) .. min (nC - 1) (y + neighborhoodSize)] -- Iterate over the columns in the neighborhood
  , (i, j) /= (x, y) -- Exclude the center index
  , get grid (i, j) /= 0 -- Exclude empty positions
  ]

-- Check if a group id is happy at a given position in the grid
isHappy :: Grid -> Index -> Int -> Bool
isHappy grid pos gid =
  let ns = findNeighbors grid pos -- Get the neighbors of the position
      same = length (filter (== gid) ns) -- Count the number of neighbors with the same group id
      total = length ns -- Count the total number of neighbors
  in total == 0 || fromIntegral same / fromIntegral total > similarityThreshold -- Check if the ratio of same group ids to total neighbors is above the threshold
 
-- Find a new position for the group id in the grid
findNewPosition :: Grid -> Int -> Maybe Index
findNewPosition grid gid =
  find (\idx -> get grid idx == 0 && isHappy grid idx gid) allIndices -- Find an empty position in the grid that is happy for the group id

-- Reallocate the group id to a new position in the grid
shuffleEmptyPositions :: Grid -> [Index]
shuffleEmptyPositions grid =
  let empty = [ idx | idx <- allIndices, get grid idx == 0 ] -- Get all empty positions in the grid
      g = mkStdGen 50 -- Create a random generator with a seed, 50 is a random seed 
      order = take (length empty) (randomRs (0, length empty - 1) g) -- Generate random indices for shuffling
  in map (empty !!) order -- Shuffle the empty positions using the random indices

-- Reallocate the group ids in the grid
reallocateTortoise :: Grid -> Grid
reallocateTortoise grid = grid V.// updates -- Update the grid with the new positions
  where
    emptyList = shuffleEmptyPositions grid -- Get the shuffled empty positions
    (_, updates) = foldl tryMove (emptyList, []) allIndices -- Iterate over all indices in the grid

    tryMove ([], acc) _ = ([], acc)
    tryMove (empties, acc) idx = 
      let gid = get grid idx -- Get the group id at the current index
      in if gid == 0 || isHappy grid idx gid -- If the group id empty or happy, continue
        then (empties, acc)
        else case find (\e -> isHappy grid e gid) empties of -- Find a happy empty position for the group id
          Just newPos -> -- If a happy empty position is found
            let remaining = filter (/= newPos) empties -- Remove the new position from the empty positions
            in (remaining, (toIndex idx, 0):(toIndex newPos, gid):acc) -- Update the grid with the new position
          Nothing -> (empties, acc) -- If the group id is not happy and the empty position is not happy, continue

-- Simulate the reallocation process for given number of steps
simulate :: Int -> Grid -> Grid
simulate 0 g = g -- If no steps left, return the grid
simulate n g = simulate (n - 1) (reallocateTortoise g) -- Reallocate the grid for one step

-- Write the grid to a list of strings
writeGrid :: Grid -> [String]
writeGrid g = map show (V.toList g) -- Convert the vector to a list of strings

-- Read the grid from a list of integers
readGrid :: [Int] -> Grid
readGrid xs = V.fromList xs -- Convert the list of integers to a vector

-- Main function to read input and output files
main :: IO ()
main = do
  args <- getArgs -- Get command line arguments
  case args of
    [inputFile, outputFile] -> do -- Check if the correct number of arguments is provided
      contents <- readFile inputFile -- Read the input file
      let values = map read (lines contents) :: [Int] -- Convert the lines to a list of integers
      let grid = readGrid values -- Read the grid from the list of integers
      let final = simulate nSteps grid -- Simulate the reallocation process
      writeFile outputFile (unlines (writeGrid final)) -- Write the final grid to the output file