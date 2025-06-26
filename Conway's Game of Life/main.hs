-- Import necessary libraries
import Codec.Picture
import System.Directory (createDirectoryIfMissing)

-- Define the grid size
gridSize :: Int
gridSize = 50

-- Define the cell size 
cellSize :: Int
cellSize = 10

-- Define the image size
imageSize :: Int
imageSize = gridSize * cellSize

-- Define the color for each state
colorFor :: Int -> PixelRGB8
colorFor 0 = PixelRGB8 255 255 255  -- white
colorFor 1 = PixelRGB8 0 0 255      -- blue
colorFor _ = PixelRGB8 0 0 0        -- error!

-- Blinker (Period 2)
pattern1 :: Int -> [[Int]]
pattern1 _ = placePattern (center 3 1) -- 
  [ [1,1,1] ] -- Horizontal line of 3 living cells

-- Toad (Period 2)
pattern2 :: Int -> [[Int]]
pattern2 _ = placePattern (center 4 2)
  [ [0,1,1,1], -- Top row: 3 live cells with 1 leading dead cell
    [1,1,1,0] ] -- Bottom row: 3 live cells with 1 trailing dead cell

-- Beacon (Period 2)
pattern3 :: Int -> [[Int]]
pattern3 _ = placePattern (center 4 4)
  [ [1,1,0,0], -- top left
    [1,1,0,0], -- bottom left
    [0,0,1,1], -- top right
    [0,0,1,1] ] -- bottom right

-- Pulsar (Period 3)
pattern4 :: Int -> [[Int]]
pattern4 _ = placePattern (center 13 13)
  [ [0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0], -- top horizontal arms
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], -- empty line
      [1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1], -- 
      [1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1], -- 
      [1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1], -- 
      [0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0], -- 
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], -- empty line
      [0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0], -- 
      [1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1], -- 
      [1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1], -- 
      [1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1], -- 
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], -- empty line
      [0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0] -- 
    ]

-- Pentadecathlon (Period 15)
pattern5 :: Int -> [[Int]]
pattern5 _ = placePattern (center 10 3)
  [ [0, 0, 1, 0, 0, 0, 0, 1, 0, 0],  -- outer ends of the pattern
      [1, 1, 0, 1, 1, 1, 1, 0, 1, 1],  -- central alternating cells 
      [0, 0, 1, 0, 0, 0, 0, 1, 0, 0] -- same as the first row
    ]

-- Function to compute the next generation of the grid
nextGen :: [[Int]] -> [[Int]]
nextGen grid = 
  [ [ cellNextState x y | x <- [0..gridSize-1] ]  --  Iterate over each cell
  | y <- [0..gridSize-1] ] -- Generate the next state of the grid
  where
    cellNextState x y = -- Determine the next state of a cell
      let alive = grid !! y !! x -- Get the current state of the cell
          n = liveNeighbors x y -- Count the number of live neighbors
      in case (alive, n) of  
           (_, 3) -> 1 -- Cell becomes alive with exactly 3 neighbors
           (1, 2) -> 1 -- Cell stays alive with 2 neighbors
           _      -> 0 --  -- Cell dies or remains dead otherwise

    liveNeighbors x y = length --  Count the number of live neighbors
      [ () | dx <- [-1..1], dy <- [-1..1], 
             (dx, dy) /= (0,0), --   -- Skip the cell itself
             let nx = x + dx,  --   Calculate neighbor coordinates, x direction
             let ny = y + dy, -- Calculate neighbor coordinates,  y direction
             inBounds nx ny, -- Check if the neighbor is within bounds
             grid !! ny !! nx == 1 ]  --  -- Check if the neighbor is alive

    inBounds x y = x >= 0 && x < gridSize && y >= 0 && y < gridSize -- Ensure the coordinates are within the grid bounds


-- Convert a matrix of integers to an image    
matrixToImage :: [[Int]] -> Image PixelRGB8
matrixToImage mat = generateImage pixelAt imageSize imageSize
  where
    pixelAt x y =
      let row = y `div` cellSize
          col = x `div` cellSize
          val = (mat !! row) !! col
      in colorFor val


-- This function places a small pattern on a larger grid at a specified position.
placePattern :: (Int, Int) -> [[Int]] -> [[Int]]
placePattern (x0, y0) small =
  [ [ if x >= x0 && x < x0 + w && y >= y0 && y < y0 + h -- If the cell is within the bounds of the small pattern
        then (small !! (y - y0)) !! (x - x0) -- Get the value from the small pattern
        else 0 --  Otherwise, return 0 (dead cell)
    | x <- [0..gridSize-1] ] -- Iterate over columns of the grid
  | y <- [0..gridSize-1] ] -- Iterate over rows of the grid
  where
    h = length small -- Height of the small pattern
    w = length (head small) --  Width of the small pattern


-- This function centers a pattern of width w and height h in the grid.
center :: Int -> Int -> (Int, Int)
center w h = ((gridSize - w) `div` 2, (gridSize - h) `div` 2)


-- Main function to create directories and save images
main :: IO ()
main = do
  let patternFuncs = zip [1..]
        [ pattern1
        , pattern2
        , pattern3
        , pattern4
        , pattern5
        ]

  mapM_
    (\(i, patFn) -> do
        let dirName = "frames" ++ show i
        createDirectoryIfMissing True dirName
        putStrLn $ "Folder Created: " ++ dirName

        let generations = take 20 $ iterate nextGen (patFn 0)  -- Generate 20 frames for each pattern
        mapM_
          (\(j, matrix) -> do
              let image = matrixToImage matrix
              let fileName = dirName ++ "/frame" ++ show j ++ ".png"
              savePngImage fileName (ImageRGB8 image)
              putStrLn $ "Saved: " ++ fileName
          )
          (zip [0..] generations)
    )
    patternFuncs