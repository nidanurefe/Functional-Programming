{-# LANGUAGE OverloadedStrings #-} --allows to automatically convert string constants to types like Text.

-- Import necessary libraries
import qualified Data.Map as Map -- for graph representation and mapping MIDs to names
import qualified Data.Set as Set -- for tracking visited nodes in BFS
import qualified Data.Text as T -- for text manipulation
import qualified Data.Text.IO as TIO -- for reading text files
import System.Environment (getArgs) -- for accessing command line arguments
import Data.Maybe (fromMaybe) -- for handling Maybe types
import System.Console.ANSI -- for colored console output
import Data.List (sort, intercalate) -- for list manipulation, sorting and joining strings

-- Define types for MID and Graph
type MID = String -- MID is a string identifier for nodes
type NameMap = Map.Map MID String -- Map from MID to name
type Graph = Map.Map MID [MID] -- Map from MID to list of connected MIDs


-- Construct the graph from the TSV file
buildGraph :: FilePath -> IO Graph
buildGraph path = do 
    content <- TIO.readFile path -- Read the content of the TSV file
    let edges = map (T.splitOn "\t") (T.lines content) -- Split lines into edges
        addEdge g [from, _, to] = -- Function to add an edge to the graph
            let f = T.unpack $ T.strip from -- Convert source MID to String and strip whitespace
                t = T.unpack $ T.strip to -- Convert target MID to String and strip whitespace
                g1 = Map.insertWith (++) f [t] g -- Insert the edge from f to t

                -- Use this line to make the graph undirected
                -- g2 = Map.insertWith (++) t [f] g1  -- Insert the edge from t to f (to make it bidirectional)

            in g1 -- Return the updated graph
            
            -- Uncomment this line if you want the graph to be undirected
            -- in g2
        addEdge g _ = g -- Ignore lines that do not match the expected format
    return $ foldl addEdge Map.empty edges -- Fold over the edges to construct the graph
    
-- Construct the name map from the TSV file
buildNameMap :: FilePath -> IO NameMap
buildNameMap path = do 
    content <- TIO.readFile path -- Read the content of the TSV file
    let pairs = map (T.splitOn "\t") (T.lines content) -- Split lines into MID-name pairs
        parseLine [mid, name] = (T.unpack $ T.strip mid, T.unpack $ T.strip name) -- Parse each line into a (MID, name) tuple
        parseLine _ = ("", "") -- Ignore lines that do not match the expected format
    return $ Map.fromList (map parseLine pairs) -- Convert the list of tuples into a Map

-- Find the shortest path between two MIDs using BFS
bfs :: Graph -> MID -> MID -> Either String ([MID], Int)
bfs graph start goal = bfs' Set.empty [(start, [start])] -- Initialize BFS with an empty visited set and a queue containing the start MID
  where
    bfs' _ [] = Left "No path found between given MIDs." -- If the queue is empty, return no path found
    bfs' visited ((curr, path):queue) -- Process the current node and its path
      | curr == goal = Right (reverse path, length path - 1) -- If the current node is the goal, return the path and distance
      | Set.member curr visited = bfs' visited queue -- If the current node has been visited, skip it
      | otherwise = -- Otherwise, continue BFS
          let neighbors = Map.findWithDefault [] curr graph -- Get the neighbors of the current node
              newQueue = queue ++ [(n, n:path) | n <- neighbors, not (Set.member n visited)] -- Add unvisited neighbors to the queue
          in bfs' (Set.insert curr visited) newQueue -- Recurse with the updated visited set and queue

-- Print the result in expected format
printResult :: NameMap -> ([MID], Int) -> IO () 
printResult nameMap (path, dist) = do -- Print the path and distance
    putStrLn $ "Shortest distance: " ++ show dist -- Print the distance
    let namePath = map (\mid -> fromMaybe mid (Map.lookup mid nameMap)) path -- Map MIDs to names using the name map
    putStrLn $ "Full path: " ++ intercalate " -> " namePath -- Print the full path with names

-- Main function 
main :: IO ()
main = do
    args <- getArgs -- Get command line arguments
    case args of -- Check if there are exactly two arguments
      [startMID, goalMID] -> do 
          graph <- buildGraph "freebase.tsv" -- Build the graph from the TSV file
          nameMap <- buildNameMap "mid2name.tsv" -- Build the name map from the TSV file
          case bfs graph startMID goalMID of -- Perform BFS to find the shortest path
            Left err -> do -- If an error occurs, print it in red
                setSGR [SetColor Foreground Vivid Red] -- Set text color to red
                putStrLn $ "Error: " ++ err -- Print the error message
                setSGR [Reset] -- Reset text color
            Right result -> printResult nameMap result -- If a path is found, print the result
      _ -> putStrLn "Run program with two MIDs as arguments: <startMID> and <goalMID>"  -- If the arguments are not correct, print usage instructions
             
      
