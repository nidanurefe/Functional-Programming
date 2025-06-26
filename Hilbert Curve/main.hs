-- Nida Nur Efe 

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import GHC.Exts.Heap (IndexTable(it_back_edge), GenClosure (n_args))
import Data.List
import Data.Ord

type Point = (Float,Float) -- 3D point locations
type Line = (Point, Point, Float) -- Two points for a line and the width
type Triangle = (Point, Point, Point) --To save STL files

sortClockwise :: [Point] -> [Point]
sortClockwise ps = sortBy (flip $ comparing (angle c)) ps
  where
    c = (avg (map fst ps), avg (map snd ps))
    avg xs = sum xs / fromIntegral (length xs)
    angle (cx, cy) (x, y) = atan2 (y - cy) (x - cx)


line_list :: [Line]
line_list = [((-5,-0.25),(5,0.25),1)]

create_triangles_from_rectangle :: [Point] -> [Triangle]
create_triangles_from_rectangle corners = [tri1, tri2, tri3, tri4]
    where
        ([x1,x2,x3,x4], [y1,y2,y3,y4]) = unzip(sortClockwise corners)
        middle_point = ((x1+x2+x3+x4) /4, (y1+y2+y3+y4)/4)
        tri1 = ((x1,y1), middle_point, (x2,y2))
        tri2 = ((x2,y2), middle_point, (x3,y3))
        tri3 = ((x3,y3), middle_point, (x4,y4))
        tri4 = ((x4,y4), middle_point, (x1,y1))


get_rectangle_corners :: Line -> [(Float,Float)]
get_rectangle_corners ((x1,x2), (y1,y2), width) = [(x1 + hwx, y1 + hwy), (x1 - hwx, y1 - hwy), (x2 - hwx, y2 - hwy), (x2 + hwx, y2 + hwy)]
    where
        length = sqrt((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1))
        nx = -(y2 - y1) / length
        ny = (x2 - x1) / length
        hwx = nx * width / 2
        hwy = ny * width / 2 

linelist_to_rects :: [Line] -> [Triangle]
linelist_to_rects line_list = concat [create_triangles_from_rectangle (get_rectangle_corners line) | line <- line_list]

createTriangleDef :: Triangle -> String
createTriangleDef ((x1,y1),(x2,y2),(x3,y3)) = "  facet\n" ++
                                                       "    outer loop\n" ++
                                                       "      vertex " ++ (show x1) ++ " " ++ (show y1) ++ " 0 \n" ++
                                                       "      vertex " ++ (show x2) ++ " " ++ (show y2) ++ " 0 \n" ++
                                                       "      vertex " ++ (show x3) ++ " " ++ (show y3) ++ " 0 \n" ++
                                                       "    endloop\n" ++
                                                       "  endfacet\n"                            

createObjectModelString :: [Triangle] -> String 
createObjectModelString n = "solid Object01\n" ++ concat [createTriangleDef y | y<-n] ++ "endsolid Object01"



writeObjModel :: [Triangle] -> String -> IO ()
writeObjModel x filename = do writeFile filename (createObjectModelString x)

hilbertcurve :: Integer -> [Line]
hilbertcurve order = getLines (generateHilbert (fromIntegral order) 10.0) 0.1
  where
    -- Generate the Hilbert curve points of order n and size 
    generateHilbert :: Int -> Float -> [Point]
    generateHilbert n size = recurse 0 0 size 0 0 size n

     -- Convert the list of points into a list of lines with given width
    getLines :: [Point] -> Float -> [Line]
    -- Returns an empty list if there are no points or only one point
    getLines [] _ = []
    getLines [_] _ = []
    -- Returns a list of lines with the given width
    getLines ((x1, y1):(x2, y2):rest) w = ((x1, x2), (y1, y2), w) : getLines ((x2, y2):rest) w

    -- Recursive function to construct the Hilbert curve
    -- Parameters:
    -- x, y   : current position
    -- xi, xj : direction vector along the x-axis
    -- yi, yj : direction vector along the y-axis
    -- n      : current order of the curve
    recurse :: Float -> Float -> Float -> Float -> Float -> Float -> Int -> [Point]
    recurse x y xi xj yi yj n
      | n <= 0    = [(x + (xi + yi) / 2, y + (xj + yj) / 2)] -- base case: return the midpoint 
      | otherwise = concat
          -- Recursive calls to generate the four segments of the curve
          [ recurse x y (yi/2) (yj/2) (xi/2) (xj/2) (n - 1)
          , recurse (x + xi/2) (y + xj/2) (xi/2) (xj/2) (yi/2) (yj/2) (n - 1)
          , recurse (x + xi/2 + yi/2) (y + xj/2 + yj/2) (xi/2) (xj/2) (yi/2) (yj/2) (n - 1)
          , recurse (x + xi/2 + yi) (y + xj/2 + yj) (-yi/2) (-yj/2) (-xi/2) (-xj/2) (n - 1)
          ]

   

main = do writeObjModel (linelist_to_rects (hilbertcurve 8)) "hilbert6.stl" 


    

    


    
