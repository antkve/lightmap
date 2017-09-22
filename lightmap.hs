import Data.Function
import Data.List

--- Addto geom <<<
type Point = (Float, Float)
getpoints tuples = concat $ map tolist tuples
tolist (x, y) = [x, y]
type Polygon = [Point]
type Line = (Point, Point)

-- Finds relative orientation of three points as ordering;
-- GT == anticlockwise, LT == clockwise, EQ == colinear
orientation :: Point -> Point -> Point -> Ordering
orientation (px, py) (qx, qy) (rx, ry) = compare ((qy-py)*(rx-qx)-(qx-px)*(ry-qy)) 0

--
anglefrompoint (sourceX, sourceY) (x, y) = atan2 (y - sourceY) (x - sourceX)

-- Tests for segment intersection using endpoint orientation (very computationally easy)
segintersects :: (Point , Point) -> (Point, Point) -> Bool
segintersects (p1, q1) (p2, q2) = 
    (o1 /= o2 && o3 /= o4) && p1 /= p2 && p1 /= q2 && q1 /= p2 && q1 /= q2
    where o1 = orientation p1 q1 p2
          o2 = orientation p1 q1 q2
          o3 = orientation p2 q2 p1
          o4 = orientation p2 q2 q1

-- finds intersection of lines from two points on each line with vector algebra
lineintersection :: (Point, Point) -> (Point, Point) -> Point
lineintersection ((px1, py1), (px2, py2)) ((qx1, qy1), (qx2, qy2)) =
    (q1 + u1 * s, q2 + u2 * s)
    where s | r1 /= 0   = (q2 + r2*p1/r1 - p2 - r2*q1/r1) / (u1*r2/r1 - u2)
            | otherwise = (r2*p1 - r2*q1)/(u1*r2)
          (p1, p2) = (px1, py1)
          (r1, r2) = (px2 - px1, py2 - py1)
          (q1, q2) = (qx1, qy1)
          (u1, u2) = (qx2 - qx1, qy2 - qy1)

-- Maybe returns segment intersection
intersection :: Line -> Line -> Maybe Point
intersection l1 l2   | l1 `segintersects` l2 = Just (lineintersection l1 l2)
                     | otherwise             = Nothing


intersections line [] = []
intersections line lines = foldl addjust [] $ map (intersection line) lines
    where addjust acc (Just x) = x:acc
          addjust acc Nothing  = acc


-- These take a line in v + rs form and one coordinate and find the other 
vlinefindyfromx (x0, y0) (rx, ry) x = y0 + ry*(x - x0)/rx
vlinefindxfromy (x0, y0) (rx, ry) y = x0 + rx*(y - y0)/ry

--- >>>
--
--
--- Addto disp <<<

draw :: ((Int, Int) -> a) -> Int -> Int -> [[a]]
draw testandreplace xBound yBound = map (map testandreplace) startingList
    where startingList = [[(x, y) | x <- [0..xBound]] | y <- [0..yBound]]

-- For using a testing function that involves the whole line
drawperline :: ([(Int, Int)] -> [a]) -> Int -> Int -> [[a]]
drawperline testAndReplace xBound yBound = map (testAndReplace) startingList
    where startingList = [[(x, y) | x <- [0..xBound]] | y <- [0..yBound]]



drawpoly :: Int -> Int -> Polygon -> [[Char]]
drawpoly xBound yBound polyPoints = drawperline (testAndReplace []) xBound yBound
    where testAndReplace [] ((x, y):tiles) = testAndReplace (intersections ((-1, fromIntegral y), (fromIntegral xBound + 1, fromIntegral y - 1)) (tolines polyPoints (head polyPoints))) tiles
          testAndReplace xs ((tileX, tileY):tiles) | even $ length [x | x <- (map fst xs), x < fromIntegral tileX] = '/' : testAndReplace xs tiles
                                                   | otherwise = ' ' : testAndReplace xs tiles
          testAndReplace _ [] = []
                      --
          
            --  | odd $ length $ getallnextintersects ((fromIntegral x, fromIntegral y), (-100, fromIntegral y - 0.5)) 
            --        $ tolines lightPoints (head lightPoints) = ' ' 
            --  | otherwise = '/'
tolines :: Polygon -> Point -> [Line]
tolines (x:y:xs) firstPoint = (x, y):tolines (y:xs) firstPoint
tolines [x] firstPoint = [(x, firstPoint)]







--- >>>
--- Addto std <<<

replaceWithIf :: (a -> a -> Bool) -> [a] -> [a] -> [a]
replaceWithIf bf _ [] = []
replaceWithIf bf (x:xs) (y:ys) | bf x y    = y:replaceWithIf bf xs ys
                               | otherwise = x:replaceWithIf bf xs ys
--- >>>


data LinePointLabel = Start | End deriving (Eq)
type LabeledLinePoint = (Point, LinePointLabel)
labelof = snd
pointof = fst

-- Labels each point in walls
labeledpointsof :: [Line] -> Point -> [LabeledLinePoint]
labeledpointsof walls sourcePos@(sourceX, sourceY) = concat (map (invertgrouping . label) walls)
     where label (point1, point2) 
             | on compare (anglefrompoint sourcePos) point1 point2 == GT = (reverseif (End, Start) point1 point2, (point1, point2))
             | otherwise = (reverseif (Start, End) point1 point2, (point1, point2))
           reverseif (a, b) point1 point2  | orientation point1 sourcePos (sourceX - 1, sourceY) == EQ = (a, b)
                                           -- check if intersects atan shear line                                         
                                           | segintersects (point1, point2) (sourcePos, (sourceX - 10000, sourceY)) = (b, a)
                                           | otherwise = (a, b)
           invertgrouping ((l1, l2), (p1, p2)) = [(p1, l1), (p2, l2)]

data ScreenMap = ScreenMap { 
  getBounds    :: (Int, Int), 
  getLevelMap  :: [[Int]], 
  getLines     :: [Line],
  getSourcePos :: Point}



isinbounds (x, y) (xBound, yBound) = x <= xBound 
                                  && x >= 0
                                  && y <= yBound
                                  && y >= 0

addshadows :: [[Char]] -> [Line] -> Point -> Int -> Int -> [[Char]]
addshadows levelMap walls sourcePos xBound yBound = 
    zipWith (replaceWithIf (\x y -> x == ' ' && y=='/')) levelMap $ drawshadow walls sourcePos xBound yBound

globLines = [((180, 60), (0, 60)), ((0, 60), (0, 0)), ((0, 0), (180, 0)), ((180, 0), (180, 60)),
                  ((60, 38), (70, 39)), ((80, 40), (90, 41)), ((100, 42), (110, 43)), ((120, 44), (130, 45)), ((150, 47), (160, 48)), ((69, 15), (51, 30))]

doit sourceX sourceY = mapM_ putStrLn $ addshadows (draw walldraw 180 61) globLines (sourceX, sourceY) 180 61 
    where walldraw (x, y) | tileisnear (x, y) `any` globLines = 'O'
                          | sourceX == fi x && sourceY == fi y = '#'
                          | (fi x, fi y) == (sourceX - 1, sourceY) = '<'
                          | (fi x, fi y) == (sourceX + 1, sourceY) = '>'
                          | (fi x, fi y) == (sourceX, sourceY - 1) = 'A'
                          | (fi x, fi y) == (sourceX, sourceY + 1) = 'V'
                          | otherwise = ' '
          tileisnear (x, y) wall = ((fi x - 0.5, fi y - 0.5), (fi x + 0.5, fi y + 0.5)) `segintersects` wall ||
                            ((fi x - 0.5, fi y + 0.5), (fi x + 0.5, fi y - 0.5)) `segintersects` wall
          fi x = fromIntegral x
-- DOIT --
--
--
--



drawshadow :: [Line] -> Point -> Int -> Int -> [[Char]]
drawshadow walls sourcePos xBound yBound = drawpoly xBound yBound $ nub $ shadowshape sourcePos walls




                                        

-- Takes source position, points labeled as start or end of wall, and list of
-- walls and returns polygon of lit area
-- Special handling for case of wall points on either side of atan shear
shadowshape :: Point -> [Line] -> Polygon
shadowshape sourcePos walls = foldl addlightpoints [] $ labeledPointsInOrder
    where addlightpoints acc labeledPoint | or (map ((sourcePos, pointof labeledPoint) `segintersects`) walls) = acc
                                          | labelof labeledPoint == Start = 
                                              (pointof labeledPoint):(projectedpoint labeledPoint):acc
                                          | otherwise = (projectedpoint labeledPoint):(pointof labeledPoint):acc 
          projectedpoint labeledPoint = getnextintersect (sourcePos, pointof labeledPoint) walls
          labeledPointsInOrder = sortBy ( compare `on` (anglefrompoint sourcePos . fst)) (labeledpointsof walls sourcePos)

getnextintersect :: (Point, Point) -> [(Point, Point)] -> Point
getnextintersect seg@((sourceX, sourceY), (pointX, pointY)) segs = 
    minimumby (compare `on` distfromsrc) (getallnextintersects seg segs) 
    where distfromsrc (x, y) = tupsum (x - sourceX, y - sourceY)
          tupsum (x, y) = x^2 + y^2
          minimumby f [] = (sourceX + 4*(pointX - sourceX), sourceY + 4*(pointY - sourceY))
          minimumby f xs = head $ drop 1 $ sortBy f xs

getallnextintersects seg@((x1, y1), (x2, y2)) segs = 
    filter (\(x, y) -> ((x2>x1) && x > x1) || ((x1>x2) && x < x1) || ((x1 == x2) && ((y1 < y2 && y > y1 ) || (y1 > y2 && y < y1 ))))
    $ getallintersects seg segs

--segray is hacky as hell, come up with replacement
getallintersects seg segs = foldl foldfunction [] segs
    where foldfunction acc l = (intersection (segray seg) l) `addjust` acc  
          segray ((px, py), (qx, qy)) = ((px, py), (px + 100 * (qx-px), py + 100*(qy-py)))
          addjust (Just b) xs = b:xs
          addjust Nothing xs  = xs
    
          

-- NEED TO ITERATE OVER WALL POINTS IN ROTATIONAL ORDER AND BE ABLE TO FIND
-- WALL POINT'S PAIR AND WHERE THAT PAIR IS
