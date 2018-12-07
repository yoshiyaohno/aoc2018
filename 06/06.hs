import qualified Data.List as L
import qualified Data.Set  as S

type Point = (Int, Int)

main = do
    input <- readFile "input.txt"
    let jeff = map parsePoint $ lines input
    let woff = ans2 jeff
    putStrLn . show . length $ woff

parsePoint :: String -> Point
parsePoint s = (x, y)
    where y = read $ (words s) !! 1
          x = read . fst . break (==',') . head . words $ s


manhDist :: Point -> Point -> Int
manhDist (a,b) (x,y) = abs (a-x) + abs (b-y)

bounds :: [Point] -> (Point, Point)
bounds pts = ((minimum xs, minimum ys),
              (maximum xs, maximum ys))
    where xs = map fst pts
          ys = map snd pts

closest :: [Point] -> Point -> Point
closest pts pt
    | length cands == 1  = head cands
    | otherwise          = (-1, -1)
    where minDist = minimum (map ptDst pts)
          ptDst   = manhDist pt
          cands   = filter ((==minDist) . ptDst) pts

traverseRect = curry _traverseRect

_traverseRect :: (Point, Point) -> [[Point]]
_traverseRect ((x1, y1), (x2,y2))
    | (x1>x2) || (y1>y2)  = []
    | x1 == x2   =  [[(x1, y) | y <- [y1..y2]]]
    | y1 == y2   =  [[(x, y1) | x <- [x1..x2]]]
    | otherwise  =  ([(x, y1) | x <- [x1..x2-1]]
                  ++ [(x2, y) | y <- [y1..y2-1]]
                  ++ [(x, y2) | x <- reverse [x1+1..x2]]
                  ++ [(x1, y) | y <- reverse [y1+1..y2]])
                  :  _traverseRect ((x1+1, y1+1), (x2-1,y2-1))

fuse :: [[a]] -> [a]
fuse = foldl (++) []

group :: Ord a => [a] -> [(Int, a)]
group [] = []
group ls = (1+length yes, x) : group rest
    where (x:xs) = L.sort ls
          (yes,rest) = span (x==) xs

mode :: Ord a => [a] -> (Int, a)
mode = maximum . group

popGrid :: [Point] -> [[Point]]
popGrid pts = map (map $ closest pts) rect
    where rect = _traverseRect . bounds $ pts

ans1 :: [Point] -> (Int, Point)
ans1 pts = mode rect
    where (outer:rest) = popGrid pts
          rect = gayRem (S.fromList outer) (fuse rest)
          
gayRem :: Ord a => S.Set a -> [a] -> [a]
gayRem _ [] = []
gayRem s (x:xs)
    | S.member x s  = gayRem s xs
    | otherwise     = x:gayRem s xs

distAll :: [Point] -> Point -> Int
distAll pts pt = foldl (+) 0 $ map (manhDist pt) pts

ans2 :: [Point] -> [Point]
ans2 pts = filter ((<10000) . distAll pts) rect
    where rect = fuse . _traverseRect . bounds $ pts
