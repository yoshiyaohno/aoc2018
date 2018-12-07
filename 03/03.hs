import qualified Data.Set as S

type Cloth = S.Set (Int, Int)

main = do
  input <- readFile "input.txt"
  let jeff = lines input
  let patches = map parseRect jeff
  let quilt = foldl chewClaim (S.empty, S.empty) patches 
  let ans2 = filter (disjoint (snd quilt) . process) patches
  putStrLn $ show ans2

data Rect = Rect {
    id     :: Int ,
    x      :: Int ,
    y      :: Int , 
    width  :: Int ,
    height :: Int
    } deriving (Show) 

disjoint :: Ord a => S.Set a -> S.Set a -> Bool
disjoint x y = S.empty == S.intersection x y

process :: Rect -> Cloth
process (Rect _ x y wt ht) =
    S.fromList [(a,b) | a <- [x..x+wt-1], b <- [y..y+ht-1]]

chewClaim :: (Cloth, Cloth) -> Rect -> (Cloth, Cloth)
chewClaim (cover, over) r = 
  ((S.union cover new), (S.union over $ S.intersection cover new))
    where new = process r
    

parseRect :: String -> Rect
parseRect s = Rect id (read x) (read y) (read wt) (read ht)
    where [n, _, pos, size] = words s
          id = read $ tail n
          (x, y) = splitFirst ',' . fst $ splitFirst ':' pos
          (wt,ht) = splitFirst 'x' size
          
splitFirst :: (Eq a) => a -> [a] -> ([a],[a])
splitFirst t (x:xs) = if (t==x) then ([], xs)
    else let (a, b) = splitFirst t xs in (x:a, b)
