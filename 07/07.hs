import qualified Data.Map as M
import qualified Data.Set as S

main = do
    input <- readFile "input.txt"
    let jeff = lines $ input
        tree = parseTree jeff
        ans  = ans1 tree
    putStrLn . show $ ans

type TechTree = M.Map Char (S.Set Char)

parseRec :: String -> (Char, Char)
parseRec s = (head step, head next)
    where [_,step,_,_,_,_,_,next,_,_] = words s

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

buildTree :: [(Char, Char)] -> TechTree
buildTree []     = M.fromList [(x, S.empty) | x <- ['A'..'Z']]
buildTree (r:rs) = M.adjust (S.union . S.singleton $ snd r) (fst r)
                       (buildTree rs)

parseTree :: [String] -> TechTree
parseTree = buildTree . map parseRec

frontier :: TechTree -> S.Set Char
frontier tree =
    S.fromList [x | x <- M.keys tree, notElem x treElems]
        where treElems = fuse . map S.elems . M.elems $ tree

ans1 :: TechTree -> [Char]
ans1 tree
    | null tree = []
    | otherwise = nextStep : (ans1 $ M.delete nextStep tree)
    where nextStep = minimum . frontier $ tree

fuse :: [[a]] -> [a]
fuse = foldl (++) []
