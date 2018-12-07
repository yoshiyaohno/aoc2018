import Data.List
import qualified Data.IntSet as IntSet
import System.Environment

main = do
    input <- readFile "input.txt"
    let jef = gayRead $ lines input
    --let ans = foldl (+) 0 jef
    let freqs = scanl (+) 0 (cycle jef)
    putStrLn $ show $ firstDup freqs

runFreqs frqs = scanl (+) 0 (rep frqs)
    where rep = concat . repeat

firstDup l = _firstDup l IntSet.empty

_firstDup :: [Int] -> IntSet.IntSet -> Int
_firstDup (x:xs) visited =
    if (IntSet.member x visited) then x
        else _firstDup xs (IntSet.insert x visited)

gayRead :: [String] -> [Int]
gayRead [] = []
gayRead (x:xs) = 
    if (x !! 0 == '+') then (read $ tail x):(gayRead xs)
        else ((read x):(gayRead xs))
