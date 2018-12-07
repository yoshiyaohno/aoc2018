import qualified Data.Char as C

main = do
    input <- readFile "input.txt"
    let jeff = head . lines $ input
    --let oof = nut jeff
    --let ans = ans1 jeff
    let testIns = map ($jeff) (map remChars ['a'..'z'])
    let ans2 = minimum $ map (length . ans1) testIns
    putStrLn $ show ans2


remChars :: Char -> String -> String
remChars c = filter ((c/=) . C.toLower)

nut :: String -> [String]
nut x = x : nut (collide x)

ans1 :: String -> String
ans1 x
  | jeff == x   = x
  | otherwise   = ans1 jeff
  where jeff = collide x

collide :: String -> String
collide [] = []
collide (x:[]) = [x]
collide (a:b:xs)
  | (a== C.toUpper b) && C.isUpper a && C.isLower b   = collide xs
  | (a== C.toLower b) && C.isUpper b && C.isLower a   = collide xs
  | otherwise            = a : collide (b:xs)
