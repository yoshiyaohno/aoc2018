import qualified Data.Set as S

main = do
    input <- readFile "input.txt"
    let jef = lines input
    let ods = joof jef
    let tds = jaaf jef
    let jjjjj = ans2 jef
    putStrLn $ show $ jjjjj
    where   joof = length . filter (not . null) . map oneDup  
            jaaf = length . filter (not . null) . map twoDup  

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

ans2 :: [[Char]] -> [Char]
ans2 (x:xs) = if length jjjf == 1
        then jjjf !! 0 else ans2 xs
    where jjjf = filter boff aaaa
          aaaa = map (greyDiff x) xs
          boff = (1==) . count ' '

greyDiff :: [Char] -> [Char] -> [Char]
greyDiff _ [] = []
greyDiff [] _ = []
greyDiff (x:xs) (y:ys) = (if (x==y) then (x:) else (' ':))
    $ greyDiff xs ys

oneDup :: [Char] -> S.Set Char
oneDup = _oneDup S.empty S.empty S.empty

_oneDup :: S.Set Char -> S.Set Char -> S.Set Char -> [Char] -> S.Set Char
_oneDup one two three [] = two
_oneDup one two three (x:xs)
    | S.member x three  = _oneDup one two three xs
    | S.member x two    = _oneDup one (S.delete x two) (S.insert x three) xs
    | S.member x one    = _oneDup one (S.insert x two) three xs
    | otherwise         = _oneDup (S.insert x one) two three xs

twoDup :: [Char] -> S.Set Char
twoDup = _twoDup S.empty S.empty S.empty S.empty

_twoDup :: S.Set Char -> S.Set Char -> S.Set Char -> S.Set Char -> [Char] -> S.Set Char
_twoDup one two three four [] = three
_twoDup oneVist twoVist threeVist fourVist (x:xs)
    | S.member x fourVist   = _twoDup oneVist twoVist threeVist fourVist xs
    | S.member x threeVist  = _twoDup oneVist twoVist (S.delete x threeVist) (S.insert x fourVist) xs
    | S.member x twoVist    = _twoDup oneVist twoVist (S.insert x threeVist) fourVist xs
    | S.member x oneVist    = _twoDup oneVist (S.insert x twoVist) threeVist fourVist xs
    | otherwise             = _twoDup (S.insert x oneVist) twoVist threeVist fourVist xs
