import qualified Data.List  as L
import qualified Data.Set   as S
import qualified Data.Map   as M
import qualified Data.Maybe as Mb

type Slps = S.Set Interval

data Guard = Guard {
    dude     :: Int
    , sleeps :: S.Set Interval
    } deriving (Show, Ord)

data Interval = Interval {
    start :: Int
    , end :: Int
    } deriving (Show, Ord, Eq)

data Content = Shift Int | Sleep | Wake
    deriving (Show)

data Entry   = Entry Int Content
    deriving (Show)

instance Eq Guard where
    a == b  = dude a == dude b

main = do
    input <- readFile "input.txt"
    let jeff = lines input
        log = L.sort jeff
        entries = map parseEntry log
        guards = procGuards entries
        guardTimes = M.map coverage guards
        nutt = M.elems $ guardTimes
        ans2 = M.filter ((17==) . (!!47)) guardTimes
        theDude = 3457
    --putStrLn . show . coverage . Mb.fromJust $ M.lookup theDude guards
    putStrLn . show $ ans2

sumIntervals :: Slps -> Int
sumIntervals = S.foldl (+) 0 . S.map spanVal

coverage :: Slps -> [Int]
coverage ss = foldl (zipWith (+)) (replicate 60 0) (S.map itvList ss)

itvList :: Interval -> [Int]
itvList (Interval a b) =
    (replicate a 0) ++ (replicate (b-a) 1) ++ (replicate (60-b) 0)

spanVal :: Interval -> Int
spanVal (Interval a b) = b - a

insGuard :: M.Map Int Slps -> Guard -> M.Map Int Slps
insGuard m g
    | M.member (dude g) m = M.insert (dude g)
                        (S.union (sleeps g) (Mb.fromJust d))
                        m
    | otherwise = M.insert (dude g) (sleeps g) m
    where d = M.lookup (dude g) m
    
parseContent :: String -> Content
parseContent s
    | s == " falls asleep"   = Sleep
    | s == " wakes up"       = Wake
    | otherwise              = Shift (read nomb)
    where [_, ('#':nomb), _, _] = words s

parseEntry :: String -> Entry
parseEntry s = Entry min (parseContent content)
    where min = read . tail . snd . break (==':') $ (fst ok)
          content = tail $ snd ok
          ok = break (==']') s

procGuards :: [Entry] -> M.Map Int Slps
procGuards [] = M.empty
procGuards ((Entry i (Shift g)) : xs) =
    insGuard (procGuards $ _nextG xs) (Guard g $ _resvals xs)

_nextG :: [Entry] -> [Entry]
_nextG (Entry i (Shift g) : xs) = (Entry i (Shift g) : xs) 
_nextG (x:xs) = _nextG xs
_nextG [] = []

_resvals :: [Entry] -> Slps
_resvals [] = S.empty
_resvals (Entry _ (Shift _) : _) = S.empty
_resvals ((Entry s Sleep):(Entry w Wake):xs) =
    S.insert (Interval s w) (_resvals xs)
