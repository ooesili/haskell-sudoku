import Data.List
import Control.Monad
import System.Environment
import System.IO

data Number = Solved Int | CanBe [Int] deriving (Eq)
type Puzzle = [[Number]]
type Col = [Number]
type Chunk = [Number]

instance Show Number where
    show (Solved x) = show x
    show (CanBe  _) = " "

main :: IO ()
main = do
    args <- getArgs
    case args of []     -> doPuzzle stdin
                 [file] -> withFile file ReadMode doPuzzle
                 _      -> error "too many arguments"

doPuzzle :: Handle -> IO ()
doPuzzle handle = do
    puzzle <- fmap concat . replicateM 3 $ do
        void $ hGetLine handle
        replicateM 3 (fmap stripPipes $ hGetLine handle)
    let maybeSolved = solve $ puzzleParse puzzle
    case maybeSolved of (Just p) -> printPuzzle p
                        Nothing  -> error "no solutions"
    where stripPipes = filter (/= '|')

-- prints a puzzle
printPuzzle :: Puzzle -> IO ()
printPuzzle p = do
    mapM_ printThreeRows $ splitThrees p
    putVertLine
    where putVertLine = putStrLn (replicate 13 '-') 
          printRow = putStrLn
                   . ('|':) . (++"|")
                   . intercalate "|"
                   . splitThrees
                   . concat
                   . map show
          printThreeRows threeRows = do
              putVertLine
              mapM_ printRow threeRows

-- parses a puzzle
puzzleParse :: [String] -> Puzzle
puzzleParse = map (map toNumber)
    where toNumber ' ' = CanBe [1..9]
          toNumber x   = Solved (read [x])

-- solves a puzzle
solve :: Puzzle -> Maybe Puzzle
solve p = do
    tried <- try p
    if isSolved tried  -- sees if its solved
       then Just tried -- success!
       else if p /= tried       -- did trySolve get anywhere?
               then solve tried -- if it did, keep trying
               else (do         -- otherwise try the unique strategy
                   uniqued <- unique tried
                   if uniqued /= tried    -- sees if tryUnique got anywhere
                      then solve uniqued  -- if it did, keept trying
                      else guess uniqued) -- else, time to guess
    where try    = puzzleMap trySolve             return
          unique = puzzleMap (return . tryUnique) try

-- temporary place holder for guessing function
guess :: Puzzle -> Maybe Puzzle
guess = const Nothing

-- maps f over the rows, columns and chunks (in that order)
-- applies g to between trying rows, columns, and chunks
puzzleMap :: ([Number] -> Maybe [Number]) ->
             (Puzzle -> Maybe Puzzle) -> Puzzle -> Maybe Puzzle
puzzleMap f g p = (f `onRows` p) >>= g
              >>= (f `onCols`)   >>= g
              >>= (f `onChunks`) >>= g

-- sees if puzzle is done
isSolved :: Puzzle -> Bool
isSolved = and . map (all go)
    where go (Solved _) = True
          go (CanBe  _) = False

-- applys a function to a Number and marks it solved if
-- that function returns only one number
smap :: ([Int] -> [Int]) -> Number -> Maybe Number
smap _ (Solved x) = Just (Solved x)
smap f (CanBe xs)
    | length xs' == 1 = Just (Solved (head xs'))
    | length xs' >  1 = Just (CanBe xs')
    | otherwise       = Nothing
    where xs' = f xs

-- returns all of the solved numbers
getSolved :: [Number] -> [Int]
getSolved = concat . map go
    where go (Solved x) = [x]
          go (CanBe  _) = []

getUnsolved :: [Number] -> [Int]
getUnsolved = concat . map go
    where go (Solved _) = []
          go (CanBe xs) = xs

-- removes impossible solutions from each CanBe
trySolve :: [Number] -> Maybe [Number]
trySolve ns = mapM (smap removeSolved) ns
    where solved         = getSolved ns
          removeSolved n = n \\ solved

-- solves CanBe's with a unique possilbity
tryUnique :: [Number] -> [Number]
tryUnique ns = map solveUnique ns
    where unsolved = getUnsolved ns
          uniques = map fst . filter (\(_,n) -> n == 1) $ count unsolved
          solveUnique      (Solved x) = (Solved x)
          solveUnique orig@(CanBe xs) =
              let unique = (xs `intersect` uniques)
              in if null unique
                 then orig
                 else Solved (head unique)

-- count the number of elements in list
count :: (Eq a) => [a] -> [(a, Int)]
count ns = foldl go (zip (nub ns) (repeat 0)) ns
    where go []         _   = error "your code done goofed, bro"
          go (a@(e,n):as) x = if x == e then (e, n+1):as
                                        else a : go as x

-- splits a list in two three groups of three
splitThrees :: [a] -> [[a]]
splitThrees [] = error "empty row/col/chunk"
splitThrees xs = [one, two, three]
    where (one, twoAndThree) = splitAt 3 xs
          (two, three)       = splitAt 3 twoAndThree

-------- conversions and applications for columns and chunks --------
-- try a function over the chunks
onCols :: ([Number] -> Maybe [Number]) -> Puzzle -> Maybe Puzzle
onCols f p = mapM f (toCols p) >>= (return . fromCols)

-- try a function over the chunks
onChunks :: ([Number] -> Maybe [Number]) -> Puzzle -> Maybe Puzzle
onChunks f p = mapM f (chunk p) >>= (return . unchunk)

-- try a function over the rows
onRows :: ([Number] -> Maybe [Number]) -> Puzzle -> Maybe Puzzle
onRows = mapM

-- columnize and uncolumnize functions
-- yes they are the same, but the type signatures will help detect errors
toCols :: Puzzle -> [Col]
toCols = transpose

fromCols :: [Col] -> Puzzle
fromCols = transpose

-- chunks and unchunk functions
-- fun fact: unchunk is just chunk reversed, with every instance of
-- concat replaced with splitThrees, and vice versa
chunk :: Puzzle -> [Chunk]
chunk = map concat      -- join triplets (chunks are still separate)
      . concat          -- join columns (triplet chunks are still separate)
      . map splitThrees -- split column into chunks of triplets
      . transpose       -- return 3 columns of triplets
      . map splitThrees -- split rows into triplets

unchunk :: [Chunk] -> Puzzle
unchunk = map concat      -- flatten triplets in each row
        . transpose       -- groups triplets into their original rows
        . map concat      -- flatten chunks in each column
        . splitThrees     -- split chunks by column
        . map splitThrees -- split chunks by row, into triplets
