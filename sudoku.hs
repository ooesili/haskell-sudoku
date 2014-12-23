import Data.List
import Control.Monad
import System.Environment
import System.IO
import Data.Maybe

-------- DATA TYPES--------

-- either a single, solved number, or a list of possibilities
data Number = Solved Int | CanBe [Int] deriving (Eq)
-- puzzles are stowed as rows
type Puzzle = [[Number]]
-- these two represent the columns and 3x3 chunks of the puzzle
type Col = [Number]
type Chunk = [Number]

instance Show Number where
    show (Solved x) = show x
    show (CanBe  _) = " "


-------- DIRTY IO STUFF --------

-- if an argument is given, use that for input, otherwise use stdin
main :: IO ()
main = do
    args <- getArgs
    case args of []     -> doPuzzle stdin
                 [file] -> withFile file ReadMode doPuzzle
                 _      -> error "too many arguments"

-- reads puzzle from given file handle and tries to solve it
doPuzzle :: Handle -> IO ()
doPuzzle handle = do
    -- read the three groups of rows
    puzzle <- fmap concat . replicateM 3 $ do
        -- discard first row (used for formatting)
        void $ hGetLine handle
        replicateM 3 (fmap stripPipes $ hGetLine handle)
    -- try to solve puzzle
    let maybeSolved = solve $ puzzleParse puzzle
    case maybeSolved of (Just p) -> printPuzzle p
                        Nothing  -> error "no solutions"
    -- after pipes are stripped, 9 characters should remain
    where stripPipes = filter (/= '|')

-- prints a puzzle
printPuzzle :: Puzzle -> IO ()
printPuzzle p = do
    mapM_ printThreeRows $ splitThrees p
    -- print a vertical line after everything else
    putVertLine
    where putVertLine = putStrLn (replicate 13 '-') 
          printRow = putStrLn         -- print
                   . ('|':) . (++"|") -- surround with pipes
                   . intercalate "|"  -- join with pipes
                   . splitThrees      -- split into 3 lists of 3 characters
                   . concat           -- [String] -> String
                   . map show         -- [Number] -> [String]
          -- print groups of three rows with vertical line before each one
          printThreeRows threeRows = do
              putVertLine
              mapM_ printRow threeRows

-- parses a puzzle from a list of numbers and spaces
puzzleParse :: [String] -> Puzzle
puzzleParse = map (map toNumber)
    where toNumber ' ' = CanBe [1..9]
          toNumber x   = Solved (read [x])


-------- MAIN PUZZLE SOLVING ALGORITHM --------

-- tries solves a puzzle, from start to finish
-- a strategy function returning Nothing indicates that the puzzle is
-- unsolvable
-- that Nothing will propagate upwards and cause solve to return Nothing
solve :: Puzzle -> Maybe Puzzle
solve p = do
    tried <- try p
    if isSolved tried  -- sees if its solved
       then Just tried -- success!
       else if p /= tried       -- did trySolve get anywhere?
               then solve tried -- if it did, keep trying
               else (do         -- otherwise try the unique strategy
                   uniqued <- unique tried
                   if uniqued /= tried       -- sees if tryUnique got anywhere
                      then solve uniqued     -- if it did, keep trying
                      else tryGuess uniqued) -- else, time to guess
    where try    = puzzleMap trySolve             return
          unique = puzzleMap (return . tryUnique) try

-- first strategy
-- operates on a single row/column/chunk
-- removes impossible solutions from each CanBe
trySolve :: [Number] -> Maybe [Number]
trySolve ns = mapM (smap removeSolved) ns
    where solved         = getSolved ns
          removeSolved n = n \\ solved

-- second strategy
-- operates on a single row/column/chunk
-- solve any CanBes that have unique possibilities
tryUnique :: [Number] -> [Number]
tryUnique ns = map solveUnique ns
          -- get list of all CanBe possibilities, duplicates included
    where unsolved = getUnsolved ns
          -- find possibilities only listed once
          uniques = map fst . filter (\(_,n) -> n == 1) $ count unsolved
          -- mark as solved if this number contains a unique possibility
          solveUnique      (Solved x) = (Solved x)
          solveUnique orig@(CanBe xs) =
              let unique = (xs `intersect` uniques)
              in if null unique then orig
                                else Solved (head unique)

-- third strategy
-- operates on a single row/column/chunk
tryGuess :: Puzzle -> Maybe Puzzle
tryGuess p = listToMaybe . catMaybes . concat $ goRows ([],p)
          -- zip through rows
    where goRows (_,  []   ) = []
          goRows (rp, r:rn)  = goNums (rp, rn, [], r) : goRows (r:rp, rn)
          -- zip through numbers
          goNums (_,  _,  _,  []  ) = []
          goNums (rp, rn, np, n:nn) =
              -- generate a new puzzle for each CanBe possibility
              let replace []     = continue
                  replace (x:xs) =
                      -- reassemble zipper into new puzzle, marking the CanBe
                      -- possibility as solved
                      let row = reverse np ++ (Solved x):nn
                          p' = reverse rp ++ row:rn
                      -- solve, check, and return puzzle
                      in (solve p' >>= checkPuzzle) : replace xs
                  continue = goNums (rp, rn, n:np, nn)
              -- skip past Solved, generate new puzzles for CanBe
              in case n of (Solved _) -> continue
                           (CanBe xs) -> replace xs


-------- PUZZLE HELPER FUNCTIONS --------

-- checks a full puzzle for errors
-- each row, column, and chunk should have 9 unique Solved Numbers
checkPuzzle :: Puzzle -> Maybe Puzzle
checkPuzzle = puzzleMap allGood Just
    where allGood ns =
              let solved = getSolved ns
              in if length (nub solved) == 9 then Just ns
                                             else Nothing

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

-- applies a function to a Number and marks it solved if
-- that function returns only one number
-- if the function returns no number return Nothing
smap :: ([Int] -> [Int]) -> Number -> Maybe Number
smap _ (Solved x) = Just (Solved x)
smap f (CanBe xs)
    | length xs' == 1 = Just (Solved (head xs'))
    | length xs' >  1 = Just (CanBe xs')
    | otherwise       = Nothing
    where xs' = f xs

-- extract solved numbers
getSolved :: [Number] -> [Int]
getSolved = concat . map go
    where go (Solved x) = [x]
          go (CanBe  _) = []

-- extracted unsolved numbers
getUnsolved :: [Number] -> [Int]
getUnsolved = concat . map go
    where go (Solved _) = []
          go (CanBe xs) = xs


-------- CONVERSIONS AND APPLICATIONS FOR COLUMNS AND CHUNKS --------

-- try a function over the chunks
onCols :: ([Number] -> Maybe [Number]) -> Puzzle -> Maybe Puzzle
onCols f p = mapM f (toCols p) >>= (return . fromCols)

-- try a function over the chunks
onChunks :: ([Number] -> Maybe [Number]) -> Puzzle -> Maybe Puzzle
onChunks f p = mapM f (toChunks p) >>= (return . fromChunks)

-- try a function over the rows
onRows :: ([Number] -> Maybe [Number]) -> Puzzle -> Maybe Puzzle
onRows = mapM

-- columnize and uncolumnize functions
-- yes they are the same, but the type signatures will help detect errors
toCols :: Puzzle -> [Col]
toCols = transpose

fromCols :: [Col] -> Puzzle
fromCols = transpose

-- chunk and unchunk functions
-- fun fact: fromChunks is just toChunks reversed, with every instance of
-- concat replaced with splitThrees, and vice versa
toChunks :: Puzzle -> [Chunk]
toChunks = map concat      -- join triplets (chunks are still separate)
         . concat          -- join columns (triplet chunks are still separate)
         . map splitThrees -- split column into chunks of triplets
         . transpose       -- return 3 columns of triplets
         . map splitThrees -- split rows into triplets

fromChunks :: [Chunk] -> Puzzle
fromChunks = map concat      -- flatten triplets in each row
           . transpose       -- groups triplets into their original rows
           . map concat      -- flatten chunks in each column
           . splitThrees     -- split chunks by column
           . map splitThrees -- split chunks by row, into triplets


-------- ABSTRACT FUNCTIONS --------

-- count the number of unique elements in list
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
