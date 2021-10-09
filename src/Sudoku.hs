module Sudoku (loadSudokuFile, solve) where

import Prelude hiding (lookup)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.List (intercalate, sortBy, nub)
import Data.Function (on)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Control.Exception
import System.IO (stderr, hPutStrLn)


-- For better or worse, Haskell doesn't provide its own 2D-array type (or
-- rather: that particular kind of syntactic sugar). This means that we either
-- have to define it ourselves or use one of the more powerful array libraries.
-- We'll define it ourselves (on top of Vector):
data Matrix a = Matrix !Int !Int !(VU.Vector a) deriving (Eq, Ord)

type Sudoku = Matrix Int -- whereby a sudoku is a 9x9 matrix.

-- Encode subscripts (x and y) as the indices of a 1D-array:
indexOf :: Matrix a -> Int -> Int -> Int
indexOf (Matrix _ column _) x y = x + y * column

-- Read an entry from a 2D-matrix by subscript:
lookup :: VU.Unbox a => Int -> Int -> Matrix a -> a
lookup x y m@(Matrix _ _ v) = (VU.!) v $ indexOf m x y


updateMatrix :: VU.Unbox a => Matrix a -> [(Int, a)] -> Matrix a
updateMatrix (Matrix rows columns v) xs = Matrix rows columns (v VU.// xs)


updateMatrixByCoords :: VU.Unbox a => Matrix a -> [((Int, Int), a)] -> Matrix a
updateMatrixByCoords matrix = updateMatrix matrix . fmap
  (\(tupl, x) -> ( (uncurry . indexOf) matrix tupl, x))


lookupRow :: VU.Unbox a => Matrix a -> Int -> [a]
lookupRow m@(Matrix _ columns _) y = [lookup x y m | x <- [0..columns-1]]


lookupColumn :: VU.Unbox a => Matrix a -> Int -> [a]
lookupColumn m@(Matrix rows _ _) x = [lookup x y m | y <- [0..rows-1]]


instance (Show a, VU.Unbox a) => Show (Matrix a) where
  show m@(Matrix rows _ _) =
    intercalate "\n" $ show <$> [lookupRow m y | y <- [0..rows-1]]


emptySudoku :: Sudoku
emptySudoku = Matrix 9 9 (VU.replicate 81 0)


testMatrix :: Sudoku
testMatrix = Matrix 9 9 (VU.fromList [1..81])


firstVerticalBlockFrom :: Sudoku -> Int -> ([Int],[[Int]])
firstVerticalBlockFrom matrix startRow = traverse threes [startRow..startRow+2]
  where
    threes = splitAt 3 . lookupRow matrix


blocks :: ([Int], [[Int]]) -> [[Int]]
blocks ([] , _)  = []
blocks (block, rest) =
  block : blocks (traverse (splitAt 3) rest)


verticalBlocksFrom :: Sudoku -> Int -> [[Int]]
verticalBlocksFrom matrix startRow =
  blocks $ firstVerticalBlockFrom matrix startRow


allRows :: Sudoku -> [[Int]]
allRows matrix = lookupRow matrix <$> [0..8]


allColumns :: Sudoku -> [[Int]]
allColumns matrix = lookupColumn matrix <$> [0..8]


allBlocks :: Sudoku -> V.Vector (Set Int)
allBlocks matrix =
  V.fromList $ Set.fromList <$> (concat $ verticalBlocksFrom matrix <$> [0,3..8])


lookupBlock :: Int -> Int -> Sudoku -> Set Int
lookupBlock x y matrix = allBlocks matrix V.! ((shift x) + 3* (shift y))
  where
    shift n | n < 0 || n > 8 = error "lookupBlock: Out of range."
            | n < 3          = 0
            | n < 6          = 1
            | otherwise      = 2


doneSudoku :: Sudoku -> Bool
doneSudoku matrix = let  check xs = sum xs == sum [1..9]
                         checklen l = length l == 9
                         checkNub xs = checklen (nub $ (filter (\x -> x /= 0)) xs)
                    in
                      all check (allRows matrix) &&
                      all check (allColumns matrix) &&
                      all check (allBlocks matrix) &&
                      all checkNub (allRows matrix) &&
                      all checkNub (allColumns matrix) &&
                      all checklen ((Set.filter (\x -> x /= 0)) <$> allBlocks matrix)


doneSudokuSomewhatUnsafe :: Sudoku -> Bool
doneSudokuSomewhatUnsafe matrix = let check xs = sum xs == sum [1..9]
                    in
                      all check (allRows matrix) &&
                      all check (allColumns matrix) &&
                      all check (allBlocks matrix)


safeRead :: Read a => String -> Maybe a
safeRead str = case reads str of
                 [(x, _)] -> Just x
                 _        -> Nothing


safeReadIntList :: String -> Maybe [Int]
safeReadIntList = sequence . fmap safeRead . words


loadFileToInts :: FilePath -> IO (Either IOException (Maybe [Int]))
loadFileToInts = try . fmap safeReadIntList . readFile


loadSudokuFile :: FilePath -> IO (Maybe (Matrix Int))
loadSudokuFile file =
  do eitherInts <- loadFileToInts file
     case eitherInts of
       Right (Just list) | length list == 81 -> -- A sudoku consists of exactly
                                                -- 81 integers (9*9 = 81).
                             return $ Just $ Matrix 9 9 (VU.fromList list)

       Left _ ->
         hPutStrLn stderr "Couldn't open file." >> return Nothing

       _      ->
         hPutStrLn stderr "Malformed input." >> return Nothing


nines :: Set Int
nines = Set.fromList [1..9]


lookupCandidates :: Int -> Int -> Matrix Int -> Set Int
lookupCandidates x y matrix = if lookup x y matrix /= 0
                              then Set.empty
                              else nines \\ foldr1 Set.union sets
  where
    sets = lookupBlock x y matrix :
           (Set.fromList <$> [lookupRow matrix y, lookupColumn matrix x])


allCandidatesWithCoords :: Sudoku -> [((Int, Int), Set Int)]
allCandidatesWithCoords matrix =
  filter (not . null . snd)
  [((x, y), lookupCandidates x y matrix) | x <- [0..8], y<- [0..8]]


allCandidatesSorted :: Sudoku -> [((Int, Int), Set Int)]
allCandidatesSorted = sorted . allCandidatesWithCoords
  where
    sorted :: [((Int, Int), Set Int)] -> [((Int, Int), Set Int)]
    sorted = sortBy (compare `on` length . snd) . filter ((> 0) . length . snd)


safeHead :: [a] -> [a]
safeHead (x:_) = [x]
safeHead _     = []


insertPivotCandidates :: Sudoku ->[Sudoku]
insertPivotCandidates matrix =
  (\x -> updateMatrixByCoords matrix [x]) <$> pivotCandidates
  where
    pivotCandidates :: [((Int, Int), Int)]
    pivotCandidates = concat ((\(c, set) -> sequence (c, Set.toList set) )
                              <$> (safeHead $ allCandidatesSorted matrix))


solve :: Sudoku -> [Sudoku]
solve = take 1 . track


track :: Sudoku -> [Sudoku]
track matrix =
  case doneSudokuSomewhatUnsafe matrix of
    True  -> [matrix]
    _     -> concat $ track <$> insertPivotCandidates matrix
