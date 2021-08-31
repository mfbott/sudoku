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


data Matrix a = Matrix !Int !Int !(VU.Vector a) deriving (Eq, Ord)

type Sudoku = Matrix Int


indexOf :: Matrix a -> Int -> Int -> Int
indexOf (Matrix _ column _) x y = x + y * column


lookup :: VU.Unbox a => Int -> Int -> Matrix a -> a
lookup x y m@(Matrix _ _ v) = (VU.!) v $ indexOf m x y


updateMatrix :: VU.Unbox a => Matrix a -> [(Int, a)] -> Matrix a
updateMatrix (Matrix rows columns v) xs = Matrix rows columns (v VU.// xs)


updateMatrixSingleton :: VU.Unbox a => Matrix a -> (Int, a) -> Matrix a
updateMatrixSingleton matrix x = updateMatrix matrix [x]


updateMatrixByCoords :: VU.Unbox a => Matrix a -> [((Int, Int), a)] -> Matrix a
updateMatrixByCoords matrix = updateMatrix matrix . fmap
  (\(tupl, x) -> ( (uncurry . indexOf) matrix tupl, x))


updateMatrixByCoord :: VU.Unbox a => Matrix a -> ((Int, Int), a) -> Matrix a
updateMatrixByCoord matrix ((x, y), element) =
  updateMatrixSingleton matrix (coords, element)
  where
    coords = indexOf matrix x y


lookupRow :: VU.Unbox a => Matrix a -> Int -> [a]
lookupRow m@(Matrix _ columns _) y = [lookup x y m | x <- [0..columns-1]]


lookupColumn :: VU.Unbox a => Matrix a -> Int -> [a]
lookupColumn m@(Matrix rows _ _) x = [lookup x y m | y <- [0..rows-1]]


instance (Show a, VU.Unbox a) => Show (Matrix a) where
  show m@(Matrix rows _ _) =
    intercalate "\n" $ show <$> [lookupRow m y | y <- [0..rows-1]]


emptySudoku :: Matrix Int
emptySudoku = Matrix 9 9 (VU.replicate 81 0)


testMatrix :: Matrix Int
testMatrix = Matrix 9 9 (VU.fromList [1..81])


firstVerticalBlockFrom :: Matrix Int -> Int -> ([Int],[[Int]])
firstVerticalBlockFrom matrix startRow = traverse threes [startRow..startRow+2]
  where
    threes = splitAt 3 . lookupRow matrix


blocks :: ([Int], [[Int]]) -> [[Int]]
blocks ([] , _)  = []
blocks (block, rest) =
  block : blocks (traverse (splitAt 3) rest)


verticalBlocksFrom :: Matrix Int -> Int -> [[Int]]
verticalBlocksFrom matrix startRow =
  blocks $ firstVerticalBlockFrom matrix startRow


allRows :: Matrix Int -> [[Int]]
allRows matrix = lookupRow matrix <$> [0..8]


allColumns :: Matrix Int -> [[Int]]
allColumns matrix = lookupColumn matrix <$> [0..8]


allBlocks :: Matrix Int -> V.Vector (Set Int)
allBlocks matrix =
  V.fromList $ Set.fromList <$> (concat $ verticalBlocksFrom matrix <$> [0,3..8])


lookupBlock :: Int -> Int -> Matrix Int -> Set Int
lookupBlock x y matrix = allBlocks matrix V.! ((shift x) + 3* (shift y))
  where
    shift n | n < 0 || n > 8 = error "lookupBlock: Out of range."
            | n < 3          = 0
            | n < 6          = 1
            | otherwise      = 2


doneSudoku :: Matrix Int -> Bool
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


doneSudokuSomewhatUnsafe :: Matrix Int -> Bool
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
safeReadIntList str = sequence $ safeRead <$> words str


loadFileToInts :: FilePath -> IO (Either IOException (Maybe [Int]))
loadFileToInts file = try $ safeReadIntList <$> readFile file


loadSudokuFile :: FilePath -> IO (Maybe (Matrix Int))
loadSudokuFile file =
  do eitherInts <- loadFileToInts file
     case eitherInts of
       Right (Just list) | length list == 81 ->
                             return $ Just $ Matrix 9 9 (VU.fromList list)

       Left _ ->
         hPutStrLn stderr "Couldn't open file." >> return Nothing

       _      ->
         hPutStrLn stderr "Malformed input." >> return Nothing


nines :: Set Int
nines = (Set.fromList [1..9])


lookupCandidates :: Int -> Int -> Matrix Int -> Set Int
lookupCandidates x y matrix = if lookup x y matrix /= 0
                              then Set.empty
                              else nines \\ foldr1 Set.union sets
  where
    sets = lookupBlock x y matrix :
           (Set.fromList <$> [lookupRow matrix y, lookupColumn matrix x])


lookupCandidatesExcept :: Int -> Int -> Matrix Int ->  V.Vector (Set Int)  -> Set Int
lookupCandidatesExcept x y matrix except = if lookup x y matrix /= 0
                              then Set.empty
                              else (nines \\ foldr1 Set.union sets) \\ (except V.! (indexOf matrix x y))
  where
    sets = lookupBlock x y matrix :
           (Set.fromList <$> [lookupRow matrix y, lookupColumn matrix x])


allCandidatesWithCoords :: Matrix Int -> [((Int, Int), Set Int)]
allCandidatesWithCoords matrix = filter (not . null . snd) [((x, y), lookupCandidates x y matrix) | x <- [0..8], y<- [0..8]]


allCandidatesWithCoordsExcept :: V.Vector (Set Int) -> Sudoku -> [((Int, Int), Set Int)]
allCandidatesWithCoordsExcept except matrix =
  filter (not . null . snd) [((x, y), lookupCandidatesExcept x y matrix except) | x <- [0..8], y<- [0..8]]


covers :: Sudoku -> [((Int, Int), Set Int)]
covers = filter (\(_, set) -> length set == 1) . allCandidatesWithCoords


insertCovers :: Matrix Int -> Matrix Int
insertCovers matrix = updateMatrixByCoords matrix $ f <$> covers matrix
  where
    f (c, set) = (c, head $ Set.toList set) -- "Set.toList set" is guaranteed to
                                            -- be non-empty. However, it might
                                            -- be a good idea refactor this
                                            -- regardless, just to avoid the
                                            -- "head" function altogether.


insertCoversSpeculative :: V.Vector (Set Int) -> Matrix Int -> Matrix Int
insertCoversSpeculative except matrix = updateMatrixByCoords matrix $ f <$> covers_
  where
    covers_ = filter (\(_, set) -> length set == 1) $ allCandidatesWithCoordsExcept except matrix
    f (c, set) = (c, head $ Set.toList set) -- See comment above.


ordCandidatesExcept :: V.Vector (Set Int) -> Sudoku -> [((Int, Int), Set Int)]
ordCandidatesExcept except = sorted . (allCandidatesWithCoordsExcept except)
  where
    sorted :: [((Int, Int), Set Int)] -> [((Int, Int), Set Int)]
    sorted = sortBy (compare `on` length . snd) . filter ((> 0) . length . snd)


ordCandidatesExcept' :: V.Vector (Set Int) -> Sudoku -> [[((Int, Int), Int)]]
ordCandidatesExcept' except matrix =
  sequence <$> ((fmap . fmap) Set.toList $ ordCandidatesExcept except matrix)


type Guess  = ((Int, Int), Int, Sudoku)

insertGuess :: Guess -> (Sudoku, Guess)
insertGuess guess@(coordinates, value, matrix) = (matrix', guess)
  where
    matrix' = updateMatrixByCoord matrix (coordinates, value)

insertGuessesBySet :: Sudoku -> [((Int, Int), Set Int)] -> [Sudoku]
insertGuessesBySet matrix = concat . fmap f
  where
    f (coords, set) = fst .
      (\v -> insertGuess (coords, v, matrix)) <$> (Set.toList set)


solve :: Sudoku -> [Sudoku]
solve matrix = take 1 $ track except matrix
  where
    except = V.fromList $ take 81 $ repeat Set.empty


track :: V.Vector (Set Int) -> Sudoku -> [Sudoku]
track except matrix =
  case doneSudokuSomewhatUnsafe matrix of
    True             -> [matrix]

    _   | null cover -> if null exceptList then []
                        else
                          concat
                             ((\ex -> track ex $
                                 insertCoversSpeculative ex matrix) <$> exceptList)

    _                -> track except $ insertCovers matrix

  where
    cover      = insertGuessesBySet matrix $ covers matrix

    exceptList :: [V.Vector (Set Int)]
    exceptList = exceptListGo except $ concat $ ordCandidatesExcept' except matrix

    exceptListGo :: V.Vector (Set Int) -> [((Int, Int), Int)] -> [V.Vector (Set Int)]
    exceptListGo _    []                      = []
    exceptListGo excp (((x,y), candidate):xs) =
      (excp V.// [(index, Set.insert candidate set)]) : exceptListGo excp xs

      where
        index  = indexOf matrix x y
        set    = excp V.! index
