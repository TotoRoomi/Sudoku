module Sudoku where
import Data.List(intersperse, transpose)
import Test.QuickCheck
import Data.Char(digitToInt)
import Data.Maybe(fromJust)

-------------------------------------------------------------------------

data Sudoku = Sudoku [[Maybe Int]]
  deriving (Show, Eq)

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

cols :: Sudoku -> [[Maybe Int]]
cols sud = transpose (rows sud)

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle

isSudoku :: Sudoku -> Bool
isSudoku sud = (and (go $ rows sud)) && (length (rows sud) == 9) && (and $ map and ((map . map) go2 $ rows sud))
  where
    go list = map (\x -> length x == 9) list
    go2 m = case m of
      Just n -> n >=1 && n <= 9
      Nothing -> True
      otherwise -> False

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = and . map and $ (map . map) go (rows sud)
  where
    go m = case m of
      Just n -> True
      Nothing -> False

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = printHelper $ rows sud

showMaybeInt :: Maybe Int -> String
showMaybeInt ma = case ma of
    Nothing -> "."
    Just a -> show a

printHelper :: [[Maybe Int]] -> IO ()
printHelper list = mapM_ putStr $ appendToEnd "\n" $ intersperse "\n" $ map unwords $ (map . map) showMaybeInt (list) where
  appendToEnd sym a = a ++ [sym] -- slow but idk

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do
  sud <- readFile path
  let listOfStrings = lines sud
  let sudokuList = (map . map) go listOfStrings
  return $ Sudoku sudokuList
  where
    go c = case c of
      '.' -> Nothing
      otherwise -> Just (digitToInt c)

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9,pure Nothing),(1,randomJust)] where
  randomJust = elements [Just m | m<-[1..9]]


-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do
      rows <- sequence [sequence [cell | j <- [1 .. 9]] | i <- [1 .. 9]]
      return $ Sudoku rows

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-------------------------------------------------------------------------

-- D

type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock block = listCondition block True notBlockMember where
  notBlockMember a blockRest = case a of
    Nothing -> True
    Just a -> not (elem (Just a) blockRest)

listCondition :: [a] -> Bool -> (a->[a]->Bool) -> Bool
listCondition _ False f = False
listCondition [] bool f = bool
listCondition (x:xs) bool f = listCondition xs (f x xs) f

blocks :: Sudoku -> [Block]
blocks sud = [b | b <- (rows sud)] ++ [b | b <- (cols sud)] ++ (squareBlocks $ rows sud)

-- .. work in progress.. makes all permutations of those three
blocks' :: Sudoku -> [Block]
blocks' sud = do
  a <- rows sud
  b <- cols sud
  c <- squareBlocks $ rows sud
  [a] ++ [b] ++ [c]

printBlocks :: Sudoku -> IO ()
printBlocks sud = printHelper $ drop 18  $ blocks sud

----- big ol' mess
-- concats the square groups into a list of [Block], generalised tho
squareBlocks :: [[a]] -> [[a]]
squareBlocks list = concat $ (map . map) concat $ transposedThrees list

-- split rows into groups of 3 then transpose and split again into groups of 3
transposedThrees :: [[a]] -> [[[[a]]]]
transposedThrees list = map split3 (transpose $ map split3 list)

-- assumes list lengths are multiples of 9
-- chunksOf 3 from Data.List.split does the same, but fun exercise
split3 :: [a] -> [[a]]
split3 list = go list [] where
  go [] accum = accum
  go list accum = go (drop 3 list) (accum ++ [take 3 list])

split3' :: [a] -> [[a]]
split3' [] = []
split3' list = a':a'':a''':(split3' rest) where
  a' = take 3 list
  a'' = take 3 (drop 3 list)
  a''' = take 3 (drop 6 list)
  rest = drop 9 list
------

prop_sudoku_blocks :: Sudoku -> Bool
prop_sudoku_blocks sud = length (blocks sud) == (27) && listCondition (map length (blocks sud)) True only9 where
  only9 x _ = x == 9

isOkay :: Sudoku -> Bool
isOkay sud = and . (map  isOkayBlock) $ blocks sud

--- E
-- (row, column), 0 indexed
type Pos = (Int,Int)

blank :: Sudoku -> Pos
blank sud = nothingIndex matrix where
  matrix = zip [0..8] $ map (zip [0..8]) (rows sud) -- [(rowIndex, [(columnIndex,Maybe Int)])]

---- Very imperative style.. try a foldWhile or smthg instead
---- Also repeated code
-- assumes there is at least 1 Nothing somewhere
nothingIndex :: [(Int, [(Int,Maybe Int)])] -> (Int, Int)
nothingIndex (x:xs) =
  if r < 0 && c < 0 then
    nothingIndex xs
    else (r,c)
  where
    (r,c) = findNothing x

-- impossible value if row does not contain a Nothing
findNothing :: (Int, [(Int,Maybe Int)]) -> (Int, Int)
findNothing (r, []) = (-1,-1)
findNothing (r, (x:xs)) =
  if (colNothing x) < 0 then
    findNothing (r, xs)
    else (r, (c))
  where
    c = fst x

-- impossible value if it's not a Nothing
colNothing :: (Int, Maybe Int) -> Int
colNothing (n, m) =
  case m of
    Nothing -> n
    otherwise -> -1

prop_empty_cell :: Sudoku -> Bool
prop_empty_cell sud = ((s !! r) !! c) == Nothing
  where
    s = rows sud
    (r,c) = blank sud

-- assumes valid index and will never reach []
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) (x:xs) (0,val) = (val:xs)
(!!=) (x:xs) (n,val) = (x: ( (!!=) xs (n-1,val)) )

prop_valid_index :: [a] -> (Int, a) -> Bool
prop_valid_index list (n,val) = n <= ((length list)-1) && n >= 0

prop_value_changed :: [Maybe Int] -> (Int,Maybe Int) -> Bool
prop_value_changed row (n,val) = (row' !! n) == val where
  row' = (!!=) row (n,val)


update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sud (r,c) val = Sudoku s' where
  s' = (!!=) s (r, row')  -- replace row
  row' = (!!=) row (c, val) -- replace value in row
  row = s !! r
  s = rows sud

-- consider generating random (Int, Maybe Int) ..... so I can test w quickCheck
prop_updated :: Sudoku -> (Int,Int) -> Maybe Int -> Bool
prop_updated sud (r,c) val = not (( ((newSud) !! r) !! c ) /= ( ((rows sud) !! r) !! c )) where
  newSud = rows $ update sud (r,c) val



solve :: Sudoku -> Maybe Sudoku
solve s
  | not (isOkay s) = Nothing -- There's a violation in s
  | isSolved s = Just s -- s is already solved
  | otherwise = pickASolution possibleSolutions
  where
    nineUpdatedSuds = [update s pos (Just (x)) | x <- [1..9]]  :: [Sudoku]
    possibleSolutions = [solve s' | s' <- nineUpdatedSuds]
    pos = blank s



pickASolution :: [Maybe Sudoku] -> Maybe Sudoku
pickASolution [] = Nothing
pickASolution (x:xs)
  | x == Nothing = pickASolution xs
  | isOkay (fromJust x) = x


readAndSolve :: FilePath -> IO ()
readAndSolve path = do
  sud <- readSudoku path
  printSudoku (fromJust (solve sud))



-- og: original, sol: solved
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf og sol = (isOkay sol) && (isSolved sol) && (noChanges og sol)

noChanges :: Sudoku -> Sudoku -> Bool
noChanges og sol = compareSudokus (rows og) (rows sol) True

-- eerily similar to compareRows...
compareSudokus :: [[Maybe Int]] -> [[Maybe Int]] -> Bool -> Bool
compareSudokus _ _ False = False
compareSudokus [] _ noChange = noChange
compareSudokus (a:as) (b:bs) noChange = compareSudokus as bs (compareRows a b True)


compareRows :: [Maybe Int] -> [Maybe Int] -> Bool -> Bool
compareRows _ _ False = False
compareRows [] _ noChange = noChange
compareRows (a:as) (b:bs) noChange =
  case a of
    Nothing -> compareRows as bs noChange
    otherwise -> compareRows as bs (a == b)




prop_SolveSound :: Sudoku -> Property
prop_SolveSound = undefined


example :: Sudoku
example =
  Sudoku
    [ [Just 3, Just 6, Nothing, Nothing, Just 7, Just 1, Just 2, Nothing, Nothing],
      [Nothing, Just 5, Nothing, Nothing, Nothing, Nothing, Just 1, Just 8, Nothing],
      [Nothing, Nothing, Just 9, Just 2, Nothing, Just 4, Just 7, Nothing, Nothing],
      [Nothing, Nothing, Nothing, Nothing, Just 1, Just 3, Nothing, Just 2, Just 8],
      [Just 4, Nothing, Nothing, Just 5, Nothing, Just 2, Nothing, Nothing, Just 9],
      [Just 2, Just 7, Nothing, Just 4, Just 6, Nothing, Nothing, Nothing, Nothing],
      [Nothing, Nothing, Just 5, Just 3, Nothing, Just 8, Just 9, Nothing, Nothing],
      [Nothing, Just 8, Just 3, Nothing, Nothing, Nothing, Nothing, Just 6, Nothing],
      [Nothing, Nothing, Just 7, Just 6, Just 9, Nothing, Nothing, Just 4, Just 3]
    ]

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

fullWrongSudoku :: Sudoku
fullWrongSudoku = Sudoku $ replicate 9 $ replicate 9 (Just 1)

failTest :: Sudoku
failTest = Sudoku [[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Just 4,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Just 4,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Just 9,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Just 1,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Just 6,Nothing,Nothing,Just 1,Nothing,Nothing,Nothing,Just 2,Just 6]]
