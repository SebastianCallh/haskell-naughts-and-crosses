import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Map as Map
import Data.List.Split
import Text.Read
import Data.List

type Grid = V.Vector (V.Vector Symbol)
type Row = V.Vector Symbol
type Pos = (Int, Int)
data Board = Board { size :: Int
                   , tiles :: Grid
                   }

data Symbol = O | X | Empty
  deriving Eq

instance Show Symbol where
  show O = "O"
  show X = "X"
  show Empty = " "

-- Assumes square matrix
transposeV :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
transposeV v
  | V.null $ V.head v = V.empty
  | otherwise = V.cons (fmap V.head v) $ transposeV $ fmap (V.drop 1) v

printState :: Board-> Symbol -> IO ()
printState board symbol = do
  putStr output where
    output = turnText ++ boardState ++ possibleMoves
    turnText = "It is " ++ show symbol ++ "'s turn\n"
    boardState = concatMap (++"\n") rows
    rows = fmap show $ tiles board
    possibleMoves = (show $ emptyTiles board) ++ "\n"

putSymbol :: Board -> Symbol -> Pos -> Board
putSymbol board symbol (rowI, colI) = Board (size board) tiles' where 
  tiles' = V.update boardTiles (V.singleton (colI, col'))
  col' = V.update (boardTiles ! colI) (V.singleton (rowI, symbol))
  boardTiles = tiles board

isWinner ::  Board -> Symbol -> Bool
isWinner board symbol =
  isVerticalVictory board symbol ||
  isHorizontalVictory board symbol ||
  isDiagonalVictory board symbol

isVerticalVictory :: Board -> Symbol -> Bool
isVerticalVictory board symbol = V.any (isRowVictory symbol) (transposeV (tiles board))

isHorizontalVictory :: Board -> Symbol -> Bool 
isHorizontalVictory board symbol = V.any (isRowVictory symbol) (tiles board)

isDiagonalVictory :: Board -> Symbol -> Bool 
isDiagonalVictory board symbol =
  all (== symbol) [ts ! x ! (s' - x) | x <- [0..s']] ||
  all (== symbol) [ts ! x ! x | x <- [0..s']] where
  ts = tiles board
  s' = size board - 1

isRowVictory :: Symbol -> Row-> Bool 
isRowVictory symbol row = all (== symbol) row

takeTurn :: Board -> Symbol -> IO Board
takeTurn board symbol = do
  putStrLn "Enter where you want to place your symbol on the format 'x y'"
  input <- getPlayerMove
  case input of
    Just pos -> return $ putSymbol board symbol pos
    Nothing -> takeTurn board symbol

-- Assumes at least 2 words as input 
getPlayerMove :: IO (Maybe Pos)
getPlayerMove = do
  [i, j] <- words <$> getLine
  case (readMaybe i, readMaybe j) of
    (Just x, Just y) -> return $ Just (x, y)
    _ -> return Nothing

emptyTiles :: Board -> V.Vector (V.Vector Int)
emptyTiles = fmap (V.elemIndices Empty) . tiles

getWinner :: Board -> Maybe Symbol
getWinner board 
  | isWinner board O = Just O
  | isWinner board X = Just X
  | otherwise = Nothing

victory :: Board -> Symbol -> IO (Symbol)
victory board symbol = do
  printState board Empty
  putStrLn (show symbol ++ " is winner!")
  return symbol

play :: Board -> IO (Symbol)
play board = do
  printState board O
  board' <- takeTurn board O
  case getWinner board' of 
    Just s -> victory board' s 
    Nothing -> do
      printState board' X
      board'' <- takeTurn board' X
      case getWinner board'' of
        Just s -> victory board'' s
        Nothing -> do
          play board''

main :: IO (Symbol)
main = do
  let board = Board size tiles where
        size = 3
        tiles = V.fromList [col | _ <- [1..size]]
        col = V.fromList [Empty | _ <- [1..size]]
  play board
