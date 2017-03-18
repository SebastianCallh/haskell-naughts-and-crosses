import qualified Data.Map as Map
import Data.List.Split
import Text.Read
import Data.List

type Pos = (Int, Int)
data Board = Board { size :: Int
                   , fields :: Map.Map Pos Symbol
                   }

data Symbol = O | X | Empty
  deriving Eq

instance Show Symbol where
  show O = "O"
  show X = "X"
  show Empty = " "

printState :: Board-> Symbol -> IO ()
printState board symbol = do
  putStr output where
    output = turnText ++ line ++ boardState ++ line ++ possibleMoves
    turnText = "It is " ++ show symbol ++ "'s turn\n"
    line = replicate 5 '-' ++ "\n"
    boardState = foldr1 (++) $ format . chunksOf (size board) $ symbols
    symbols = map (show . snd) $ Map.toList $ fields board
    format = concatMap (\r -> "|" : r ++ ["|\n"])
    possibleMoves = (show $ Map.keys $ emptyFields board) ++ "\n"

putSymbol :: Board -> Symbol -> Pos -> Board
putSymbol board symbol pos = Board (size board) newFields where
  newFields = Map.adjust (\_ -> symbol) pos $ fields board

isWinner ::  Board -> Symbol -> Bool
isWinner board symbol =
  isVerticalVictory board symbol ||
  isHorizontalVictory symbols ||
  isDiagonalVictory symbols where
  symbols = Map.filter (== symbol) $ fields board

isVerticalVictory :: Board -> Symbol -> Bool
isVerticalVictory board symbol = any (isRowVictory board symbol) [1..size board]

isRowVictory board symbol row = all isSameSymbol [1..size board] where
  isSameSymbol col = Map.lookup (col, row) (fields board) == Just symbol
  
isHorizontalVictory :: Map.Map Pos Symbol -> Bool
isHorizontalVictory symbols = False

isDiagonalVictory :: Map.Map Pos Symbol -> Bool
isDiagonalVictory symbols = False

takeTurn :: Board -> Symbol -> IO Board
takeTurn board symbol = do
  putStrLn "Enter where you want to place your symbol on the format 'x y'"
  pos <- getPlayerMove
  case pos of
    Nothing -> takeTurn board symbol
    Just pos ->
      if pos `Map.member` (emptyFields board)
      then return $ putSymbol board symbol pos
      else takeTurn board symbol

-- Assumes at least 2 words as input 
getPlayerMove :: IO (Maybe Pos)
getPlayerMove = do
  [i, j] <- words <$> getLine
  case (readMaybe i, readMaybe j) of
    (Just x, Just y) -> return $ Just (x, y)
    _ -> return Nothing

emptyFields :: Board -> Map.Map Pos Symbol
emptyFields = Map.filter (== Empty) . fields 

getWinner :: Board -> Maybe Symbol
getWinner board 
  | isWinner board O = Just O
  | isWinner board X = Just X
  | otherwise = Nothing
  
play :: Board -> IO ()
play board = do
  printState board O
  let winner = getWinner board
  case winner of 
    Just s -> putStrLn (show s ++ " is winner!")
    Nothing -> do
      board' <- takeTurn board O
      printState board' X
      board'' <- takeTurn board' X
      play board''

main :: IO ()
main = do
  let board = Board size fields where
        size = 3
        fields = Map.fromList [(pos, Empty) | pos <- positions]
        positions = [(x, y) | x <- [1..size], y <- [1..size]]
  play board
