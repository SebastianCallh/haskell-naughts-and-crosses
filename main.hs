import qualified Data.Vector.Unboxed as V
import Data.List.Split
import Data.List

type Pos = (Int, Int)
type Board = [Symbol]

data Symbol = O | X | Empty
instance Show Symbol where
  show O = "O"
  show X = "X"
  show Empty = " "

size = 3

printState :: Board-> Symbol -> IO ()
printState board symbol = do
  putStr output where
    output = "It is " ++ show symbol ++ "s turn\n" ++ line ++ (foldr1 (++) charList) ++ "\n" ++ line
    line = replicate 5 '-' ++ "\n"
    charList = concat . intersperse ["\n"] . applyBorder . chunksOf size $ map show board
    applyBorder = map (\r -> "|" : r ++ ["|"])

put :: Symbol -> Pos -> Board -> Board
put s (x, y) b = xs ++ [s] ++ ys where
  (xs, _:ys) = splitAt pos b
  pos = x + y * size

play :: Board -> IO ()
play board = do
  printState board O
  board' <- takeTurn board O
  printState board' X
  board'' <- takeTurn board' X
  play board''

takeTurn :: Board -> Symbol -> IO Board
takeTurn board symbol = do
  [x, y] <- words <$> getLine
  return $ put symbol (read x, read y) board

emptyPos :: Board -> [Pos]
emptyPos board = filter ((==) Empty) board

main :: IO ()
main = do
  let board = replicate (size * size) Empty
  play board
