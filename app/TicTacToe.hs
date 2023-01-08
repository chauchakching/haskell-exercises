module TicTacToe where

import Data.List (elemIndices, sortBy, sortOn)
import Data.Maybe (isJust)
import Data.Tree (Tree (Node))
import System.Random (randomRIO)

data Player = O | B | X deriving (Show, Eq, Ord)

type GameState = [Player]

initGame :: GameState
initGame = replicate 9 B

tictactoe :: IO ()
tictactoe = do
  putStrLn "start game"
  -- printGame initGame
  -- let game1 = move initGame X 5
  -- fmap printGame game1
  runGame initGame

runGame :: GameState -> IO ()
runGame game = do
  putStrLn "------------"
  putStrLn ""
  let p = turn game
  let p2 = enemy p
  if
      | wins game p -> putStrLn $ "-- Player " ++ show p ++ " wins! --"
      | wins game p2 -> putStrLn $ "-- Player " ++ show p2 ++ " wins! --"
      | gameEnd game -> putStrLn "-- DRAW --"
      | otherwise -> do
          -- updatedGame <- if p == O then randMove game p else return $ smartMove game
          putStrLn $ "player " ++ show p ++ " making a calculated move..."
          updatedGame <- return $ smartMove game
          printGame updatedGame
          runGame updatedGame

gameEnd :: GameState -> Bool
gameEnd = notElem B

win :: GameState -> Bool
win g = wins g O || wins g X

wins :: GameState -> Player -> Bool
wins game p = any matchAllPoss allWinPoss
 where
  allWinPoss = horizontalPoss ++ verticalPoss ++ diagonalPoss
  horizontalPoss = map (\i -> [i .. i + 2]) [0, 3, 6]
  verticalPoss = map (\i -> [i, i + 3, i + 6]) [0 .. 2]
  diagonalPoss = [[0, 4, 8], [2, 4, 6]]
  matchAllPoss = all (\i -> game !! i == p)

-- Use min-max to get optimal move
smartMove :: GameState -> GameState
smartMove g = snd $ (if turn g == X then last else head) $ sortOn fst nextMoves
 where
  nextMoves = branchesNodes $ minmaxTree $ prune 9 $ gametree g

minmaxTree :: Tree GameState -> Tree (Player, GameState)
minmaxTree (Node g [])
  | wins g X = Node (X, g) []
  | wins g O = Node (O, g) []
  | otherwise = Node (B, g) []
minmaxTree (Node g gTrees)
  | turn g == X = Node (maximum nextMovesScores, g) maxTrees
  | otherwise = Node (minimum nextMovesScores, g) maxTrees
 where
  nextMovesScores = [p | Node (p, _) _ <- maxTrees]
  maxTrees = map minmaxTree gTrees

gametree :: GameState -> Tree GameState
gametree g = Node g [gametree g' | g' <- moves g]

-- Possible moves of a game. No more move if someone won.
moves :: GameState -> [GameState]
moves g
  | win g = []
  | null indexes = []
  | otherwise = map (forceMove g p) indexes
 where
  p = turn g
  indexes = elemIndices B g

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

randMove :: GameState -> Player -> IO GameState
randMove game p = do
  let indexes = elemIndices B game
  case length indexes of
    0 -> pure game
    _ -> do
      i <- (indexes !!) <$> randomRIO (0, length indexes - 1)
      pure $ forceMove game p i

checkMove :: GameState -> Int -> Either String ()
checkMove game i
  | i < 0 || i >= 9 = Left "Invalid move: out of range"
  | game !! i /= B = Left "Invalid move: already occupied"
  | otherwise = Right ()

forceMove :: GameState -> Player -> Int -> GameState
forceMove game x i
  | i < 0 || i >= 9 = game
  | otherwise = updateIdx i x game

showPosition :: Player -> String
showPosition x = case x of
  O -> "O"
  X -> "X"
  _ -> " "

printGame :: GameState -> IO ()
printGame xs = do
  putStrLn $ showRow $ take 3 xs
  putStrLn $ "-┼-┼-"
  putStrLn $ showRow $ take 3 $ drop 3 $ xs
  putStrLn $ "-┼-┼-"
  putStrLn $ showRow $ take 3 $ drop 6 $ xs

turn :: GameState -> Player
turn g =
  if even (length $ filter (/= B) g)
    then O
    else X

showRow :: GameState -> String
showRow [a, b, c] = showPosition a ++ "|" ++ showPosition b ++ "|" ++ showPosition c
showRow x = error $ "invalid row to print: " ++ show x

enemy :: Player -> Player
enemy p = if p == O then X else O

updateIdx :: Int -> a -> [a] -> [a]
updateIdx i x xs
  | i < 0 || i >= length xs = xs
  | otherwise = take i xs ++ [x] ++ drop (i + 1) xs

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = take n xs : splitEvery n (drop n xs)

root :: Tree a -> a
root (Node a _) = a

branches :: Tree a -> [Tree a]
branches (Node _ xs) = xs

branchesNodes :: Tree a -> [a]
branchesNodes = map root . branches