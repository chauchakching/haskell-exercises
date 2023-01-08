module TicTacToe where

import Data.List (intersperse, sortOn, transpose)
import Data.Maybe (fromMaybe)
import Data.Tree (Tree (Node))
import System.Random (randomRIO)

data Player = O | B | X deriving (Show, Eq, Ord)

type GameState = [[Player]]

gameSize :: Int
gameSize = 3

initGame :: GameState
initGame = replicate gameSize $ replicate gameSize B

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
gameEnd = all (notElem B)

win :: GameState -> Bool
win g = wins g O || wins g X

wins :: GameState -> Player -> Bool
wins game p = any line (rows ++ cols ++ diags)
 where
  line = all (== p)
  rows = game
  cols = transpose game
  diags = [diag game, diag (transpose game)]

diag :: GameState -> [Player]
diag g = [g !! n !! n | n <- [0 .. gameSize - 1]]

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
  | otherwise = gs
 where
  p = turn g
  gs :: [GameState]
  gs = [updateIdx i (updateIdx j p $ g !! i) g | i <- [0 .. length g - 1], j <- [0 .. length (head g) - 1], (g !! i) !! j == B]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

randMove :: GameState -> IO GameState
randMove game = do
  fromMaybe game <$> randItem (moves game)

showPosition :: Player -> String
showPosition x = case x of
  O -> "O"
  X -> "X"
  _ -> " "

printGame :: GameState -> IO ()
printGame xs = do
  let middleS = init $ concat (replicate (length $ head xs) "-┼")
  mapM_ putStrLn $ intersperse middleS $ map showRow xs

turn :: GameState -> Player
turn g =
  if even (length $ filter (/= B) $ concat g)
    then O
    else X

showRow :: [Player] -> String
showRow xs = intercalate "|" $ map showPosition xs

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

randItem :: [a] -> IO (Maybe a)
randItem [] = pure Nothing
randItem xs = do
  i <- randomRIO (0, length xs - 1)
  pure $ Just $ xs !! i