module TicTacToe where

import Data.List (any, findIndices)
import Safe (headMay)
import System.Random (randomRIO)

data PositionState = O | X | E
  deriving (Show, Eq)

type GameState = [PositionState]

initGame :: GameState
initGame = replicate 9 E

tictactoe :: IO ()
tictactoe = do
  putStrLn "start game"
  -- printGame initGame
  -- let game1 = move initGame X 5
  -- fmap printGame game1
  randomGame O initGame

randomGame :: PositionState -> GameState -> IO ()
randomGame p game = do
  putStrLn "------------"
  printGame game
  putStrLn ""
  if win game p
    then putStrLn $ "-- Player " ++ show p ++ " wins! --"
    else
      if win game (enemy p)
        then putStrLn $ "-- Player " ++ (show $ enemy p) ++ " wins! --"
        else do
          if gameEnd game
            then putStrLn "-- DRAW --"
            else do
              randomGame (enemy p) =<< randMove game p

gameEnd :: GameState -> Bool
gameEnd = all (/= E)

win :: GameState -> PositionState -> Bool
win game p = any matchAllPoss allWinPoss
 where
  allWinPoss = horizontalPoss ++ verticalPoss ++ diagonalPoss
  horizontalPoss = map (\i -> [i .. i + 2]) [0, 3, 6]
  verticalPoss = map (\i -> [i, i + 3, i + 6]) [0 .. 2]
  diagonalPoss = [[0, 4, 8], [2, 4, 6]]
  matchAllPoss = all (\i -> game !! i == p)

randMove :: GameState -> PositionState -> IO GameState
randMove game p = do
  let indexes = findIndices (== E) game
  case length indexes of
    0 -> pure game
    otherwise -> do
      i <- (indexes !!) <$> randomRIO (0, length indexes - 1)
      pure $ forceMove game p i

checkMove :: GameState -> PositionState -> Int -> Either String ()
checkMove game x i
  | i < 0 || i >= 9 = Left "Invalid move: out of range"
  | game !! i /= E = Left "Invalid move: already occupied"
  | otherwise = Right ()

forceMove :: GameState -> PositionState -> Int -> GameState
forceMove game x i
  | i < 0 || i >= 9 = game
  | game !! i /= E = game
  | otherwise = updateIdx i x game

showPosition :: PositionState -> String
showPosition x = case x of
  O -> "O"
  X -> "X"
  otherwise -> " "

printGame :: GameState -> IO ()
printGame xs = do
  putStrLn $ showRow $ take 3 xs
  putStrLn $ "-┼-┼-"
  putStrLn $ showRow $ take 3 $ drop 3 $ xs
  putStrLn $ "-┼-┼-"
  putStrLn $ showRow $ take 3 $ drop 6 $ xs

showRow :: [PositionState] -> String
showRow (a : b : c : []) = showPosition a ++ "|" ++ showPosition b ++ "|" ++ showPosition c

enemy :: PositionState -> PositionState
enemy p = if p == O then X else O

updateIdx :: Int -> a -> [a] -> [a]
updateIdx i x xs
  | i < 0 || i >= length xs = xs
  | otherwise = (take i xs) ++ [x] ++ (drop (i + 1) xs)

splitEvery :: Int -> [a] -> [[a]]
splitEvery n [] = []
splitEvery n xs = (take n xs) : (splitEvery n $ drop n xs)