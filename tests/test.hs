import Data.Foldable (traverse_)
import Data.List (intersperse)
import Data.Tree (Tree (Node), drawTree)
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@=?), (@?=))
import Text.Read (Lexeme (String))
import TicTacToe (GameState, Player (B, O, X), branches, gametree, initGame, moves, prune, showPosition, splitEvery)

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testGroup
        "splitEvery"
        [ testCase "splitEvery 3 [1 .. 9]" $ (splitEvery 3 [1 .. 9]) @?= [[1 .. 3], [4 .. 6], [7 .. 9]]
        ]
    , testGroup
        "gametree"
        [ testCase "empty: root has 9 sub-trees" $ (length $ branches $ prune 1 $ gametree $ initGame) @?= 9
        , testCase "3 moves: root has 6 sub-trees" $ (length $ branches $ prune 1 $ gametree [[O, O, B], [X, X, B], [O, X, B]]) @?= 3
        ]
    , testGroup
        "moves"
        [ testCase "empty board: 9 moves available" $ (length $ moves initGame) @?= 9
        ]
    ]

showGameTree :: Tree GameState -> String
showGameTree = drawTree . fmap showGameState
 where
  showGameState :: GameState -> String
  showGameState = intersperse ',' . concatMap showPosition . concat
