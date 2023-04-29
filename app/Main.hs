module Main where

import Config (Size, initAliveCellRate, refleshInterval, size)
import Control.Concurrent (threadDelay)
import Data.List
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Random

type Cell = (Bool, Int, Int)

type World = [Cell]

main :: IO ()
main = do
  world <- generateWorld size
  loop world 0
  where
    loop :: World -> Int -> IO ()
    loop world generation = do
      clearScreen
      setCursorPosition 0 0
      putStrLn $ "Generation Count: " ++ show generation
      printWorld size world
      threadDelay refleshInterval
      loop (advance world) $ generation + 1

isAlive :: Cell -> Bool
isAlive (alive, _, _) = alive

advance :: World -> World
advance world = map (\(alive, x, y) -> (willBeAlive world (alive, x, y), x, y)) world

willBeAlive :: World -> Cell -> Bool
willBeAlive world (alive, x, y) =
  if alive
    then aliveCount == 2 || aliveCount == 3
    else aliveCount == 3
  where
    aliveCount = countAroundAliveCell world (alive, x, y)

countAroundAliveCell :: World -> Cell -> Int
countAroundAliveCell world = length . filter id . map (`elem` world) . aroundAliveCell

aroundAliveCell :: Cell -> [Cell]
aroundAliveCell (_, x, y) =
  [ (True, x - 1, y - 1),
    (True, x - 1, y),
    (True, x - 1, y + 1),
    (True, x, y - 1),
    (True, x, y + 1),
    (True, x + 1, y - 1),
    (True, x + 1, y),
    (True, x + 1, y + 1)
  ]

generateWorld :: Size -> IO World
generateWorld (width, height) = do
  world <- mapM (\y -> mapM (`generateCell` y) [1 .. width]) [1 .. height]
  return (concat world)

generateCell :: Int -> Int -> IO Cell
generateCell x y = do
  alive <- getBool
  return (alive, x, y)

getBool :: IO Bool
getBool = do
  x <- randomRIO (0, 1) :: IO Double
  return $ x < initAliveCellRate

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

printWorld :: Size -> World -> IO ()
printWorld (width, height) world = do
  let display = map (\cell -> if isAlive cell then '■' else '□') world
  let displayWithNewline = intercalate "\n" $ chunksOf (fst size) display
  putStrLn displayWithNewline
