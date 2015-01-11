{-# LANGUAGE TemplateHaskell #-}

import System.Random
import GifStream

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

-- Stopping focus of the browser tab stops the animation. Reload the page to fix it.

data Position = Position
    { _x :: Int
    , _y :: Int
    }

data Move = MoveLeft | MoveRight | MoveUp | MoveDown deriving Eq

data State = State
    { _oldMove :: Move
    , _snake :: [Position]
    , _food :: Position
    }

makeLenses ''Position
makeLenses ''State

-- 30000 seems to be the lowest value that works in Firefox
-- 30 ms => 33 fps
delay = 100000 -- in µs
port = 5002

width = 32
height = 32
zoomScale = 4

main :: IO ()
main = server port delay logic

--logic :: IO () -> IO Char -> (Frame -> IO ()) -> IO ()
logic wait getInput sendFrame = evalStateT game initialState
  where
    game = do
      input <- lift $ getInput

      -- Generate new state
      let move = validateMove oldMove (charToMove input oldMove) -- Aufgabe 2

      let newSnake = moveSnake snake food move -- Aufgabe 2

      newFood <- moveFood newSnake food -- Aufgabe 3

      let frame = map (map (colorize newSnake newFood)) fieldPositions -- Aufgabe 1

      sendFrame (scale zoomScale frame)

      wait
      if checkGameOver newSnake -- Aufgabe 4
      then initialState >>= go
      else go (State move newSnake newFood)

initialState :: IO State
initialState = do
  let startSnake = [(15,15),(14,15)]
  let food = (28,28)
  return (State MoveRight startSnake food)

charToMove :: Char -> Move -> Move
charToMove c oldMove = case c of
  'w' -> MoveUp
  'a' -> MoveLeft
  's' -> MoveDown
  'd' -> MoveRight
  _   -> oldMove

scale :: Int -> Frame -> Frame
scale z frame = concatMap (replicate z) (map (concatMap (replicate z)) frame)

-- Aufgabe 1

fieldPositions :: [[Position]]
fieldPositions = splitEvery width [(x,y) | y <- [0..height-1], x <- [0..width-1]]

splitEvery :: Int -> [e] -> [[e]]
splitEvery i ls
  | length ys > i = xs : splitEvery i ys
  | otherwise     = [xs,ys]
  where (xs,ys) = splitAt i ls

colorize :: [Position] -> Position -> Position -> RGB
colorize snake food x
  | x `elem` snake = (3,3,3)
  | x == food      = (3,0,0)
  | otherwise      = (1,1,1)

-- Aufgabe 2

moveSnake :: [Position] -> Position -> Move -> [Position]
moveSnake xs@((x,y):_) food move = newHead : newTail
  where newHead = case move of
          MoveLeft  -> (x-1,y)
          MoveRight -> (x+1,y)
          MoveUp    -> (x,y-1)
          MoveDown  -> (x,y+1)
        newTail = if newHead == food then xs
                                     else init xs

validateMove :: Move -> Move -> Move
validateMove oldMove move = if opposite move == oldMove then oldMove else move

opposite :: Move -> Move
opposite MoveLeft  = MoveRight
opposite MoveRight = MoveLeft
opposite MoveUp    = MoveDown
opposite MoveDown  = MoveUp

-- Aufgabe 3

moveFood :: [Position] -> Position -> IO Position
moveFood (x:xs) food
  | x == food = getRandomOutside (x:xs)
  | otherwise = return food

getRandomOutside :: [Position] -> IO Position
getRandomOutside xs = do
  fx <- randomRIO (0, width  - 1)
  fy <- randomRIO (0, height - 1)

  if (fx,fy) `elem` xs
  then getRandomOutside xs
  else return (fx,fy)

-- Aufgabe 4

checkGameOver :: [Position] -> Bool
checkGameOver ((x,y):xs) = (x,y) `elem` xs
                        || x < 0 || x >= width
                        || y < 0 || y >= height
