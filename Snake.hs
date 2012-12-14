{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Control.Concurrent
import System.Random
import Data.IORef
import Control.Concurrent.MVar

import MSignal
import Net

-- Stopping focus of the browser tab stops the animation.
-- Reload the page to fix it.

-- 30000 seems to be the lowest value that works in Firefox
-- 30 ms => 33 fps
delay = 100000 -- in µs

port = 5002

main :: IO ()
main = do
    wait <- getMetronome
    getAction <- inputGetter
    server port delay (logic wait getAction)

width = 32
height = 32
zoom = 4

data Action = L | R | U | D deriving Eq

type Position = (Int,Int)

moveSnake :: [Position] -> Position -> Action -> [Position]
moveSnake xs@((x,y):_) food action = newHead : newTail
  where newHead = case action of
          L -> (x-1,y)
          R -> (x+1,y)
          U -> (x,y-1)
          D -> (x,y+1)
        newTail = if newHead == food then xs
                                     else init xs

moveFood :: [Position] -> Position -> IO Position
moveFood (x:xs) food
  | x == food = getRandomOutside (x:xs)
  | otherwise = return food

colorize :: [Position] -> Position -> Position -> RGB
colorize snake food x
  | x `elem` snake = (3,3,3)
  | x == food      = (3,0,0)
  | otherwise      = (1,1,1)

charToAction :: Char -> Action -> Action
charToAction c x = case c of
  'w' -> U
  'a' -> L
  's' -> D
  'd' -> R
  _   -> x

validateAction :: Action -> Action -> Action
validateAction oldAction action = if opposite action == oldAction then oldAction else action

getMetronome :: IO (IO ())
getMetronome = do
    var <- newMVar ()
    forkIO $ forever $ do
        threadDelay delay
        putMVar var ()
    return $ takeMVar var

inputGetter :: IO (IO Char)
inputGetter = do
    inputRef <- newIORef 'd'
    forkIO $ forever $ do
        c <- getChar
        writeIORef inputRef c
    return $ readIORef inputRef


data State = State
    { oldAction :: Action
    , snake :: [Position]
    , food :: Position
    }

initialState :: IO State
initialState = do
    let startSnake = [(15,15),(14,15)]
    food <- getRandomOutside startSnake
    return $ State R startSnake food

logic :: IO () -> IO Char -> FrameSignal -> IO ()
logic wait getInput frameSignal = initialState >>= go
  where
    go (State {..}) = do
      input <- getInput
      let action = validateAction oldAction $ charToAction input oldAction

      let newSnake = moveSnake snake food action

      newFood <- moveFood newSnake food

      let frame = map (map (colorize newSnake newFood)) imgPositions

      sendMSignal frameSignal $ scale zoom frame

      wait
      if checkGameOver newSnake
      then do
         initialState >>= go
      else do
         go (State action newSnake newFood)


checkGameOver :: [Position] -> Bool
checkGameOver ((x,y):xs) = ((x,y) `elem` xs
                          || x < 0 || x >= width
                          || y < 0 || y >= height)

opposite :: Action -> Action
opposite L = R
opposite R = L
opposite U = D
opposite D = U

getRandomOutside :: [Position] -> IO Position
getRandomOutside xs = do
  fx <- randomRIO (0, width  - 1)
  fy <- randomRIO (0, height - 1)

  if (fx,fy) `elem` xs
  then getRandomOutside xs
  else return (fx,fy)

scale :: Int -> Frame -> Frame
scale z frame = concatMap (replicate z) $ map (concatMap (replicate z)) frame

imgPositions :: [[Position]]
imgPositions = splitEvery width [(x,y) | y <- [0..height-1], x <- [0..width-1]]

splitEvery :: Int -> [e] -> [[e]]
splitEvery i ls
  | length ys > i = xs : splitEvery i ys
  | otherwise     = [xs,ys]
  where (xs,ys) = splitAt i ls

img :: Frame
img = replicate height $ replicate width (0,0,0)
