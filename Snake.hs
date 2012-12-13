import Control.Monad
import Control.Concurrent
import System.Random
import Data.IORef

import MSignal
import Net

-- Stopping focus of the browser tab stops the animation.
-- Reload the page to fix it.

-- 30000 seems to be the lowest value that works in Firefox
-- 30 ms => 33 fps
delay = 100000 -- in µs

port = 5002

main :: IO ()
main = server port delay logic

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
        newTail
          | newHead == food = xs
          | otherwise  = init xs

moveFood :: [Position] -> Position -> IO Position
moveFood (x:xs) food
  | x == food = getRandomOutside (x:xs)
  | otherwise = return food

colorize :: [Position] -> Position -> Position -> RGB
colorize snake food x
  | x `elem` snake = (3,3,3)
  | x == food      = (3,0,0)
  | otherwise      = (1,1,1)

updateAction :: Char -> Action -> Action
updateAction c x = if opposite x == y then x else y
  where y = case c of
             'w' -> U
             'a' -> L
             's' -> D
             'd' -> R
             _   -> x

logic :: FrameSignal -> IO ()
logic frameSignal = do
  sendMSignal frameSignal $ scale zoom img -- write default image
  oldActionRef <- newIORef R
  actionRef <- newIORef R
  snakeRef  <- newIORef [(15,15),(14,15)]
  foodRef <- getRandomOutside [(15,15),(14,15)] >>= newIORef

  let
    loop = do
      _ <- forkIO updateGame
      threadDelay delay
      loop

    updateGame = do
      action <- readIORef actionRef
      food   <- readIORef foodRef
      snake  <- readIORef snakeRef

      let newSnake = moveSnake snake food action

      newFood <- moveFood snake food

      let frame = map (map (colorize newSnake newFood)) imgPoss

      writeIORef oldActionRef action
      writeIORef snakeRef newSnake
      writeIORef foodRef newFood

      sendMSignal frameSignal $ scale zoom frame

      checkGameOver newSnake

    input = do
      c <- getChar
      modifyIORef actionRef $ updateAction c
      input

    checkGameOver ((x,y):xs) = when
      (  (x,y) `elem` xs
      || x < 0 || x >= width
      || y < 0 || y >= height) gameOver

    gameOver = do
      writeIORef oldActionRef R
      writeIORef actionRef R
      writeIORef snakeRef [(15,15),(14,15)]
      food <- getRandomOutside [(15,15),(14,15)]
      writeIORef foodRef food

  _ <- forkIO input
  loop

opposite :: Action -> Action
opposite L = R
opposite R = L
opposite U = D
opposite D = U

getRandomOutside :: [Position] -> IO Position
getRandomOutside xs = do
  fx <- randomRIO (0,width-1)
  fy <- randomRIO (0,height-1)

  if (fx,fy) `elem` xs
  then getRandomOutside xs
  else return (fx,fy)

scale :: Int -> Frame -> Frame
scale z frame = concatMap (replicate z) $ map (concatMap (replicate z)) frame

imgPoss :: [[Position]]
imgPoss = splitEvery width [(x,y) | y <- [0..height-1], x <- [0..width-1]]

splitEvery :: Int -> [e] -> [[e]]
splitEvery i ls
  | length ys > i = xs : splitEvery i ys
  | otherwise     = [xs,ys]
  where (xs,ys) = splitAt i ls

img :: Frame
img = replicate height $ replicate width (0,0,0)
