import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import System.Random
import Data.IORef

import MSignal
import Net

-- Stopping focus of the browser tab stops the animation and fucks everything
-- up. Reload the page to fix it.

main = server delay logic

-- 30000 seems to be the lowest value that works in Firefox
-- 30 ms => 33 fps
delay = 100000 -- in µs

width = 32
height = 32
zoom = 4

data Action = L | R | U | D deriving Eq

moveSnake xs@((x,y):_) food action = newHead : newTail
  where newHead = case action of
            L -> (x-1,y)
            R -> (x+1,y)
            U -> (x,y-1)
            D -> (x,y+1)
        newTail
          | newHead == food = xs
          | otherwise  = init xs

moveFood (x:xs) food
  | x == food = getRandomOutside (x:xs)
  | otherwise = return food

colorize snake food x
  | x `elem` snake = (3,3,3)
  | x == food      = (3,0,0)
  | otherwise      = (1,1,1)

logic imageSignal = do
  sendMSignal imageSignal $ scale zoom img -- write default image
  oldActionRef <- newIORef R
  actionRef <- newIORef R
  snakeRef  <- newIORef [(15,15),(14,15)]
  food <- getRandomOutside [(15,15),(14,15)]
  foodRef   <- newIORef food

  let
    loop = do
      forkIO updateGame
      threadDelay delay
      loop

    updateGame = do
      action <- readIORef actionRef
      food   <- readIORef foodRef
      snake  <- readIORef snakeRef

      let newSnake = moveSnake snake food action

      newFood <- moveFood snake food

      let image = map (map (colorize newSnake newFood)) imgPoss

      writeIORef oldActionRef action
      writeIORef snakeRef newSnake
      writeIORef foodRef newFood

      sendMSignal imageSignal $ scale zoom image

      checkGameOver newSnake

    input = do
      c <- getChar
      x <- readIORef oldActionRef
      writeIORef actionRef $
        let y = case c of
                  'w' -> U
                  'a' -> L
                  's' -> D
                  'd' -> R
                  otherwise -> x
        in if opposite x == y then x else y
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

  forkIO input
  loop

opposite L = R
opposite R = L
opposite U = D
opposite D = U

getRandomOutside xs = do
  fx <- randomRIO (0,width-1)
  fy <- randomRIO (0,height-1)

  if (fx,fy) `elem` xs
  then getRandomOutside xs
  else return (fx,fy)

scale zoom img = concatMap (replicate zoom) $ map (concatMap (replicate zoom)) img

imgPoss = splitEvery width [(x,y) | y <- [0..height-1], x <- [0..width-1]]

splitEvery :: Int -> [e] -> [[e]]
splitEvery i ls
  | length ys > i = xs : splitEvery i ys
  | otherwise     = [xs,ys]
  where (xs,ys) = splitAt i ls

img :: [[(Int,Int,Int)]]
img = replicate height $ replicate height (0,0,0)
