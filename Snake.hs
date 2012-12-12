import Data.IORef
import Control.Concurrent
import Control.Concurrent.MVar
import System.Random

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
zoom = 8

data Action = L | R | U | D deriving Eq

logic state = do
  sendMSignal state $ scale zoom img -- write default image
  oldActionRef <- newIORef R
  actionRef <- newIORef R
  snakeRef  <- newIORef [(15,15),(14,15)]
  foodRef   <- newIORef (28,28)

  let
    loop = do
      forkIO action
      threadDelay delay
      loop

    action = do
      action <- readIORef actionRef
      writeIORef oldActionRef action
      modifyIORef snakeRef $ \xs@((x,y):_) -> (case action of
          L -> (x-1,y)
          R -> (x+1,y)
          U -> (x,y-1)
          D -> (x,y+1)
        ):xs

      food <- readIORef foodRef
      (x@(xx,xy):xs) <- readIORef snakeRef
      modifyIORef snakeRef $ \(x:xs) -> if x == food
        then x : xs
        else x : init xs

      (fx,fy) <- getRandomOutside (x:xs)

      if x `elem` xs || xx < 0 || xx >= width || xy < 0 || xy >= height then gameOver else return ()

      if x == food
      then writeIORef foodRef (fx,fy)
      else return ()

      snake <- readIORef snakeRef

      let colorize x = if x `elem` snake then (3,3,3) else if x == food then (3,0,0) else (0,0,0)
          image = splitEvery width $ map colorize [(x,y) | y <- [0..height-1], x <- [0..width-1]]

      sendMSignal state $ scale zoom image

    input = do
      c <- getChar
      x <- readIORef oldActionRef
      writeIORef actionRef $
        let y = case c of
                  'a' -> L
                  'd' -> R
                  'w' -> U
                  's' -> D
                  otherwise -> x
        in if opposite x == y then x else y
      input

    gameOver = do
      writeIORef oldActionRef R
      writeIORef actionRef R
      writeIORef snakeRef [(15,15),(14,15)]
      writeIORef foodRef (28,28)
      --threadDelay 2000000
      --loop

  forkIO $ input
  loop

opposite L = R
opposite R = L
opposite U = D
opposite D = U

getRandomOutside xs = do
  fx <- randomRIO (0,width-1)
  fy <- randomRIO (0,height-1)
  return $ getNextFitting (fx,fy)

  where getNextFitting (fx,fy)
          | (fx,fy) `elem` xs = if fx < width - 1 then getNextFitting (fx+1,fy)
                                                  else getNextFitting (0, fy+1)
          | otherwise         = (fx,fy)

scale zoom img = concat $ map (replicate zoom) $ map (\x -> concat $ map (replicate zoom) x) img

build g = g (:) []

splitEvery :: Int -> [e] -> [[e]]
splitEvery i ls = map (take i) (build (splitter ls)) where
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

img :: [[(Int,Int,Int)]]
img = replicate height $ replicate height (0,0,0)
