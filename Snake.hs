import Data.IORef
import Control.Concurrent
import Control.Concurrent.MVar

import Net

-- Stopping focus of the browser tab stops the animation and fucks everything
-- up. Reload the page to fix it.

main = server delay logic

-- 30000 seems to be the lowest value that works in Firefox
-- 30 ms => 33 fps
delay = 300000 -- in µs

width = 32
height = 32
zoom = 8

data Action = L | R | U | D

logic state = do
  putMVar state $ scale zoom img -- write default image
  actionRef <- newIORef R
  snakeRef  <- newIORef [(15,15),(16,15)]
  foodRef   <- newIORef (28,28)

  let
    loop = do
      forkIO $ action
      threadDelay $ delay
      loop

    action = do
      action <- readIORef actionRef
      modifyIORef snakeRef $ \xs@((x,y):_) -> (case action of
          L -> (x-1,y)
          R -> (x+1,y)
          U -> (x,y-1)
          D -> (x,y+1)
        ):xs

      food <- readIORef foodRef
      modifyIORef snakeRef $ \(x:xs) -> if x == food
        then x : xs
        else x : init xs

      snake <- readIORef snakeRef

      let colorize x = if x `elem` snake then (3,3,3) else if x == food then (3,0,0) else (0,0,0)
          image = splitEvery width $ map colorize [(x,y) | y <- [0..height-1], x <- [0..width-1]]
      putMVar state $ scale zoom image

    input = do
      c <- getChar
      case c of
        'a' -> writeIORef actionRef L
        'd' -> writeIORef actionRef R
        'w' -> writeIORef actionRef U
        's' -> writeIORef actionRef D
        otherwise -> return ()
      input

  forkIO $ input
  loop

scale zoom img = concat $ map (replicate zoom) $ map (\x -> concat $ map (replicate zoom) x) img

build g = g (:) []

splitEvery :: Int -> [e] -> [[e]]
splitEvery i ls = map (take i) (build (splitter ls)) where
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

img :: [[(Int,Int,Int)]]
img = replicate height $ replicate height (0,0,0)
