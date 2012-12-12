import Data.IORef

import Net

-- Stopping focus of the browser tab stops the animation and fucks everything
-- up. Reload the page to fix it.

main = server delay logic

-- 30000 seems to be the lowest value that works in Firefox
-- 30 ms => 33 fps
delay = 30000 -- in µs

logic state = do
  writeIORef state img -- write default image
  loop

  where
    loop = do
      c <- getChar
      case c of
        'a' -> writeIORef state img2
        otherwise -> writeIORef state img
      loop

img :: [[(Int,Int,Int)]]
img = replicate 128 [(r,g,b) | r <- [0..3], g <- [0..3], b <- [0..3] ++ [0..3]]

img2 :: [[(Int,Int,Int)]]
img2 = replicate 128 [(r,g,b) | r <- [3,2..0], g <- [3,2..0], b <- [0,0,0,0,0,0,0,0]]
