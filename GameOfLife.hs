module Main
where

import Data.IORef
import System.Timeout
import Control.Concurrent

import Net

-- Stopping focus of the browser tab stops the animation and fucks everything
-- up. Reload the page to fix it.

main = server delay logic

delay = 100000 -- in µs
width = 85
height = 85

logic imageRef = do
  writeIORef imageRef $ toImg initialField -- write default image
  loop

  where
    loop = do
      threadDelay delay

      modifyIORef (map ) imageRef

      loop

toImg field = map (map (\x -> if x then (0,0,0) else (3,3,3))) field

initialField :: [[Bool]]
initialField = glider ++ rest
  where glider =
          [[False,True,False] ++ replicate (width - 3) False]
          ++ [[False,False,True] ++ replicate (width - 3) False]
          ++ [[True, True, True] ++ replicate (width - 3) False]

        rest = replicate height $ replicate width False
