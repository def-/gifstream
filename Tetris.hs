import Data.IORef
import System.Timeout
import Control.Concurrent

import Net

-- Stopping focus of the browser tab stops the animation and fucks everything
-- up. Reload the page to fix it.

main = server delay logic

delay = 100000 -- in µs
width = 10
height = 20

FieldState = Empty | Filled deriving (Eq, Show)
Action = None | MoveLeft | MoveRight | RotateLeft | RotateRight deriving (Eq, Show)

tetrominoes =
  [ [ "#"
    , "#"
    , "#"
    , "#"
    ]
  , [ "##"
    , "##"
    ]
  , [ "_#_"
    , "###"
    ]
  , [ "##_"
    , "_##"
    ]
  , [ "_##"
    , "##_"
    ]
  ]

logic imageRef = do
  writeIORef imageRef img -- write default image
  actionRef <- newIORef None
  fallingRef <- newIORef (Int, Int, [String])
  fieldRef <- newIORef $ replicate height $ replicate width Empty

  forkIO $ time
  input

  where
    time = do
      threadDelay delay

      action <- readIORef actionRef
      case action of
        None        -> return ()
        MoveLeft    -> 
        MoveRight   -> 
        RotateLeft  -> 
        RotateRight -> 

    input = do
      c <- getChar
      case c of
        'a' -> writeIORef actionRef MoveLeft
        'd' -> writeIORef actionRef MoveRight
        'w' -> writeIORef actionRef RotateLeft
        's' -> writeIORef actionRef RotateRight
      input

img :: [[(Int,Int,Int)]]
img = replicate 64 [(r,g,b) | r <- [0..3], g <- [0..3], b <- [0..3]]

img2 :: [[(Int,Int,Int)]]
img2 = replicate 64 [(r,g,b) | r <- [0..3], g <- [0,0,0,0], b <- [0,0,0,0]]
