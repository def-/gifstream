import GifStream

-- Stopping focus of the browser tab stops the animation. Reload the page to fix it.

type Position = (Int,Int)

data Action = MoveLeft | MoveRight | MoveUp | MoveDown deriving Eq

data State = State
    { oldAction :: Action
    , snake :: [Position]
    , food :: Position
    }

-- 30000 seems to be the lowest value that works in Firefox
-- 30 ms => 33 fps
delay = 200000 -- in µs
port = 5002

width = 32
height = 32
zoom = 4

main :: IO ()
main = server port delay logic

logic :: IO () -> IO Char -> FrameSignal -> IO ()
logic wait getInput frameSignal = initialState >>= go
  where
    go (State oldAction snake food) = do
      input <- getInput

      -- Generate new State
      let action = charToAction input oldAction
      let newSnake = snake
      let newFood = food

      let frame = case action of
            MoveUp    -> replicate height $ replicate width (3,0,0)
            MoveDown  -> replicate height $ replicate width (0,3,0)
            MoveLeft  -> replicate height $ replicate width (0,0,3)
            MoveRight -> replicate height $ replicate width (3,3,3)

      sendMSignal frameSignal $ scale zoom frame

      wait
      go $ State action newSnake newFood

initialState :: IO State
initialState = do
  let startSnake = [(15,15),(14,15)]
  let food = (28,28)
  return $ State MoveRight startSnake food

charToAction :: Char -> Action -> Action
charToAction c oldAction = case c of
  'w' -> MoveUp
  'a' -> MoveLeft
  's' -> MoveDown
  'd' -> MoveRight
  _   -> oldAction

scale :: Int -> Frame -> Frame
scale z frame = concatMap (replicate z) $ map (concatMap (replicate z)) frame
