{-# LANGUAGE OverloadedStrings #-}

module Net (
  server,
  RGB,
  Frame,
  FrameSignal,
  Logic
  )
  where

import qualified Data.ByteString as B

import Network hiding (accept)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Control.Concurrent

import System.IO

import MSignal
import Gif

type FrameSignal = MSignal Frame
type Logic = FrameSignal -> IO ()

server :: PortNumber -> Int -> Logic -> IO ()
server port delay logic = withSocketsDo $ do
  hSetBuffering stdin NoBuffering
  sock <- listenOn $ PortNumber port
  putStrLn $ "Guckst du hier: http://localhost:" ++ show port ++ "/"
  frameSignal <- newMSignal
  _ <- forkIO $ loop delay frameSignal sock
  logic frameSignal

loop :: Int -> FrameSignal -> Socket -> IO ()
loop delay frameSignal sock = do
  (conn, _) <- accept sock

  _ <- forkIO $ body conn
  loop delay frameSignal sock

  where -- lower delay in GIF to force browser to actually show the gif we send
    body c = do
      f <- receiveMSignal frameSignal
      sendAll c $ msg $ initialFrame (delay `div` 20000) f
      nextFrame c

    nextFrame c = do
      f <- receiveMSignal frameSignal
      sendAll c $ frame (delay `div` 20000) f
      nextFrame c

    msg content = B.intercalate "\r\n"
      [ "HTTP/1.0 200 OK"
      , "Server: gifstream/0.1"
      , "Content-Type: image/gif"
      , "Content-Transfer-Encoding: binary"
      , ""
      , content
      ]
