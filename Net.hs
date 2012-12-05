{-# LANGUAGE OverloadedStrings #-}

module Net (
  server,
  )
  where

import qualified Data.ByteString as B

import Network hiding (accept)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Control.Concurrent

import Data.IORef
import System.IO

import Gif

port  = 5002
delay = 100000 -- in µs

server logic = withSocketsDo $ do
  hSetBuffering stdin NoBuffering
  sock <- listenOn $ PortNumber port
  state <- newIORef []
  forkIO $ loop state sock
  logic state

loop state sock = do
  (conn, _) <- accept sock

  forkIO $ body conn
  loop state sock

  where -- lower delay in GIF to force browser to actually show the gif we send
    body c = do
      i <- readIORef state
      sendAll c $ msg $ initialFrame (delay `div` 15000) i
      nextFrame c

    nextFrame c = do
      threadDelay delay
      i <- readIORef state
      sendAll c $ frame (delay `div` 15000) i
      nextFrame c

    msg content = B.intercalate "\r\n"
      [ "HTTP/1.0 200 OK"
      , "Server: gifstream/0.1"
      , "Content-Type: image/gif"
      , "Content-Transfer-Encoding: binary"
      , ""
      , content
      ]
