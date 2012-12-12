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
import Control.Concurrent.Chan

import System.IO

import Gif

port  = 5002

server delay logic = withSocketsDo $ do
  hSetBuffering stdin NoBuffering
  sock <- listenOn $ PortNumber port
  imageChan <- newChan
  forkIO $ loop delay imageChan sock
  logic imageChan

loop delay imageChan sock = do
  (conn, _) <- accept sock

  forkIO $ body conn
  imageChan2 <- dupChan imageChan
  loop delay imageChan2 sock

  where -- lower delay in GIF to force browser to actually show the gif we send
    body c = do
      i <- readChan imageChan
      sendAll c $ msg $ initialFrame (delay `div` 15000) i
      nextFrame c

    nextFrame c = do
      i <- readChan imageChan
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
