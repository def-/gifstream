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
import Control.Concurrent.MVar

import System.IO

import Gif

port  = 5002

server delay logic = withSocketsDo $ do
  hSetBuffering stdin NoBuffering
  sock <- listenOn $ PortNumber port
  imageRef <- newEmptyMVar
  forkIO $ loop delay imageRef sock
  logic imageRef

loop delay imageRef sock = do
  (conn, _) <- accept sock

  forkIO $ body conn
  loop delay imageRef sock

  where -- lower delay in GIF to force browser to actually show the gif we send
    body c = do
      i <- takeMVar imageRef
      sendAll c $ msg $ initialFrame (delay `div` 15000) i
      nextFrame c

    nextFrame c = do
      --threadDelay delay
      i <- takeMVar imageRef
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
