{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
  )
  where

import qualified Data.ByteString as B

import Network hiding (accept)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Control.Concurrent

import Data.IORef
import System.IO
import System.IO.Unsafe

import Gif

state = unsafePerformIO $ newIORef $ img

delay = 10 -- in s/100

main = withSocketsDo $ do
    hSetBuffering stdin NoBuffering
    sock <- listenOn $ PortNumber 5002
    forkIO $ loop sock
    input

input = do
  c <- getChar
  case c of
    'a' -> writeIORef state img2
    otherwise -> writeIORef state img
  input

loop sock = do
   --content <- B.readFile "foo.gif"
   (conn, _) <- accept sock

   let body c = do sendAll c $ msg $ initialFrame delay img
                   go c
                   sendAll c $ finalize
                   sClose c

       go c = do i <- readIORef state
                 threadDelay $ delay * 15000
                 sendAll c $ frame delay i
                 go c

   forkIO $ body conn
   loop sock

--msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

msg content = B.intercalate "\r\n"
  [ "HTTP/1.0 200 OK"
  , "Server: gifstream/0.1"
  , "Content-Type: image/gif"
  , "Content-Transfer-Encoding: binary"
  , ""
  , content
  ]
