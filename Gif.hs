{-# LANGUAGE OverloadedStrings #-} 

module Gif (
  initialFrame,
  frame,
  finalize
  )
  where

-- max image width is 85 pixels

import qualified Data.ByteString as B

initialFrame delay img = B.concat
  [ "GIF89a"
  , number w, number h, gctInfo, bgColor, aspect -- logical screen descriptor
  , realCT, dummyCT                              -- color table
  , "!\255\vNETSCAPE2.0\ETX\SOH\NUL\NUL\NUL"     -- application extension
  , frame delay img
  ]
  where
    w = length $ head img
    h = length img
    gctInfo = B.singleton 0xf6
    bgColor = smallNumber 127
    aspect  = "\NUL"

    realCT  = B.concat $ map B.pack [[r,g,b] | r <- colors, g <- colors, b <- colors]
    colors  = [0,64,128,255]
    dummyCT = B.concat $ replicate 64 $ B.pack [255,255,255]

frame delay img = B.concat
  [ "!\249\EOT\b", number delay, "\255", "\NUL"  -- graphic control extension
  , ",", yPos, xPos, number w, number h, localCT -- image descriptor
  , lzwMinSize, imageData, "\NUL"                -- image
  ]
  where
    w = length $ head img
    h = length img
    yPos = number 0
    xPos = number 0
    localCT = "\NUL"

    lzwMinSize = B.singleton 0x07
    imageData = B.concat $ map mapLines img
    mapLines x = B.concat
      [ bytesToFollow, clear
      , B.pack $ map (\(r,g,b) -> fromIntegral $ 16*r+4*g+b) x
      ]

    bytesToFollow = smallNumber $ w + 1
    clear = B.singleton 0x80

finalize = B.concat [bytesToFollow, stop, "\NUL", ";"]
  where
    bytesToFollow = smallNumber 1
    stop = B.singleton 0x81

smallNumber x = B.singleton $ fromIntegral $ x `mod` 256
number x = B.pack $ map fromIntegral [x `mod` 256, x `div` 256]
