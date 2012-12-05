{-# LANGUAGE OverloadedStrings #-} 

module Gif (
  img,
  img2,
  initialFrame,
  frame,
  finalize
  )
  where

import Data.Char
import qualified Data.ByteString as B

main = do
  B.writeFile "foo.gif" $ B.concat [initialFrame 10 img, frame 10 img2, finalize]

img :: [[(Int,Int,Int)]]
img = take 64 $ repeat [(r,g,b) | r <- [0..3], g <- [0..3], b <- [0..3]]

img2 :: [[(Int,Int,Int)]]
img2 = take 64 $ repeat [(r,g,b) | r <- [0..3], g <- [0,0,0,0], b <- [0,0,0,0]]

initialFrame delay img = B.concat
  [ header
  , logicalScreenDescriptor
  , colortable
  , applicationExtension
  , frame delay img
  ]
  where -- http://www.onicos.com/staff/iz/formats/gif.html
    header      = "GIF89a"

    w = length $ head img
    h = length img

    colortable = realCT `B.append` dummyCT
      where realCT = B.concat $ map B.pack [[r,g,b] | r <- colors, g <- colors, b <- colors]
            dummyCT = B.concat $ replicate 64 $ B.pack [255,255,255]
            colors = [0,64,128,255]

    logicalScreenDescriptor = B.concat [width, height, gctInfo, bgColor, aspectRatio]
    width       = number w
    height      = number h
    gctInfo     = B.singleton 0xf6
    bgColor     = smallNumber 127
    aspectRatio = "\NUL"

    applicationExtension = "!\255\vNETSCAPE2.0\ETX\SOH\NUL\NUL\NUL"

frame delay img = B.concat [graphicControlExtension, imageDescriptor, image]
  where
    graphicControlExtension = B.concat ["!\249\EOT\b", delayB, "\255", "\NUL"]
    delayB = number delay

    image = B.concat [lzwMinSize, imageData, "\NUL"]

    imageData = B.concat $ (map mapLines img)
    mapLines x = B.concat [bytesToFollow, clear, B.pack $ map (\(r,g,b) -> fromIntegral $ 16*r+4*g+b) x]

    imageDescriptor = B.concat [",", yPos, xPos, width, height, localColor]
    w = length $ head img
    h = length img

    width      = number w
    height     = number h
    yPos       = number 0
    xPos       = number 0
    localColor = "\NUL"

    bytesToFollow = smallNumber $ w + 1
    clear = B.singleton 0x80

    lzwMinSize = B.singleton 0x07

finalize = B.concat [imageEnd, terminator]
  where
    imageEnd = B.concat [smallNumber 1, stop, "\NUL"]
    stop  = B.singleton 0x81

    terminator  = ";"

smallNumber x = B.singleton $ fromIntegral $ x `mod` 256
number x = B.pack $ map fromIntegral [x `mod` 256, x `div` 256]
