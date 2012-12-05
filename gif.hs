{-# LANGUAGE OverloadedStrings #-} 

import Data.Char
import qualified Data.ByteString as B

main = do
  B.writeFile "foo.gif" $ toGif img

ct = realCT `B.append` dummyCT
  where realCT = B.concat $ map B.pack [[r,g,b] | r <- colors, g <- colors, b <- colors]
        dummyCT = B.concat $ replicate 64 $ B.pack [255,255,255]
        colors = [0,64,128,255]

img :: [[(Int,Int,Int)]]
img = take 64 $ repeat [(r,g,b) | r <- [0..3], g <- [0..3], b <- [0..3]]

--toGif img = gif (length $ head img) (length img) imageData
--  where

toGif img = gif w h imageData imageData2
  where
    w = length $ head img
    h = length img
    imageData = B.concat $ (map mapLines img)
    imageData2 = B.concat $ (map mapLines2 img) ++ [imageEnd]
    mapLines x = B.concat [bytesToFollow, clear, B.pack $ map (\(r,g,b) -> fromIntegral $ 16*r+4*g+b) x]
    mapLines2 x = B.concat [bytesToFollow, clear, B.pack $ map (\(r,g,b) -> fromIntegral $ 16*r) x]

    imageEnd = B.concat [smallNumber 1, stop, "\NUL"]
    bytesToFollow = smallNumber $ w + 1
    clear = B.singleton 0x80
    stop  = B.singleton 0x81


gif w h imageData imageData2 = B.concat
  [ header
  , logicalScreenDescriptor
  , ct
  , applicationExtension
  , frame
  , frame2
  , terminator
  ]
  where -- http://www.onicos.com/staff/iz/formats/gif.html
    header      = "GIF89a"

    logicalScreenDescriptor = B.concat [width, height, gctInfo, bgColor, aspectRatio]
    width       = number w
    height      = number h
    gctInfo     = B.singleton 0xf6
    bgColor     = smallNumber 127
    aspectRatio = "\NUL"

    imageDescriptor = B.concat [",", yPos, xPos, width, height, localColor]
    yPos        = number 0
    xPos        = number 0
    localColor  = "\NUL"

    applicationExtension = "!\255\vNETSCAPE2.0\ETX\SOH\NUL\NUL\NUL"

    frame = B.concat [graphicControlExtension, imageDescriptor, image]
    image = B.concat [lzwMinSize, imageData, "\NUL"]

    frame2 = B.concat [graphicControlExtension, imageDescriptor, image2]
    image2 = B.concat [lzwMinSize, imageData2, "\NUL"]

    graphicControlExtension = B.concat ["!\249\EOT\b", delay, "\255", "\NUL"]
    delay = number 100

    lzwMinSize = B.singleton 0x07
    terminator  = ";"

smallNumber x = B.singleton $ fromIntegral $ x `mod` 256
number x = B.pack $ map fromIntegral [x `mod` 256, x `div` 256]
