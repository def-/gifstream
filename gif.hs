{-# LANGUAGE OverloadedStrings #-} 

import Data.Char
import qualified Data.ByteString as B

main = do
  B.writeFile "foo.gif" $ B.concat [initialFrame img, nextFrame img2, finalize]

ct = realCT `B.append` dummyCT
  where realCT = B.concat $ map B.pack [[r,g,b] | r <- colors, g <- colors, b <- colors]
        dummyCT = B.concat $ replicate 64 $ B.pack [255,255,255]
        colors = [0,64,128,255]

img :: [[(Int,Int,Int)]]
img = take 64 $ repeat [(r,g,b) | r <- [0..3], g <- [0..3], b <- [0..3]]
img2 = take 64 $ repeat [(r,g,b) | r <- [0..3], g <- [0,0,0,0], b <- [0,0,0,0]]

toGif img = gif w h [imageData, imageData2]
  where
    w = length $ head img
    h = length img

    imageData = B.concat $ (map mapLines img)
    mapLines x = B.concat [bytesToFollow, clear, B.pack $ map (\(r,g,b) -> fromIntegral $ 16*r+4*g+b) x]

    imageData2 = B.concat $ (map mapLines2 img)
    mapLines2 x = B.concat [bytesToFollow, clear, B.pack $ map (\(r,g,b) -> fromIntegral $ 16*r) x]

    bytesToFollow = smallNumber $ w + 1
    clear = B.singleton 0x80

initialFrame img = B.concat
  [ header
  , logicalScreenDescriptor
  , ct
  , applicationExtension
  , frame
  ]
  where -- http://www.onicos.com/staff/iz/formats/gif.html
    w = length $ head img
    h = length img

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

    imageData = B.concat $ (map mapLines img)
    mapLines x = B.concat [bytesToFollow, clear, B.pack $ map (\(r,g,b) -> fromIntegral $ 16*r+4*g+b) x]

    bytesToFollow = smallNumber $ w + 1
    clear = B.singleton 0x80

    graphicControlExtension = B.concat ["!\249\EOT\b", delay, "\255", "\NUL"]
    delay = number 100

    lzwMinSize = B.singleton 0x07

nextFrame img = frame
  where
    w = length $ head img
    h = length img

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

    frame = B.concat [graphicControlExtension, imageDescriptor, image]
    image = B.concat [lzwMinSize, imageData, "\NUL"]

    imageData = B.concat $ (map mapLines img)
    mapLines x = B.concat [bytesToFollow, clear, B.pack $ map (\(r,g,b) -> fromIntegral $ 16*r+4*g+b) x]

    bytesToFollow = smallNumber $ w + 1
    clear = B.singleton 0x80

    graphicControlExtension = B.concat ["!\249\EOT\b", delay, "\255", "\NUL"]
    delay = number 100

    lzwMinSize = B.singleton 0x07


finalize = B.concat [imageEnd, terminator]
  where
    imageEnd = B.concat [smallNumber 1, stop, "\NUL"]
    stop  = B.singleton 0x81

    terminator  = ";"

gif w h imageDatas = B.concat
  [ header
  , logicalScreenDescriptor
  , ct
  , applicationExtension
  , frames
  , imageEnd
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

    frames = B.concat $ map (\i -> B.concat [graphicControlExtension, imageDescriptor, i]) images
    images = map (\i -> B.concat [lzwMinSize, i, "\NUL"]) imageDatas

    graphicControlExtension = B.concat ["!\249\EOT\b", delay, "\255", "\NUL"]
    delay = number 100

    lzwMinSize = B.singleton 0x07

    imageEnd = B.concat [smallNumber 1, stop, "\NUL"]
    stop  = B.singleton 0x81

    terminator  = ";"

smallNumber x = B.singleton $ fromIntegral $ x `mod` 256
number x = B.pack $ map fromIntegral [x `mod` 256, x `div` 256]
