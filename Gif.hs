{-# LANGUAGE OverloadedStrings #-}

module Gif (
  initialFrame,
  frame,
  finalize,
  RGB,
  Frame
  )
  where

import Data.List
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Arrow

import LZW

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8() -- for OverloadedStrings

type RGB = (Int,Int,Int)
type Frame = [[RGB]]

initialFrame :: Int -> Frame -> B.ByteString
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

frame :: Int -> Frame -> B.ByteString
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
    imageData = B.concat $ mapLine $ B.unpack $ lzwEncode 7 $ map (\(r,g,b) -> 16*r+4*g+b) $ concat img

    mapLine x
      | null ys   = z
      | otherwise = z ++ mapLine ys
      where (y,ys) = splitAt 126 x
            z = [ bytesToFollow
                , B.pack $ map fromIntegral y
                ]
            bytesToFollow = smallNumber $ length y
            clear = B.singleton 0x80

finalize :: B.ByteString
finalize = B.concat [bytesToFollow, stop, "\NUL", ";"]
  where
    bytesToFollow = smallNumber 1
    stop = B.singleton 0x81

smallNumber :: Int -> B.ByteString
smallNumber x = B.singleton $ fromIntegral $ x `mod` 256

number :: Int -> B.ByteString
number x = B.pack $ map fromIntegral [x `mod` 256, x `div` 256]

take2 = filter((==2).length). map (take 2). tails

doLZW _ [] = []
doLZW as (x:xs) =  lzw (map return as) [x] xs
   where lzw a w [] = [fromJust $ elemIndex w a]
         lzw a w (x:xs)  | w' `elem` a = lzw a w' xs
                         | otherwise   = fromJust (elemIndex w a) : lzw (a++[w']) [x] xs
              where w' = w++[x]

encode_LZW alphabet = work (map (:[]) alphabet) where
  chunk pred lst = last . takeWhile (pred . fst) . tail $ zip (inits lst) (tails lst)
  work table []  = []
  work table lst = fromJust (elemIndex tok table) : work (table ++ [tok ++ [head rst]]) rst
    where (tok, rst) = chunk (`elem` table) lst
