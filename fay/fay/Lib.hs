{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Lib where

import           Prelude
import           JQuery hiding (onKeycode)
import           FFI
import           Fay.Text (fromString)
import qualified Fay.Text as T

int2Text :: Int -> T.Text
int2Text n = T.pack $ Prelude.filter (/= '"') $ show n

addInt :: Int -> Int -> Int
addInt = ffi "Number(%1) + Number(%2)"

subInt :: Int -> Int -> Int
subInt = ffi "Number(%1) - Number(%2)"

getKeyCode :: Event -> Fay T.Text
getKeyCode = ffi "%1['keyCode']"

getLength :: JQuery -> Fay Int
getLength = ffi "%1['length']"

readInt :: T.Text -> Fay Int
readInt = ffi "%1"

readDouble :: T.Text -> Fay Double
readDouble = ffi "%1"

createKeydownEvent :: Int -> Event
createKeydownEvent = ffi "jQuery.Event('keydown', {keyCode: %1})"

triggerHandlerWithEvent :: Event -> JQuery -> Fay ()
triggerHandlerWithEvent = ffi "%2['triggerHandler'](%1)"

onKeycode :: (KeyCode -> Fay ()) -> JQuery -> Fay ()
onKeycode callback = keydown func
  where
    func e = do
      code <- getKeyCode e >>= readDouble
      callback (case code of
                        38 -> KeyUp
                        40 -> KeyDown
                        37 -> KeyLeft
                        39 -> KeyRight
                        _  -> SomeKey code)

keycode2Int :: KeyCode -> Int
keycode2Int KeyLeft  = 37
keycode2Int KeyUp    = 38
keycode2Int KeyRight = 39
keycode2Int KeyDown  = 40

chunksOf :: Int -> [Int] -> [[Int]]
chunksOf n _ | n < 1 = []
chunksOf n [] = []
chunksOf n ns = take n ns : chunksOf n (drop n ns)

getId :: JQuery -> Fay T.Text
getId jq = do
  id <- getAttr "id" jq
  return $ case id of
    Defined text -> text
    Undefined -> "none"

buildTable :: (Int -> Fay T.Text) -> Int -> Int -> Fay T.Text
buildTable f row col = do
  rows <- mapM (buildRow f) $ chunksOf col [0..(row * col - 1)]
  return $ T.concat ["<table>", T.concat rows, "</table>"]

buildRow :: (Int -> Fay T.Text) -> [Int] -> Fay T.Text
buildRow f ids  = do
  cells <- mapM f ids
  return $ T.concat ["<tr>", T.concat cells, "</tr>"]



