{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Main where

import           Prelude
import           JQuery
import           FFI
import           Fay.Text (fromString)
import qualified Fay.Text as T
import Data.Char (ord)

alert :: String -> Fay ()
alert = ffi "alert(%1)"

console :: T.Text -> Fay ()
console = ffi "console.log(%1)"

charCodeOffset :: Int
charCodeOffset = 96

readInt :: T.Text -> Fay Int
readInt = ffi "%1"

chunksOf :: Int -> [Int] -> [[Int]]
chunksOf n _ | n < 1 = []
chunksOf n [] = []
chunksOf n ns = take n ns : chunksOf n (drop n ns)

main :: Fay ()
main = ready $ do
  buttonCreateField >>= onClick createTable
  buttonSolve >>= onClick initEditor
  initCursor

  -- Main.load
  -- select "#fielogld table td" >>= onClick start
  -- select "body" >>= keydown move
  return ()

createTable :: Event -> Fay Bool
createTable _ = do
  rowNum <- select "#rowNum" >>= getVal >>= readInt
  colNum <- select "#colNum" >>= getVal >>= readInt
  select "div.fieldSize input" >>= setAttr "disabled" "disabled"
  select "#field" >>= append (buildTable rowNum colNum)
  buttonCreateField >>= addClass "disabled"
  buttonSolve >>= removeClass "disabled"
  return True
  where
    buildTable :: Int -> Int -> T.Text
    buildTable row col = let contents = T.concat $ map buildRow $ chunksOf col [0..(row * col - 1)]
                         in  T.concat ["<table>", contents, "</table>"]

    buildCell :: Int -> T.Text
    buildCell id = T.concat ["<td id=\"c", T.pack (show id), "\"><input type=\"number\"></td>"]

    buildRow :: [Int] -> T.Text
    buildRow ids  = T.concat ["<tr>", T.concat (map buildCell ids), "</tr>"]

initEditor :: Event -> Fay Bool
initEditor _ = do
  -- save
  select "#field table" >>= remove
  select "#input" >>= addClass "disabled"
  buttonSolve >>= addClass "disabled"
  initCursor
  -- load
  return True

buttonCreateField = select "#createTableButton"
buttonSolve = select "#solve"
buttonCursor = select "#cursors"

initCursor :: Fay ()
initCursor = do
  buttonCursor >>= removeClass "disabled"
  select "#cursor-up" >>= onClick (dispatch KeyUp)
  select "#cursor-down" >>= onClick (dispatch KeyDown)
  select "#cursor-left" >>= onClick (dispatch KeyLeft)
  select "#cursor-right" >>= onClick (dispatch KeyRight)
  return ()
  where
    dispatch :: KeyCode -> Event -> Fay Bool
    dispatch _ _ = select "body" >>= trigger "keydown" >> return True


save :: Event -> Fay Bool
save = undefined

getNextId :: Fay ()
getNextId = undefined

lineCurrentCell :: Fay ()
lineCurrentCell = undefined

lineNextCell :: Fay ()
lineNextCell = undefined

hasTable :: Fay ()
hasTable = undefined

keyDown :: Fay ()
keyDown = undefined

