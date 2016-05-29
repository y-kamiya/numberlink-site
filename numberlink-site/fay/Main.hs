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
import Data.Function

alert :: String -> Fay ()
alert = ffi "alert(%1)"

console :: T.Text -> Fay ()
console = ffi "console.log(%1)"

this :: Fay JQuery
this = ffi "this"

charCodeOffset :: Int
charCodeOffset = 96

readInt :: T.Text -> Fay Int
readInt = ffi "%1"

chunksOf :: Int -> [Int] -> [[Int]]
chunksOf n _ | n < 1 = []
chunksOf n [] = []
chunksOf n ns = take n ns : chunksOf n (drop n ns)

buildTable :: (Int -> T.Text) -> Int -> Int -> T.Text
buildTable f row col = let contents = T.concat $ map (buildRow f) $ chunksOf col [0..(row * col - 1)]
                       in  T.concat ["<table>", contents, "</table>"]

buildRow :: (Int -> T.Text) -> [Int] -> T.Text
buildRow f ids  = T.concat ["<tr>", T.concat (map f ids), "</tr>"]

main :: Fay ()
main = ready $ do
  isEmptyCell "#field"
  isEmptyCell "#field > p"
  setCurrentId noid
  buttonCreateField >>= onClick createTable
  buttonSolve >>= onClick initEditor
  -- select "body" >>= keydown print

  -- select "#fielogld table td" >>= onClick start
  select "body" >>= keydown move
  return ()

createTable :: Event -> Fay Bool
createTable _ = do
  rowNum <- select "#rowNum" >>= getVal >>= readInt
  colNum <- select "#colNum" >>= getVal >>= readInt
  select "div.fieldSize input" >>= setAttr "disabled" "disabled"
  select "#field" >>= append (buildTable buildCell rowNum colNum)
  buttonCreateField >>= addClass "disabled"
  buttonSolve >>= removeClass "disabled"
  return True
  where
    buildCell :: Int -> T.Text
    buildCell id = T.concat ["<td id=\"c", T.pack (show id), "\"><input type=\"number\"></td>"]

initEditor :: Event -> Fay Bool
initEditor _ = do
  -- select "#input" >>= addClass "disabled"
  buttonSolve >>= addClass "disabled"
  initCursor
  loadField
  return True

buttonCreateField = select "#createTableButton"
buttonSolve = select "#solve"
buttonCursor = select "#cursors"

initCursor :: Fay ()
initCursor = do
  buttonCursor >>= removeClass "disabled"
  select "#cursor-up" >>= onClick dispatch
  select "#cursor-down" >>= onClick dispatch 
  select "#cursor-left" >>= onClick dispatch 
  select "#cursor-right" >>= onClick dispatch 
  return ()
  where
    dispatch :: Event -> Fay Bool
    dispatch _ = select "body" >>= trigger "keydown" >> return True


loadField :: Fay Bool
loadField = do
  rowNum <- select "#rowNum" >>= getVal >>= readInt
  colNum <- select "#colNum" >>= getVal >>= readInt
  table <- buildTable buildCell rowNum colNum
  select "#field table" >>= remove
  select "#field" >>= append table
  select "#field > table tbody > tr > td" >>= onClick start
  return True
  where
    cellValue :: Int -> Fay T.Text
    cellValue cellId = let selector = T.concat ["#c", T.pack $ show cellId]
                       in  select selector >>= findSelector "input" >>= getVal
                       -- in  select "#c0" >>= findSelector "input" >>= getVal

    buildTable :: (Int -> Fay T.Text) -> Int -> Int -> Fay T.Text
    buildTable f row col = do
      rows <- mapM (buildRow f) $ chunksOf col [0..(row * col - 1)]
      return $ T.concat ["<table>", T.concat rows, "</table>"]

    buildRow :: (Int -> Fay T.Text) -> [Int] -> Fay T.Text
    buildRow f ids  = do
      cells <- mapM f ids 
      return $ T.concat ["<tr>", T.concat cells, "</tr>"]

    buildCell :: Int -> Fay T.Text
    buildCell cellId = do
      value <- cellValue cellId 
      return $ case value of
         ""  -> T.concat ["<td id=\"", T.pack $ show cellId, "\">", innerTable, "</td>"]
         v   -> T.concat ["<td id=\"", T.pack $ show cellId, "\">", value, "</td>"]

    innerTable = "<table class=\"innerTable\"><tr><td></td><td></td></tr><tr><td></td><td></td></tr></table>"


start :: Event -> Fay Bool
start e = do
  select "#field td" >>= removeClass "current"
  a <- select "#field"
  el <- currentTarget e
  findElement el a >>= addClass "current"
                   >>= getId >>= setCurrentId
  return True

getId :: JQuery -> Fay T.Text
getId jq = do
  id <- getAttr "id" jq
  return $ case id of
    Defined text -> text
    Undefined -> "none"

getCurrentId :: Fay T.Text
getCurrentId = select "#currentId" >>= getVal

setCurrentId :: T.Text -> Fay JQuery
setCurrentId id = select "#currentId" >>= setVal id

getNextId :: T.Text -> T.Text -> Fay T.Text
getNextId _ _ = return "1"

lineCurrentCell :: T.Text -> T.Text -> Fay ()
lineCurrentCell _ _ = return ()

lineNextCell :: T.Text -> T.Text -> Fay ()
lineNextCell _ _ = return ()

noid = "none"

onKeydownListener :: Event -> Fay ()
onKeydownListener e = do
  currentId <- getCurrentId
  if currentId == noid 
    then return ()
    else do
      keycode <- getKeyCode e
      nextId <- getNextId currentId keycode
      if nextId == noid
        then move e
        else return ()

move :: Event -> Fay ()
move e = do
  keycode <- getKeyCode e
  currentId <- getCurrentId
  nextId <- getNextId currentId keycode    
  let current= T.concat ["#", currentId]
      next = T.concat ["#", nextId]
  select current>>= removeClass "current"
  isEmptyCell current

  emptyCurrent <- isEmptyCell current
  if emptyCurrent
    then return ()
    else lineCurrentCell current keycode

  emptyNext <- isEmptyCell next
  if emptyNext
    then setCurrentId noid
    else do
      lineNextCell next keycode
      select next >>= addClass "current"
      setCurrentId nextId

  return ()

getKeyCode :: Event -> Fay T.Text
getKeyCode = ffi "%1['keyCode']"

isEmptyCell :: T.Text -> Fay Bool
isEmptyCell selector = do
  len <- select selector >>= children >>= getLength
  print len
  return $ 1 <= len 

getLength :: JQuery -> Fay Int
getLength = ffi "%1['length']"






