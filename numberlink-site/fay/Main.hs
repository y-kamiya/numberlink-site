{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Main where

import           Prelude
import           JQuery hiding (onKeycode)
import           FFI
import           Fay.Text (fromString)
import qualified Fay.Text as T
import Data.Char (ord)
import Data.Function

import Lib

noid = "none"

buttonCreateField = select "#createTableButton"
buttonSolve       = select "#solve"
buttonCursor      = select "#cursors"


data PuzzleState = PuzzleState {
    rowNum :: Int
  , colNum :: Int
  , keycode :: KeyCode
  , currentId :: Int
}

main :: Fay ()
main = ready $ do
  isEmptyCell "#field"
  isEmptyCell "#field > p"
  setCurrentId noid
  buttonCreateField >>= onClick createTable
  buttonSolve >>= onClick initEditor
  select "body" >>= onKeycode onKeydownListener
  return ()

createTable :: Event -> Fay Bool
createTable _ = do
  rowNum <- select "#rowNum" >>= getVal >>= readInt
  colNum <- select "#colNum" >>= getVal >>= readInt
  select "div.fieldSize input" >>= setAttr "disabled" "disabled"
  table <- buildTable buildCell rowNum colNum
  select "#field" >>= append table
  buttonCreateField >>= addClass "disabled"
  buttonSolve >>= removeClass "disabled"
  return True
  where
    buildCell :: Int -> Fay T.Text
    buildCell id = return $ T.concat ["<td id=\"c", T.pack (show id), "\"><input type=\"number\"></td>"]

initEditor :: Event -> Fay Bool
initEditor _ = do
  buttonSolve >>= addClass "disabled"
  initCursor
  loadField
  return True

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
    dispatch keycode _ = let e = createKeydownEvent $ keycode2Int keycode
                         in  select "body" >>= triggerHandlerWithEvent e >> return True

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
    cellValue cellId = let selector = T.concat ["#c", int2Text cellId]
                       in  select selector >>= findSelector "input" >>= getVal

    buildCell :: Int -> Fay T.Text
    buildCell cellId = do
      value <- cellValue cellId
      return $ case value of
         ""  -> T.concat ["<td id=\"", int2Text cellId, "\">", innerTable, "</td>"]
         v   -> T.concat ["<td id=\"", int2Text cellId, "\">", value, "</td>"]

    innerTable = "<table class=\"innerTable\"><tr><td></td><td></td></tr><tr><td></td><td></td></tr></table>"

start :: Event -> Fay Bool
start e = do
  select "#field td" >>= removeClass "current"
  a <- select "#field"
  el <- currentTarget e
  findElement el a >>= addClass "current"
                   >>= getId >>= setCurrentId
  return True

getCurrentId :: Fay T.Text
getCurrentId = select "#currentId" >>= getVal

setCurrentId :: T.Text -> Fay JQuery
setCurrentId id = select "#currentId" >>= setVal id

getNextId :: PuzzleState -> T.Text
getNextId state =
  let row = (currentId state) `div` (colNum state)
      col = (currentId state) `mod` (colNum state)
  in getNextId' (keycode state) row col
  where
    getNextId' :: KeyCode -> Int -> Int -> T.Text
    getNextId' KeyLeft _ col | col < 1 = noid
    getNextId' KeyLeft _ _ = int2Text $ addInt (currentId state) (-1)
    getNextId' KeyUp row _ | row < 1 = noid
    getNextId' KeyUp row _ = int2Text $ subInt (currentId state) (colNum state)
    getNextId' KeyRight _ col | (colNum state) <= col + 1 = noid
    getNextId' KeyRight _ col = int2Text $ addInt (currentId state) 1
    getNextId' KeyDown row _ | (rowNum state) <= row + 1 = noid
    getNextId' KeyDown row _ = int2Text $ addInt (currentId state) (colNum state)
    getNextId' _ _ _ = noid

lineCurrentCell :: T.Text -> KeyCode -> Fay ()
lineCurrentCell selector KeyLeft = void $ select selector >>= findSelector "td" >>= eq 2 >>= addClass "top"
lineCurrentCell selector KeyUp = void $ select selector >>= findSelector "td" >>= eq 0 >>= addClass "right"
lineCurrentCell selector KeyRight = void $ select selector >>= findSelector "td" >>= eq 1 >>= addClass "bottom"
lineCurrentCell selector KeyDown = void $ select selector >>= findSelector "td" >>= eq 3 >>= addClass "left"

lineNextCell :: T.Text -> KeyCode -> Fay ()
lineNextCell selector KeyLeft = void $ select selector >>= findSelector "td" >>= eq 1 >>= addClass "bottom"
lineNextCell selector KeyUp = void $ select selector >>= findSelector "td" >>= eq 3 >>= addClass "left"
lineNextCell selector KeyRight = void $ select selector >>= findSelector "td" >>= eq 2 >>= addClass "top"
lineNextCell selector KeyDown = void $ select selector >>= findSelector "td" >>= eq 0 >>= addClass "right"

onKeydownListener :: KeyCode -> Fay ()
onKeydownListener keycode = do
  currentId <- getCurrentId
  when (existCell currentId) $ do
    rowNum <- select "#rowNum" >>= getVal >>= readInt
    colNum <- select "#colNum" >>= getVal >>= readInt
    id <- readInt currentId

    let state = PuzzleState rowNum colNum keycode id
    when (existCell $ getNextId state) $ move state

  return ()

move :: PuzzleState -> Fay ()
move state = do
  let current = T.concat ["#", int2Text $ currentId state]
      nextId = getNextId state
      next = T.concat ["#", nextId]
  print $ keycode state
  print nextId
  select current >>= removeClass "current"
  isEmptyCell current

  emptyCurrent <- isEmptyCell current
  when emptyCurrent $ lineCurrentCell current (keycode state)

  emptyNext <- isEmptyCell next
  when emptyNext $ void $ do
    lineNextCell next (keycode state)
    select next >>= addClass "current"

  setCurrentId nextId

  return ()

isEmptyCell :: T.Text -> Fay Bool
isEmptyCell selector = do
  len <- select selector >>= children >>= getLength
  return $ 1 <= len

existCell :: T.Text -> Bool
existCell = (/= noid)

