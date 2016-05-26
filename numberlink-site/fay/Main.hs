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

main :: Fay ()
main = do
  select "#createTableButton" >>= onClick createTable
  select "#save" >>= onClick save
  select "#solve" >>= onClick initEditor

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
  select "#buttons div" >>= eq 0 >>= addClass "disabled"
  select "#buttons div" >>= eq 1 >>= removeClass "disabled"
  return True

buildTable :: Int -> Int -> T.Text
buildTable row col = let contents = T.concat $ map buildRow $ chunksOf col [0..(row * col - 1)]
                     in  T.concat ["<table>", contents, "</table>"]

readInt :: T.Text -> Fay Int
readInt = ffi "%1"

chunksOf :: Int -> [Int] -> [[Int]]
chunksOf n _ | n < 1 = []
chunksOf n [] = []
chunksOf n ns = take n ns : chunksOf n (drop n ns)

buildRow :: [Int] -> T.Text
buildRow ids  = T.concat ["<tr>", T.concat (map buildCell ids), "</tr>"]

buildCell :: Int -> T.Text
buildCell id = T.concat ["<td id=\"c", T.pack (show id), "\"><input type=\"number\"></td>"]

initEditor :: Event -> Fay Bool
initEditor = undefined

initCursor :: Fay ()
initCursor = undefined

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



-- load :: Fay ()
-- load = do
--   input <- select "#input" >>= getText 
--   rowNum <- select "#rowNum" >>= getVal 
--   colNum <- select "#colNum" >>= getVal >>= text2Int
--   let innerTable =  "<table class=\"innerTable\"><tr><td></td><td></td></tr><tr><td></td><td></td></tr></table>"
--       tuples = zip [0..] (T.unpack input)
--       tdList = flip map tuples $ \(cellId, c) -> case c of
--                                                  '.' -> buildTd cellId innerTable
--                                                  otherwise -> buildTd cellId (show $ ord c - charCodeOffset)
--       table = "<table><tr>" ++ (concat $ intercalate ["</tr><tr>"] (chunk colNum tdList)) ++ "</tr></table>"
--   select "#field" >>= append (T.pack table)
--   return ()
--   where
--     buildTd :: Int -> String -> String
--     buildTd i s = "<tr><td id=\"" ++ show i ++ "\">" ++ s ++ "</td>"
--
-- start :: Event -> Fay Bool
-- start e = undefined
--
-- move :: Event -> Fay ()
-- move e = undefined
--
-- handler e = do
--   alert "hello"
--   return False
--
-- chunk :: Int -> [String] -> [[String]]
-- chunk n [] = [[]]
-- chunk n ss = take n ss : chunk n (drop n ss)
--
-- text2Int :: T.Text -> Fay Int
-- text2Int = ffi "%1"
