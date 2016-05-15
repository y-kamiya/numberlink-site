{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import System.FilePath.Posix (takeFileName, takeExtension)
import Data.ByteString.Char8 (unpack, pack, append)

main :: IO ()
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app req f = do
    print $ requestMethod req
    print $ rawPathInfo req
    let filename = takeFileName $ unpack $ rawPathInfo req
    let extname = takeExtension $ unpack $ rawPathInfo req
    let contentType = "text/" `append` pack (tail extname)
    f $ responseFile status200 [(hContentType, contentType)] (publicDir ++ "/" ++ filename) Nothing

publicDir = "resources"
