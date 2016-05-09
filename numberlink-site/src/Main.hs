{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import System.FilePath.Posix (takeFileName)
import Data.ByteString.Char8 (unpack)

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
    f $ responseFile status200 [(hContentType, "text/html")] (publicDir ++ "/" ++ filename) Nothing
    -- f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"

publicDir = "resources"
