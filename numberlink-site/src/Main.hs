{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

main :: IO ()
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app req f = do
    print $ requestMethod req
    print $ rawPathInfo req
    f $ responseFile status200 [(hContentType, "text/html")] "a.html" Nothing
    -- f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"
