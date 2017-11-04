{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import System.FilePath.Posix (takeFileName, takeExtension)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Char8 as B
import Blaze.ByteString.Builder.ByteString

main :: IO ()
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app req f = do
    print $ requestMethod req
    print $ rawPathInfo req
    let filepath = publicFilePath $ takeFileName $ B.unpack $ rawPathInfo req
        extname = takeExtension $ B.unpack $ rawPathInfo req
        contentType = "text/" `B.append` B.pack (tail extname)
    exists <- doesFileExist filepath
    if exists
      then do
        builder <- fromByteString <$> B.readFile filepath
        f $ responseBuilder status200 [(hContentType, contentType)] builder
      else f $ responseBuilder status404 [] $ fromByteString "Not Found"

publicFilePath :: String -> String
publicFilePath filename = publicDir ++ "/" ++ filename

publicDir = "resources"
