{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Data.Text (Text)
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import Data.ByteString.Char8 (pack)

main = quickHttpServe site

site = ifTop homePage <|>
       route [ ("fun", method POST locationHandler) ]


homePage = do
    serveFile "static/info.html"

locationHandler = do
    postParams <- rqPostParams <$> getRequest
    logError . pack . show $ postParams
