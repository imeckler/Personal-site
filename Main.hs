{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative
import Data.Text (Text)
import Snap
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import Snap.Snaplet
import Snap.Snaplet.Heist
import Text.Templating.Heist
import Snap.StaticPages
import Data.ByteString.Char8 (pack)

import Debug.Trace

data App = App
    { _heist     :: Snaplet (Heist App)
    , _blog      :: Snaplet StaticPages
    }

makeLenses [''App]

instance HasHeist App where heistLens = subSnaplet heist

appInit :: SnapletInit App App
appInit = makeSnaplet "personal-site" "" Nothing $ do
    hs <- nestSnaplet "" heist $ heistInit "templates"
    bs <- nestSnaplet "blog" blog $ staticPagesInit "blogdata"
    addRoutes [ ("/fun", method POST locationHandler)
              , ("static", serveDirectory "static")
              ]

    return $ App hs bs

main = serveSnaplet defaultConfig appInit

locationHandler = do
    postParams <- rqPostParams <$> getRequest
    logError . pack . show $ postParams
