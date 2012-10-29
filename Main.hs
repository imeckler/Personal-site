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
import Snap.Snaplet.Session
import Text.Templating.Heist
import Snap.StaticPages
import qualified Data.ByteString.Char8 as B

import Debug.Trace

data App = App
    { _heist  :: Snaplet (Heist App)
    , _blog   :: Snaplet StaticPages
    --, _sess   :: Snaplet SessionManager
    }

makeLenses [''App]

instance HasHeist App where heistLens = subSnaplet heist

backgrounds = map (B.append "static/images/background/") 
    [ "tarp.jpg"
    , "fabric1small.jpg"
    , "fabric2.jpg"
    ]



appInit :: SnapletInit App App
appInit = makeSnaplet "personal-site" "" Nothing $ do
    hs <- nestSnaplet "" heist $ heistInit "templates"
    bs <- nestSnaplet "blog" blog $ staticPagesInit "blogdata"
    --sess <- nestSnaplet 
    addRoutes [ ("/fun", method POST locationHandler)
              , ("static", serveDirectory "static")
              , ("/", redirect "blog")
              ]

    return $ App hs bs

main = serveSnaplet defaultConfig appInit

locationHandler = do
    postParams <- rqPostParams <$> getRequest
    logError . B.pack . show $ postParams
