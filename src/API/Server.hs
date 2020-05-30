module API.Server where

import API.Endpoints        (API)
import API.Handlers         (getUsersHandler, uploadUsersHandler)
import Core.StreamingUpload (uploadApp)
import Interface.UI         (ui)
import Servant

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

server :: Server API
server = uploadUsersHandler
    :<|> serveDirectoryFileServer "static/largeupload"
    :<|> getUsersHandler
    :<|> Tagged uploadApp
    :<|> Tagged ui
