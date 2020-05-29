module API.Server where

import API.Endpoints (API)
import API.Handlers  (getUsersHandler, uploadUsersHandler)
import Interface.UI  (ui)
import Beta.StreamingUpload (uploadApp)
import Servant

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

server :: Server API
server = uploadUsersHandler
    :<|> getUsersHandler
    :<|> Tagged uploadApp
    :<|> Tagged ui
