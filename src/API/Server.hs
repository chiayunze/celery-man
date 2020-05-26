module API.Server where

import API.Endpoints (API)
import API.Handlers  (getUsersHandler, uploadUsersHandler)
import Interface.UI  (ui)
import Servant

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

server :: Server API
server = uploadUsersHandler
    :<|> getUsersHandler
    -- :<|> getUserHandler
    -- :<|> createUserHandler
    -- :<|> updateUserHandler
    -- :<|> deleteUserHandler
    :<|> Tagged ui
