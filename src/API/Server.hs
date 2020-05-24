module API.Server where

import API.Endpoints (API)
import API.Handlers  (createUserHandler, deleteUserHandler, getUserHandler,
                      getUsersHandler, updateUserHandler, uploadUsersHandler)
import Servant

api :: Proxy API
api = Proxy

app :: Application
app = serve api $
    uploadUsersHandler
    :<|> getUsersHandler
    :<|> getUserHandler
    :<|> createUserHandler
    :<|> updateUserHandler
    :<|> deleteUserHandler
