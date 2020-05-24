{-# LANGUAGE OverloadedStrings #-}

module API.Handlers where

import API.Endpoints          (GenericResponse (GenericResponseSuccess),
                               GetUsersResponse)
import Control.Monad.IO.Class (liftIO)
import Core.Service           (importUsers)
import Core.Types             (Employee, EmployeeId)
import Data.ByteString.Lazy   (fromStrict)
import Data.Text              (Text)
import Data.Text.Encoding     (encodeUtf8)
import Servant                (Handler, NoContent, throwError)
import Servant.Server         (err400, err501, errBody)

uploadUsersHandler :: FilePath -> Handler GenericResponse
uploadUsersHandler filePath = do
    results <- liftIO $ importUsers filePath
    case results of
        Left e  -> throwError err400 { errBody = fromStrict . encodeUtf8 $ e }
        Right x -> return GenericResponseSuccess

getUsersHandler :: Maybe Double -> Maybe Double -> Maybe Int -> Maybe Int -> Maybe Text -> Handler GetUsersResponse
getUsersHandler minSalary maxSalary offset limit sort = throwError err501 { errBody = "undefined" }

getUserHandler :: EmployeeId -> Handler Employee
getUserHandler = undefined

createUserHandler :: EmployeeId -> Employee -> Handler GenericResponse
createUserHandler = undefined

updateUserHandler :: EmployeeId -> Employee -> Handler GenericResponse
updateUserHandler = undefined

deleteUserHandler :: EmployeeId -> Handler NoContent
deleteUserHandler = undefined
