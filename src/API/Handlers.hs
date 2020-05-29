{-# LANGUAGE OverloadedStrings #-}

module API.Handlers where

import API.Endpoints          (GenericResponse (GenericResponseSuccess),
                               GetUsersResponse (GetUsersResponse))
import Control.Monad.IO.Class (liftIO)
import Core.Service           (getUsers, importUsers)
import Data.ByteString.Lazy   (fromStrict, readFile)
import Data.Text              (Text)
import Data.Text.Encoding     (encodeUtf8)
import Prelude                hiding (readFile)
import Servant                (Handler, throwError)
import Servant.Server         (err400, errBody)

uploadUsersHandler :: FilePath -> Handler GenericResponse
uploadUsersHandler filePath = do
    results <- liftIO $ importUsers =<< readFile filePath
    case results of
        Left e  -> throwError err400 { errBody = fromStrict . encodeUtf8 $ e }
        Right _ -> return GenericResponseSuccess

getUsersHandler :: Double -> Double -> Int -> Int -> Text -> Handler GetUsersResponse
getUsersHandler minSalary maxSalary offset limit sort = do
    results <- liftIO $ getUsers minSalary maxSalary offset limit sort
    case results of
        Left e -> throwError err400 { errBody = fromStrict . encodeUtf8 $ e }
        Right (users, _) -> return $ GetUsersResponse users
