{-# LANGUAGE OverloadedStrings #-}

module API.Handlers where

import API.Endpoints          (GenericResponse (GenericResponseSuccess),
                               GetUsersResponse (GetUsersResponse))
import Control.Monad.IO.Class (liftIO)
import Core.Service           (getUsers, importUsers)
import Core.Types             (EmployeesTableField (Id, Login, Name, Salary))
import Data.ByteString.Lazy   (fromStrict)
import Data.Scientific        (fromFloatDigits)
import Data.Text              (Text, head, null, tail)
import Data.Text.Encoding     (encodeUtf8)
import Prelude                hiding (head, null, tail)
import Servant                (Handler, throwError)
import Servant.Server         (err400, errBody)

uploadUsersHandler :: FilePath -> Handler GenericResponse
uploadUsersHandler filePath = do
    results <- liftIO $ importUsers filePath
    case results of
        Left e  -> throwError err400 { errBody = fromStrict . encodeUtf8 $ e }
        Right _ -> return GenericResponseSuccess

getUsersHandler :: Double -> Double -> Int -> Int -> Text -> Handler GetUsersResponse
getUsersHandler minSalary maxSalary offset limit sort
    | minSalary > maxSalary = throwError err400 { errBody = "minSalary cannot exceed maxSalary"}
    | minSalary < 0 = throwError err400 { errBody = "minSalary cannot be negative" }
    | offset < 0 = throwError err400 { errBody = "offset cannot be negative" }
    | limit > 1000 = throwError err400 { errBody = "maximum for limit field is 1000" }
    | null sort || head sort `notElem` ['+', '-', ' '] = throwError err400 { errBody = "sort prefix must be + or - only" }
    | null sort || tail sort `notElem` ["id", "login", "name", "salary"] = throwError err400 { errBody = "sort field must be id, login, name, or salary only (uncapitalized)" }
    | otherwise = do
        let sortField = case tail sort of
                "id"     -> Id
                "login"  -> Login
                "name"   -> Name
                "salary" -> Salary
                _        -> Id
            sortAsc = head sort /= '-'
            minSalary' = fromFloatDigits minSalary
            maxSalary' = fromFloatDigits maxSalary
        results <- liftIO $ getUsers minSalary' maxSalary' offset limit sortField sortAsc
        case results of
            Left e -> throwError err400 { errBody = fromStrict . encodeUtf8 $ e }
            Right (users, _) -> return $ GetUsersResponse users

-- getUserHandler :: EmployeeId -> Handler Employee
-- getUserHandler = undefined

-- createUserHandler :: EmployeeId -> Employee -> Handler GenericResponse
-- createUserHandler = undefined

-- updateUserHandler :: EmployeeId -> Employee -> Handler GenericResponse
-- updateUserHandler = undefined

-- deleteUserHandler :: EmployeeId -> Handler NoContent
-- deleteUserHandler = return $ throwError err501 { errBody = "undefined" }
