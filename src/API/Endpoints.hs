{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module API.Endpoints where

import Core.Types        (Employee, EmployeeId)
import Data.Aeson        (ToJSON, object, toJSON, (.=))
import Data.Functor      ((<&>))
import Data.Text         (Text)
import GHC.Generics      (Generic)
import Servant           ((:<|>), (:>), Capture, DeleteNoContent, Get, JSON,
                          Patch, Post, QueryParam', ReqBody, Required)
import Servant.Multipart (FileData (fdFileCType, fdPayload), FromMultipart,
                          MultipartForm, Tmp, fromMultipart, lookupFile)

type API =
    -- POST /users/upload
        "users"
        :> "upload"
        :> MultipartForm Tmp FilePath
        :> Post '[JSON] GenericResponse
    -- GET /users
    :<|> "users"
        :> QueryParam' '[Required] "minSalary" Double
        :> QueryParam' '[Required] "maxSalary" Double
        :> QueryParam' '[Required] "offset" Int
        :> QueryParam' '[Required] "limit" Int
        :> QueryParam' '[Required] "sort" Text
        :> Get '[JSON] GetUsersResponse
    -- GET /users/{id}
    :<|> "users"
        :> Capture "id" EmployeeId
        :> Get '[JSON] Employee
    -- POST /users/{id}
    :<|> "users"
        :> Capture "id" EmployeeId
        :> ReqBody '[JSON] Employee
        :> Post '[JSON] GenericResponse
    -- PATCH /users/{id}
    :<|> "users"
        :> Capture "id" EmployeeId
        :> ReqBody '[JSON] Employee
        :> Patch '[JSON] GenericResponse
    -- DELETE /users/{id}
    :<|> "users"
        :> Capture "id" EmployeeId
        :> DeleteNoContent

instance FromMultipart Tmp FilePath where
    fromMultipart multipartData = lookupFile "file" multipartData >>= checkEncoding <&> fdPayload
        where checkEncoding f = if fdFileCType f == "text/csv" then Just f else Nothing

data GenericResponse = GenericResponseSuccess | GenericResponseFailure { error :: Text }

instance ToJSON GenericResponse where
    toJSON GenericResponseSuccess     = object ["success" .= True]
    toJSON (GenericResponseFailure e) = object ["success" .= False, "error" .= e]

newtype GetUsersResponse = GetUsersResponse
    { results :: [Employee]
    } deriving (Generic)

instance ToJSON GetUsersResponse
