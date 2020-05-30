{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module API.Endpoints where

import Core.Types        (Employee)
import Data.Aeson        (ToJSON, object, toJSON, (.=))
import Data.Functor      ((<&>))
import Data.Text         (Text)
import GHC.Generics      (Generic)
import Servant           ((:<|>), (:>), Get, JSON, Post, QueryParam', Raw,
                          Required)
import Servant.Multipart (FileData (fdFileCType, fdPayload), FromMultipart,
                          MultipartForm, Tmp, fromMultipart, lookupFile)

type API =
    -- POST /users/upload
        "users"
        :> "upload"
        :> MultipartForm Tmp FilePath
        :> Post '[JSON] GenericResponse
    -- GET /users/largeupload/streaming
    :<|> "users"
        :> "largeupload"
        :> Raw
    -- GET /users
    :<|> "users"
        :> QueryParam' '[Required] "minSalary" Double
        :> QueryParam' '[Required] "maxSalary" Double
        :> QueryParam' '[Required] "offset" Int
        :> QueryParam' '[Required] "limit" Int
        :> QueryParam' '[Required] "sort" Text
        :> Get '[JSON] GetUsersResponse
    -- ws /beta/users/upload large file upload endpoint
    :<|> "beta"
        :> "users"
        :> "upload"
        :> Raw
    -- concur-replica UI
    :<|> Raw

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
