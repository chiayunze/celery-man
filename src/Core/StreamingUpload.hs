{-# LANGUAGE OverloadedStrings #-}

module Core.StreamingUpload where

import           Conduit                        (ConduitT, Void, chunksOfCE,
                                                 filterC, foldMC, iterMC, lift,
                                                 repeatWhileMC, runConduit,
                                                 (.|))
import           Control.Exception              (bracket)
import           Control.Monad.Trans.Except     (ExceptT (ExceptT), runExceptT,
                                                 throwE)
import           Core.Types                     (Employee (Employee))
import           Data.ByteString                (ByteString)
import           Data.Csv                       (HasHeader (HasHeader),
                                                 defaultDecodeOptions)
import           Data.Csv.Conduit               (CsvParseError (CsvParseError, IncrementalError),
                                                 fromCsvLiftError)
import           Data.Functor                   ((<&>))
import           Data.Text                      (Text, append, filter, head,
                                                 null, pack)
import           Data.UUID                      (toText)
import           Data.UUID.V4                   (nextRandom)
import           Database.Postgres              (createTempEmployeesTable,
                                                 deleteTempEmployeesTable,
                                                 getConn,
                                                 insertIntoTempEmployeesTable,
                                                 transferTempToTarget)
import qualified Database.PostgreSQL.Simple     as PG (Connection, close)
import           Network.HTTP.Types             (status400)
import           Network.Wai                    (Application, responseLBS)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.WebSockets             (Connection, ServerApp,
                                                 acceptRequest,
                                                 defaultConnectionOptions,
                                                 receiveData, sendClose,
                                                 sendTextData, withPingThread)
import           Prelude                        hiding (filter, head, mapM_,
                                                 null)

uploadApp :: Application
uploadApp = websocketsOr defaultConnectionOptions wsApp backupApp
    where   backupApp _ respond = respond $ responseLBS status400 [] "not ws"

wsApp :: ServerApp
wsApp pending = bracket
    allocateResources -- accept ws connection, acquire db connection, create temp table
    releaseResources -- delete temp table, close db and ws connections
    doStreamingImport -- parse, validate, insert in a streaming fashion, then send result message

    where   allocateResources = do
                conn <- acceptRequest pending
                pgConn <- getConn
                uuid <- nextRandom <&> toText <&> filter (/= '-')
                _ <- createTempEmployeesTable pgConn uuid
                return (conn, pgConn, uuid)

            releaseResources (conn, pgConn, uuid) = do
                _ <- deleteTempEmployeesTable pgConn uuid
                PG.close pgConn
                sendClose conn ("" :: Text)

            doStreamingImport (conn, pgConn, uuid) = withPingThread conn 30 (return ()) $ do
                result <- runExceptT $ runImportProcess conn pgConn uuid --
                case result of
                    Left e  -> sendTextData conn (printUploadError e)
                    Right _ -> sendTextData conn ("import successful!" :: Text)

-- definitions for the kinds of errors we will encounter
data UploadError = InvalidCSV Text | InvalidSalary Employee | ClashWithinRecords Text | ClashWithExisting Text
printUploadError :: UploadError -> Text
printUploadError e = case e of
    InvalidCSV t -> "parsing errors with csv: " `append` t
    InvalidSalary (Employee id' login name salary) -> "salary cannot be negative: "
        `append` id' `append` " " `append` login `append` " " `append` name `append` " " `append` (pack . show $ salary)
    ClashWithinRecords _ -> "imports do not have unique ids or logins"
    ClashWithExisting _ -> "imports will result in non-unique ids or logins when applied to existing records"

-- define our monad transformer for running the import
type UploadT a = ExceptT UploadError IO a

-- core logic for import process
runImportProcess :: Connection -> PG.Connection -> Text -> UploadT ()
runImportProcess conn pgConn uuid = do
    leftovers <- runConduit $ pipeline conn pgConn uuid                         -- run pipeline (see below), return last chunk
    ExceptT . clashWithin $ insertIntoTempEmployeesTable pgConn uuid leftovers  -- insert last chunk
    ExceptT . clashExisting $ transferTempToTarget pgConn uuid                  -- copy temp to main table
    return ()
    where   transformSQLError errorType = either (Left . errorType) Right
            clashWithin = fmap (transformSQLError ClashWithinRecords)
            clashExisting = fmap (transformSQLError ClashWithExisting)

-- conduit for stream processing, returns the final chunk yet to be inserted (i.e. totalRecords moduluo chunkSize n)
pipeline :: Connection -> PG.Connection -> Text -> ConduitT () Void (ExceptT UploadError IO) [Employee]
pipeline conn pgConn uuid =
    repeatWhileMC (lift $ receiveData conn) (/= "COMPLETE") -- read websocket messages until "COMPLETE" is received
    .| chunksOfCE 1                                         -- stream individual bytes
    .| csvToEmployeesConduit                                -- parse bytes into employee records
    .| filterC notComment                                   -- filter out records which are comments
    .| iterMC salaryValid                                   -- validate that salaries are non-negative
    .| foldMC (flushToDB pgConn uuid 500) []                -- perform a database insert every n records (i.e. flush our memory)

csvToEmployeesConduit :: ConduitT ByteString Employee (ExceptT UploadError IO)  ()
csvToEmployeesConduit = fromCsvLiftError handleParseException defaultDecodeOptions HasHeader
    where   handleParseException e = case e of
                CsvParseError _ t  -> InvalidCSV t
                IncrementalError t -> InvalidCSV t

notComment :: Employee -> Bool
notComment (Employee id' _ _ _) = not (null id') && head id' /= '#'

salaryValid :: Employee -> UploadT ()
salaryValid employee@(Employee _ _ _ salary) = if salary >= 0
    then return ()
    else throwE $ InvalidSalary employee

flushToDB :: PG.Connection -> Text -> Int -> [Employee] -> Employee -> UploadT [Employee]
flushToDB pgConn tempTableUuid cacheSize cache employee = do
    let newCache = employee:cache
    if length newCache > cacheSize
        then do
            result <- lift $ insertIntoTempEmployeesTable pgConn tempTableUuid newCache
            case result of
                Right _ -> return []
                Left e  -> throwE $ ClashWithinRecords e
        else return newCache
