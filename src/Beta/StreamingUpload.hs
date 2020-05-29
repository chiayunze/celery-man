{-# LANGUAGE OverloadedStrings #-}

module Beta.StreamingUpload where

import           Conduit                        (ConduitT, Void, filterC,
                                                 foldMC, iterMC, repeatWhileMC,
                                                 runConduit, (.|))
import           Control.Exception.Safe         (throw, try)
import           Core.Types                     (Employee (Employee))
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS (length)
import           Data.Csv                       (HasHeader (HasHeader),
                                                 defaultDecodeOptions)
import           Data.Csv.Conduit               (CsvParseError (CsvParseError, IncrementalError),
                                                 fromCsvLiftError)
import           Data.Functor                   ((<&>))
import           Data.Text                      (Text, filter, head, null,
                                                 unpack)
import           Data.UUID                      (toText)
import           Data.UUID.V4                   (nextRandom)
import           Database.Postgres              (createTempEmployeesTable,
                                                 deleteTempEmployeesTable,
                                                 getConn,
                                                 insertIntoTempEmployeesTable,
                                                 transferTempToTarget)
import qualified Database.PostgreSQL.Simple     as PG (Connection)
import           Network.HTTP.Types             (status400)
import           Network.Wai                    (Application, responseLBS)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.WebSockets             (Connection, ServerApp,
                                                 acceptRequest,
                                                 defaultConnectionOptions,
                                                 receiveData, sendClose,
                                                 withPingThread)
import           Prelude                        hiding (filter, head, mapM_,
                                                 null)

uploadApp :: Application
uploadApp = websocketsOr defaultConnectionOptions wsApp backupApp
    where   backupApp _ respond = respond $ responseLBS status400 [] "not ws"

pipeline :: Connection -> PG.Connection -> Text -> ConduitT () Void IO [Employee]
pipeline conn pgConn uuid = repeatWhileMC (receiveData conn) (/= "COMPLETE") -- read websocket messages until "COMPLETE" is received
             .| iterMC (print . BS.length) -- print bytes received, TODO: return it to UI
             .| csvToEmployeesConduit -- parse bytes into employee records
             .| filterC notComment -- filter out records which are comments
             .| iterMC salaryValid -- validate that salaries are non-negative, if negative raise exception which will stop the pipeline, TODO: fix this impurity
             .| foldMC (flushToDB pgConn uuid 3) [] -- perform a database insert every n records (i.e. maximum records to keep in memory)
             -- returns the leftover records yet to be inserted (i.e number of leftover records will be totalRecords moduluo n)

csvToEmployeesConduit :: ConduitT ByteString Employee IO ()
csvToEmployeesConduit = fromCsvLiftError handleParseException defaultDecodeOptions HasHeader

handleParseException :: CsvParseError -> IOError
handleParseException e = case e of
    CsvParseError _ t  -> userError $ unpack t
    IncrementalError t -> userError $ unpack t

notComment :: Employee -> Bool
notComment (Employee id' _ _ _) = not (null id') && head id' /= '#'

salaryValid :: Employee -> IO ()
salaryValid employee@(Employee _ _ _ salary) = if salary >= 0
    then return ()
    else throw $ userError ("salary invalid for " ++ show employee)

flushToDB :: PG.Connection -> Text -> Int -> [Employee] -> Employee -> IO [Employee] -- TODO: replace with Reader monad
flushToDB pgConn tempTableUuid cacheSize cache employee = do
    let newCache = employee:cache
    if length newCache > cacheSize
        then do
            result <- insertIntoTempEmployeesTable pgConn tempTableUuid newCache
            case result of
                Right _ -> return []
                Left e  -> throw $ userError (unpack e)
        else return newCache

wsApp :: ServerApp
wsApp pending = do

    -- accept websocket connection, acquire database connection, generate uuid for temp table
    conn <- acceptRequest pending
    pgConn <- getConn
    uuid <- nextRandom <&> toText <&> filter (/= '-')
    print uuid

    -- create a temp table in the database instance
    x <- createTempEmployeesTable pgConn uuid
    print x

    -- ping the connection every 30s to ensure it is kept alive
    withPingThread conn 30 (return ()) $ do

        -- first message is the total size of the file
        numBytes <- receiveData conn
        print (numBytes :: Text) -- TODO: render it on the UI

        -- run parsing, validation, inserting into temp table in a streaming fashion (see "pipeline" function)
        exceptLeftovers <- try $ runConduit $ pipeline conn pgConn uuid

        case exceptLeftovers :: Either IOError [Employee] of

            -- if fail, print error message
            Left e -> do
                print ("exception occurred" :: Text)
                print e

            -- if successful, returns last chunk of employee records
            Right leftoverEmployees -> do

                -- insert the last chunk of employee records
                result <- insertIntoTempEmployeesTable pgConn uuid leftoverEmployees
                -- TODO: use monad instances to clean up error handling
                case result of
                    Right _ -> do

                        -- if insert successful, copy the temp table into the main table
                        result' <- transferTempToTarget pgConn uuid
                        case result' of

                            Right _ -> do

                                -- if copy successful, delete the temp table, print ok, close the websocket connection
                                _  <- deleteTempEmployeesTable pgConn uuid

                                -- send a friendly close message to close the websocket connection
                                sendClose conn ("ok import complete!" :: Text)
                                print ("ok" :: Text)

                            Left e -> print e

                    Left e -> throw $ userError (unpack e)

