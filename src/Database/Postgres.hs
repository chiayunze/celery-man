{-# LANGUAGE OverloadedStrings #-}

module Database.Postgres where

import Control.Exception.Safe     (try)
import Core.Types                 (Employee)
import Data.ByteString.Char8      (pack)
import Data.Text                  (Text, append)
import Data.Text.Encoding         (decodeUtf8)
import Data.Vector                (Vector, toList)
import Database.PostgreSQL.Simple (SqlError, connectPostgreSQL, executeMany,
                                   execute_, sqlErrorDetail, sqlErrorMsg,
                                   withTransaction)
import System.Environment         (getEnv)

getConnString :: IO String
getConnString = do
    user <- getEnv "PG_USER"
    password <- getEnv "PG_PASSWORD"
    host <- getEnv "PG_HOST"
    port <- getEnv "PG_PORT"
    return $ "postgres://" ++ user ++ ":" ++ password ++ "@" ++ host ++ ":" ++ port

importEmployeesToDB :: Vector Employee -> IO (Either Text ())
importEmployeesToDB employees = do

    let employees' = toList employees

    connString <- fmap pack getConnString
    conn <- connectPostgreSQL connString

    result <- try $ withTransaction conn $ void $ do

        let execute_' = execute_ conn
            executeMany' = executeMany conn

        -- create temp table and dump new into temp table
        _ <- execute_' "CREATE TABLE celery_man.employees_temp (id VARCHAR(255) PRIMARY KEY, login VARCHAR(255) UNIQUE, name VARCHAR(255), salary NUMERIC(16, 2))"
        _ <- executeMany' "INSERT INTO celery_man.employees_temp (id, login, name, salary) VALUES (?, ?, ?, ?)" employees'

        -- delete rows in target where in temp table
        _ <- execute_' "DELETE FROM celery_man.employees WHERE id IN (SELECT id FROM celery_man.employees_temp)"

        -- copy temp table into target table
        _ <- execute_' "INSERT INTO celery_man.employees SELECT * FROM celery_man.employees_temp"

        -- delete temp table
        _ <- execute_' "DROP TABLE celery_man.employees_temp"

        return ()

    case result :: Either SqlError () of
        Left e  -> return $ Left $ "DB error: " `append` decodeUtf8 (sqlErrorMsg e) `append` "\n" `append` decodeUtf8 (sqlErrorDetail e)
        Right _ -> return $ Right ()
