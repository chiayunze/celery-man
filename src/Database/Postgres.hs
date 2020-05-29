{-# LANGUAGE OverloadedStrings #-}

module Database.Postgres where

import           Control.Exception.Safe           (try)
import           Core.Types                       (Employee (Employee),
                                                   EmployeeId, EmployeeLogin,
                                                   EmployeeName, EmployeeSalary,
                                                   EmployeesTableField (Id, Login, Name, Salary))
import qualified Data.ByteString                  as BS (append)
import           Data.ByteString.Char8            (pack)
import           Data.Int                         (Int64)
import           Data.Text                        (Text, append)
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import           Data.Vector                      (Vector, toList)
import           Database.PostgreSQL.Simple       (Connection, SqlError,
                                                   connectPostgreSQL,
                                                   executeMany, execute_, query,
                                                   sqlErrorDetail, sqlErrorMsg,
                                                   withTransaction)
import           Database.PostgreSQL.Simple.Types (Query (Query))
import           System.Environment               (getEnv)

getConn :: IO Connection
getConn = do
    user <- getEnv "PG_USER"
    password <- getEnv "PG_PASSWORD"
    host <- getEnv "PG_HOST"
    port <- getEnv "PG_PORT"
    let connString = "postgres://" ++ user ++ ":" ++ password ++ "@" ++ host ++ ":" ++ port
    connectPostgreSQL $ pack connString

formatErrorString :: SqlError -> Text
formatErrorString err = "DB error: "
    `append` decodeUtf8 (sqlErrorMsg err)
    `append` "\n"
    `append` decodeUtf8 (sqlErrorDetail err)

importEmployeesToDB :: Vector Employee -> IO (Either Text ())
importEmployeesToDB employees = do
    conn <- getConn
    importEmployeesToDB' conn employees

importEmployeesToDB' :: Connection -> Vector Employee -> IO (Either Text ())
importEmployeesToDB' conn employees = do

    let execute_' = execute_ conn
        executeMany' = executeMany conn
        employees' = toList employees

    result <- try $ withTransaction conn $ do
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
        Left e  -> return $ Left $ formatErrorString e
        Right _ -> return $ Right ()

getUsersFromDB :: EmployeeSalary -> EmployeeSalary -> Int -> Int -> EmployeesTableField -> Bool -> IO (Either Text ([Employee], Int))
getUsersFromDB minSalary maxSalary offset limit sortField sortAsc = do
    conn <- getConn
    getUsersFromDB' conn minSalary maxSalary offset limit sortField sortAsc

getUsersFromDB' :: Connection -> EmployeeSalary -> EmployeeSalary -> Int -> Int -> EmployeesTableField -> Bool -> IO (Either Text ([Employee], Int))
getUsersFromDB' conn minSalary maxSalary offset limit sortField sortAsc = do

    let sortAsc' = if sortAsc then "ASC" else "DESC"
        sortField' = case sortField of
            Id     -> "id"
            Name   -> "name"
            Login  -> "login"
            Salary -> "salary"
        queryTemplate = Query $ "WITH t AS \
            \(SELECT *, ROW_NUMBER() OVER \
            \(ORDER BY " `BS.append` sortField' `BS.append` " " `BS.append` sortAsc' `BS.append` ") as rownum \
            \FROM celery_man.employees WHERE salary >= ? AND salary <= ?), \
            \t2 AS (SELECT COUNT(*) AS records FROM t) \
            \SELECT id, login, name, salary, (SELECT records from t2) \
            \FROM t WHERE rownum > ? AND rownum <= ? + ?"
        substituteVars = (minSalary, maxSalary, offset, offset, limit)

    result <- try $ query conn queryTemplate substituteVars

    case result :: Either SqlError [(EmployeeId, EmployeeLogin, EmployeeName, EmployeeSalary, Int)] of
        Left e -> return $ Left $ formatErrorString e
        Right rows -> return $ Right (
            map (\(id', login, name, salary, _) -> Employee id' login name salary) rows
          , if null rows then 0 else (\(_, _, _, _, records) -> records) $ head rows
          )

createTempEmployeesTable :: Connection -> Text -> IO (Either Text ())
createTempEmployeesTable conn tempName = do
    let queryString = Query $ "CREATE TABLE celery_man.employees_temp_"
            `BS.append` encodeUtf8 tempName
            `BS.append` " (id VARCHAR(255) PRIMARY KEY, login VARCHAR(255) UNIQUE, name VARCHAR(255), salary NUMERIC(16, 2))"
    result <- try $ execute_ conn queryString
    case result :: Either SqlError Int64 of
        Left e  -> return $ Left $ formatErrorString e
        Right _ -> return $ Right ()

insertIntoTempEmployeesTable :: Connection -> Text -> [Employee] -> IO (Either Text ())
insertIntoTempEmployeesTable conn tempName employees = do
    let queryTemplate = Query $ "INSERT INTO celery_man.employees_temp_"
            `BS.append` encodeUtf8 tempName
            `BS.append`"(id, login, name, salary) VALUES (?, ?, ?, ?)"
    result <- try $ executeMany conn queryTemplate employees
    case result :: Either SqlError Int64 of
        Left e  -> return $ Left $ formatErrorString e
        Right _ -> return $ Right ()

deleteTempEmployeesTable :: Connection -> Text -> IO (Either Text ())
deleteTempEmployeesTable conn tempName = do
    let queryString = Query $ "DROP TABLE celery_man.employees_temp_"
                `BS.append` encodeUtf8 tempName
    result <- try $ execute_ conn queryString
    case result :: Either SqlError Int64 of
        Left e  -> return $ Left $ formatErrorString e
        Right _ -> return $ Right ()

transferTempToTarget :: Connection -> Text -> IO (Either Text ())
transferTempToTarget conn tempName = do
    let deleteQueryString = Query $ "DELETE FROM celery_man.employees \
            \ WHERE id IN (SELECT id FROM celery_man.employees_temp_"
            `BS.append` encodeUtf8 tempName `BS.append` ")"
        insertQueryString = Query $ "INSERT INTO celery_man.employees SELECT * FROM celery_man.employees_temp_"
            `BS.append` encodeUtf8 tempName
    result <- try $ withTransaction conn $ do
        _ <- execute_ conn deleteQueryString
        _ <- execute_ conn insertQueryString
        return ()
    case result :: Either SqlError () of
        Left e  -> return $ Left $ formatErrorString e
        Right _ -> return $ Right ()
