{-# LANGUAGE OverloadedStrings #-}

module Database.PostgresSpec where

import Control.Exception.Safe     (try)
import Core.Types                 (Employee (Employee),
                                   EmployeesTableField (Id, Login, Name, Salary))
import Data.Vector                (fromList)
import Database.Postgres          (createTempEmployeesTable,
                                   deleteTempEmployeesTable, getConn,
                                   getUsersFromDB', importEmployeesToDB',
                                   insertIntoTempEmployeesTable,
                                   transferTempToTarget)
import Database.PostgreSQL.Simple (Connection, SqlError, close, execute_,
                                   query_, sqlErrorMsg)
import Test.Hspec                 (Spec, after, before, describe, it, shouldBe,
                                   shouldReturn)

setupDB :: IO Connection
setupDB = do
    conn <- getConn
    _ <- execute_ conn "CREATE SCHEMA celery_man;\
        \ CREATE TABLE celery_man.employees (\
        \ id VARCHAR(255) PRIMARY KEY,\
        \ login VARCHAR(255) UNIQUE,\
        \ name VARCHAR(255),\
        \ salary NUMERIC(16, 2));"
    return conn

teardownDB :: Connection -> IO ()
teardownDB conn = do
    _ <- execute_ conn "DROP SCHEMA celery_man CASCADE;"
    close conn

spec :: Spec
spec = before setupDB $
        after teardownDB $ do

            describe "getUsersFromDB" $  do

                it "is Right when import succeeds" $ \conn ->
                    importEmployeesToDB' conn (fromList [Employee "id" "login" "name" 100]) `shouldReturn` Right ()

                it "imports the correct entries" $ \conn -> do
                    let employees = [
                            Employee "id" "login" "name" 100
                          , Employee "id2" "login2" "name" 100
                          ]
                    _ <- importEmployeesToDB' conn (fromList employees)
                    query_ conn "SELECT * FROM celery_man.employees" `shouldReturn` employees

                it "should reject when id clashes" $ \conn -> do
                    let employees = [
                            Employee "id" "login" "name" 100
                          , Employee "id" "login2" "name" 100
                          ]
                    importEmployeesToDB' conn (fromList employees) `shouldReturn`
                        Left "DB error: duplicate key value violates unique constraint \"employees_temp_pkey\"\nKey (id)=(id) already exists."

                it "should reject when id clashes" $ \conn -> do
                    let employees = [
                            Employee "id" "login" "name" 100
                          , Employee "id2" "login" "name" 100
                          ]
                    importEmployeesToDB' conn (fromList employees) `shouldReturn`
                        Left "DB error: duplicate key value violates unique constraint \"employees_temp_login_key\"\nKey (login)=(login) already exists."

                it "should reject when inserts clash with existing logins" $ \conn -> do
                    _ <- execute_ conn "INSERT INTO celery_man.employees (id, login, name, salary) VALUES ('id', 'login', 'name', 100)"
                    importEmployeesToDB' conn (fromList [Employee "id2" "login" "name" 100]) `shouldReturn`
                        Left "DB error: duplicate key value violates unique constraint \"employees_login_key\"\nKey (login)=(login) already exists."

                it "should reject when updates clash with existing logins" $ \conn -> do
                    _ <- execute_ conn "INSERT INTO celery_man.employees (id, login, name, salary)\
                            \ VALUES ('id', 'login', 'name', 100), ('id2', 'login2', 'name', 100)"
                    importEmployeesToDB' conn (fromList [Employee "id2" "login" "name" 100]) `shouldReturn`
                        Left "DB error: duplicate key value violates unique constraint \"employees_login_key\"\nKey (login)=(login) already exists."

            describe "getUsersFromDB" $ do

                it "should return correct ordered users when sorted by id" $ \conn -> do
                    let employees = [
                            Employee "id" "login" "name" 100
                          , Employee "id2" "login2" "name" 100
                          ]
                    _ <- execute_ conn "INSERT INTO celery_man.employees (id, login, name, salary)\
                            \ VALUES ('id', 'login', 'name', 100), ('id2', 'login2', 'name', 100)"
                    getUsersFromDB' conn 0 200 0 30 Id True `shouldReturn` Right (employees, 2)

                it "should return correct ordered users when sorted by login" $ \conn -> do
                    let employees = [
                            Employee "id" "login345" "name" 100
                          , Employee "id2" "login123" "name" 100
                          ]
                    _ <- execute_ conn "INSERT INTO celery_man.employees (id, login, name, salary)\
                            \ VALUES ('id', 'login345', 'name', 100), ('id2', 'login123', 'name', 100)"
                    getUsersFromDB' conn 0 200 0 30 Login False `shouldReturn` Right (employees, 2)

                it "should return correct ordered users when sorted by name" $ \conn -> do
                    let employees = [
                            Employee "id2" "login2" "name123" 100
                          , Employee "id" "login" "name456" 100
                          ]
                    _ <- execute_ conn "INSERT INTO celery_man.employees (id, login, name, salary)\
                            \ VALUES ('id', 'login', 'name456', 100), ('id2', 'login2', 'name123', 100)"
                    getUsersFromDB' conn 0 200 0 30 Name True `shouldReturn` Right (employees, 2)

                it "should return correct number of users when sorted by salary" $ \conn -> do
                    let employees = [
                            Employee "id2" "login2" "name" 100
                          , Employee "id" "login" "name" 200
                          ]
                    _ <- execute_ conn "INSERT INTO celery_man.employees (id, login, name, salary)\
                            \ VALUES ('id', 'login', 'name', 200), ('id2', 'login2', 'name', 100)"
                    getUsersFromDB' conn 0 2000 0 30 Salary True `shouldReturn` Right (employees, 2)

                it "should return correct number of users when filtered by salary" $ \conn -> do
                    _ <- execute_ conn "INSERT INTO celery_man.employees (id, login, name, salary)\
                            \ VALUES ('id', 'login', 'name', 100), ('id2', 'login2', 'name', 1000)"
                    getUsersFromDB' conn 900 1000 0 30 Salary True `shouldReturn`
                        Right ([Employee "id2" "login2" "name" 1000], 1)

                it "should return [] when nothing in table" $ \conn ->
                    getUsersFromDB' conn 0 200 0 30 Id True `shouldReturn` Right ([], 0)

            describe "createTempEmployeesTable" $
                it "should create a table" $ \conn -> do
                    _<- createTempEmployeesTable conn "uuid"
                    query_ conn "SELECT * FROM celery_man.employees_temp_uuid" `shouldReturn` ([] :: [Employee])

            describe "insertIntoTempEmployeesTable" $
                it "should insert records" $ \conn -> do
                    let employees = [
                            Employee "id" "login" "name" 100
                          , Employee "id2" "login2" "name" 100
                          ]
                    _ <- execute_ conn "CREATE TABLE celery_man.employees_temp_uuid \
                        \(id VARCHAR(255) PRIMARY KEY, login VARCHAR(255) UNIQUE, name VARCHAR(255), salary NUMERIC(16, 2))"
                    _ <- insertIntoTempEmployeesTable conn "uuid" employees
                    query_ conn "SELECT * FROM celery_man.employees_temp_uuid" `shouldReturn` employees

            describe "deleteTempEmployeesTable" $
                it "should delete the table" $ \conn -> do
                    _ <- execute_ conn "CREATE TABLE celery_man.employees_temp_uuid \
                        \(id VARCHAR(255) PRIMARY KEY, login VARCHAR(255) UNIQUE, name VARCHAR(255), salary NUMERIC(16, 2))"
                    _ <- deleteTempEmployeesTable conn "uuid"
                    result <- try $ query_ conn "SELECT * FROM celery_man.employees_temp_uuid"
                    either sqlErrorMsg (const "") (result :: Either SqlError [Employee]) `shouldBe`
                        "relation \"celery_man.employees_temp_uuid\" does not exist"

            describe "transferTempToTarget" $
                it "should copy records" $ \conn -> do
                    let employees = [
                            Employee "id" "login" "name" 100
                          , Employee "id2" "login2" "name" 100
                          ]
                    _ <- execute_ conn "CREATE TABLE celery_man.employees_temp_uuid \
                        \(id VARCHAR(255) PRIMARY KEY, login VARCHAR(255) UNIQUE, name VARCHAR(255), salary NUMERIC(16, 2))"
                    _ <- execute_ conn "INSERT INTO celery_man.employees_temp_uuid (id, login, name, salary)\
                            \ VALUES ('id', 'login', 'name', 100), ('id2', 'login2', 'name', 100)"
                    _ <- transferTempToTarget conn "uuid"
                    query_ conn "SELECT * FROM celery_man.employees_temp_uuid" `shouldReturn` employees
