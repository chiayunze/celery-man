{-# LANGUAGE OverloadedStrings #-}

module Database.PostgresSpec where

import Core.Types                 (Employee (Employee),
                                   EmployeesTableField (Id, Login, Name, Salary))
import Data.ByteString            (ByteString)
import Data.Vector                (fromList)
import Database.Postgres          (getUsersFromDB', importEmployeesToDB')
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL,
                                   execute_, query_)
import Test.Hspec                 (Spec, afterAll, beforeAll, beforeWith,
                                   describe, it, shouldReturn)

connString :: ByteString
connString = "postgres://postgres:pw@localhost:5432" -- modify to connect to test db

setupDB :: IO Connection
setupDB = do
    conn <- connectPostgreSQL connString
    _ <- execute_ conn "CREATE SCHEMA celery_man;\
        \ CREATE TABLE celery_man.employees (\
        \ id VARCHAR(255) PRIMARY KEY,\
        \ login VARCHAR(255) UNIQUE,\
        \ name VARCHAR(255),\
        \ salary NUMERIC(16, 2));"
    return conn

teardownDB :: Connection -> IO ()
teardownDB conn = do
    _ <- execute_ conn "DROP TABLE celery_man.employees;\
        \DROP SCHEMA celery_man;"
    close conn

flushDB :: Connection -> IO Connection
flushDB conn = do
    _ <- execute_ conn "DELETE FROM celery_man.employees"
    return conn

spec :: Spec
spec = beforeAll setupDB $ -- before first spec items
        afterAll teardownDB $ -- after last spec item
        beforeWith flushDB $ do -- before every spec item

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
