{-# LANGUAGE OverloadedStrings #-}

module Core.Service where

import Core.Types           (Employee, id, login, salary)
import Data.ByteString.Lazy (readFile)
import Data.Csv             (HasHeader (HasHeader), decode)
import Data.Set             (fromList, size)
import Data.Text            (Text, pack)
import Data.Vector          (Vector, length, map, toList)
import Database.Postgres    (tryUpdateDatabase)
import Prelude              hiding (id, length, map, readFile)

importUsers :: FilePath -> IO (Either Text Int)
importUsers filePath = do
    contents <- readFile filePath
    case decode HasHeader contents of
        Left e -> return $ Left $ pack e -- csv parsing error
        Right employees -> case validateCSVEmployees employees of
            Left e'          -> return $ Left e' -- logic error within csv file
            Right employees' -> tryUpdateDatabase employees'

validateCSVEmployees :: Vector Employee -> Either Text (Vector Employee)
validateCSVEmployees employees
    | salariesNonNegative = Left "csv error: negative salaries are disallowed"
    | idsNotUnique = Left "csv error: ids must be unique"
    | loginsNotUnique = Left "csv error: logins must be unique"
    | otherwise = Right employees
    where salariesNonNegative = any (\employee -> salary employee < 0) employees
          employeeCount = length employees
          idsNotUnique = employeeCount /= (size . fromList . toList $ map id employees)
          loginsNotUnique = employeeCount /= (size . fromList . toList $ map login employees)
