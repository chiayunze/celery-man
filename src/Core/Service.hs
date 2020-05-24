{-# LANGUAGE OverloadedStrings #-}

module Core.Service where

import Core.Types           (Employee, id, login, salary)
import Data.ByteString.Lazy (readFile)
import Data.Csv             (HasHeader (HasHeader), decode)
import Data.Set             (fromList, size)
import Data.Text            (Text, head, pack)
import Data.Vector          (Vector, filter, length, map, toList)
import Database.Postgres    (importEmployeesToDB)
import Prelude              hiding (filter, head, id, length, map, readFile)

importUsers :: FilePath -> IO (Either Text ())
importUsers filePath = do
    contents <- readFile filePath
    case decode HasHeader contents of
        Left e -> return $ Left $ pack e -- csv parsing error
        Right employees -> do
            let filteredEmployees = filter (\employee -> head (id employee) /= '#') employees -- filter out #
            case validateCSVEmployees filteredEmployees of
                Left e'          -> return $ Left e' -- logic error within csv file
                Right employees' -> importEmployeesToDB employees'

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
