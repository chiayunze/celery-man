{-# LANGUAGE OverloadedStrings #-}

module Core.Service where

import           Core.Types           (Employee, EmployeeSalary, EmployeesTableField (Id, Login, Name, Salary),
                                       id, login, salary)
import           Data.ByteString.Lazy (ByteString)
import           Data.Csv             (HasHeader (HasHeader), decode)
import           Data.Scientific      (fromFloatDigits)
import           Data.Set             (fromList, size)
import           Data.Text            (Text, head, null, pack, tail)
import           Data.Vector          (Vector, filter, length, map, toList)
import qualified Data.Vector          as V (null)
import           Database.Postgres    (getUsersFromDB, importEmployeesToDB)
import           Prelude              hiding (filter, head, id, length, map,
                                       null, tail)

importUsers :: ByteString -> IO (Either Text ())
importUsers contents = case decode HasHeader contents of
    Left e -> return $ Left $ pack e -- csv parsing error
    Right employees -> do
        let filteredEmployees = filter (\employee -> head (id employee) /= '#') employees -- filter out #
        case validateCSVEmployees filteredEmployees of
                Left e'          -> return $ Left e' -- logic error within csv file
                Right employees' -> importEmployeesToDB employees'

validateCSVEmployees :: Vector Employee -> Either Text (Vector Employee)
validateCSVEmployees employees
    | V.null employees = Left "empty"
    | salariesNonNegative = Left "negative salaries are disallowed"
    | idsNotUnique = Left "ids must be unique"
    | loginsNotUnique = Left "logins must be unique"
    | otherwise = Right employees
    where salariesNonNegative = any (\employee -> salary employee < 0) employees
          employeeCount = length employees
          idsNotUnique = employeeCount /= (size . fromList . toList $ map id employees)
          loginsNotUnique = employeeCount /= (size . fromList . toList $ map login employees)

getUsers :: Double -> Double -> Int -> Int -> Text -> IO (Either Text ([Employee], Int))
getUsers minSalary maxSalary offset limit sort =
    case validateGetUsersParams minSalary maxSalary offset limit sort of
        Left e -> return $ Left e
        Right _ -> getUsers' minSalary' maxSalary' offset limit sortField sortAsc
            where   sortField = case tail sort of
                        "id"     -> Id
                        "login"  -> Login
                        "name"   -> Name
                        "salary" -> Salary
                        _        -> Id
                    sortAsc = head sort /= '-'
                    minSalary' = fromFloatDigits minSalary
                    maxSalary' = fromFloatDigits maxSalary

getUsers' :: EmployeeSalary -> EmployeeSalary -> Int -> Int -> EmployeesTableField -> Bool -> IO (Either Text ([Employee], Int))
getUsers' = getUsersFromDB

validateGetUsersParams :: Double -> Double -> Int -> Int -> Text -> Either Text ()
validateGetUsersParams minSalary maxSalary offset limit sort
    | minSalary > maxSalary = Left "minimum salary cannot exceed maximum salary"
    | minSalary < 0 = Left "minimum salary cannot be negative"
    | offset < 0 = Left "offset cannot be negative"
    | limit < 1 = Left "minimim for limit field is 1"
    | limit > 1000 = Left "maximum for limit field is 1000"
    -- we allow ' ' since that is what + encoded gives us
    | null sort || head sort `notElem` ['+', '-', ' '] = Left "sort prefix must be + or - only"
    | null sort || tail sort `notElem` ["id", "login", "name", "salary"] =
        Left "sort field must be id, login, name, or salary only (uncapitalized)"
    | otherwise = Right ()
