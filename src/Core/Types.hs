{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Core.Types where

import Data.Aeson   (FromJSON, ToJSON)
import Data.Csv     (FromRecord (parseRecord), (.!))
import Data.Text    (Text)
import GHC.Generics (Generic)

data Employee = Employee {
    id     :: EmployeeId
  , login  :: EmployeeLogin
  , name   :: EmployeeName
  , salary :: EmployeeSalary
} deriving (Generic)

type EmployeeId = Text
type EmployeeLogin = Text
type EmployeeName = Text
type EmployeeSalary = Double

instance FromJSON Employee
instance ToJSON Employee

instance FromRecord Employee where
    parseRecord v
        | length v == 4 = Employee <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3
        | otherwise     = fail "preceeding row does not have 4 columns"
