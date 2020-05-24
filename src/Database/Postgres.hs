{-# LANGUAGE OverloadedStrings #-}

module Database.Postgres where

import Core.Types  (Employee (Employee))
import Data.Text   (Text)
import Data.Vector (Vector)

tryUpdateDatabase :: Vector Employee -> IO (Either Text Int)
tryUpdateDatabase employees = return $ Right 1

employeesDB :: [Employee]
employeesDB = [
    Employee "e0001" "hpotter" "Harry Potter" 1234.50
  , Employee "e0002" "rwesley" "Ron Weasley" 19234.50
  , Employee "e0007" "hgranger" "Hermione Granfger" 0.00
  ]
