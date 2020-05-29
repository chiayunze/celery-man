{-# LANGUAGE OverloadedStrings #-}

module Core.ServiceSpec where

import Core.Service          (validateCSVEmployees, validateGetUsersParams)
import Core.Types            (Employee (Employee))
import Data.Text             (append)
import Data.Text.Arbitrary   ()
import Data.Vector           (empty, fromList)
import Test.Hspec            (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSize)
import Test.QuickCheck       (property)

spec :: Spec
spec = do

    describe "validateCSVEmloyees" $ do

        it "is Right when given a good sample" $ do
            let employees = fromList [
                    Employee "id" "login" "name" 100
                  , Employee "id2" "login2" "name" 100
                  ]
            validateCSVEmployees employees `shouldBe` Right employees

        it "is Left when a salary is negative" $ do
            let employees = fromList [Employee "id" "login" "name" (-100)]
            validateCSVEmployees employees `shouldBe` Left "negative salaries are disallowed"

        it "is Left when ids are not unique" $ do
            let employees = fromList [
                    Employee "id" "login" "name" 100
                  , Employee "id" "login2" "name2" 100
                  ]
            validateCSVEmployees employees `shouldBe` Left "ids must be unique"

        it "is Left when logins are not unique" $ do
            let employees = fromList [
                    Employee "id" "login" "name" 100
                  , Employee "ids" "login" "name" 100
                  ]
            validateCSVEmployees employees `shouldBe` Left "logins must be unique"

        it "is Left when empty" $ validateCSVEmployees empty `shouldBe` Left "empty"

    describe "validateGetUsersParams" $ do

        it "is Right when given good inputs" $
            validateGetUsersParams 0 1000 0 30 "+id" `shouldBe` Right ()

        it "is Left when minSalary is negative" $
            validateGetUsersParams (-1) 1000 0 30 "+id" `shouldBe`
                Left "minimum salary cannot be negative"

        it "is Left when minSalary exceeds maxSalary" $
            validateGetUsersParams 10000 1000 0 30 "+id" `shouldBe`
                Left "minimum salary cannot exceed maximum salary"

        it "is Left when offset is negative" $
            validateGetUsersParams 0 1000 (-1) 30 "+id" `shouldBe`
                Left "offset cannot be negative"

        it "is Left when limit is negative" $
            validateGetUsersParams 0 1000 0 (-30) "+id" `shouldBe`
                Left "minimim for limit field is 1"

        it "is Left when limit is zero" $
            validateGetUsersParams 0 1000 0 0 "+id" `shouldBe`
                Left "minimim for limit field is 1"

        it "is Left when limit exceeds 1000" $
            validateGetUsersParams 0 1000 0 1001 "+id" `shouldBe`
                Left "maximum for limit field is 1000"

        it "is Left when sort prefix is invalid" $
            validateGetUsersParams 0 1000 0 30 "invalid" `shouldBe`
                Left "sort prefix must be + or - only"

        it "is Left when sort column is invalid" $
            validateGetUsersParams 0 1000 0 30 "+invalid" `shouldBe`
                Left "sort field must be id, login, name, or salary only (uncapitalized)"

        modifyMaxSize (const 1) $ it "has sort prefix quickchecked" $ property $
            \x -> validateGetUsersParams 0 1000 0 30 (x `append` "id") == Left "sort prefix must be + or - only"

        modifyMaxSize (const 10) $ it "has sort suffix quickchecked" $ property $
            \x -> validateGetUsersParams 0 1000 0 30 ("+" `append` x) ==
                if x `notElem` ["id", "login", "name", "salary"]
                    then Left "sort field must be id, login, name, or salary only (uncapitalized)"
                    else Right ()
