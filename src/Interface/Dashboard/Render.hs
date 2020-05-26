{-# LANGUAGE OverloadedStrings #-}

module Interface.Dashboard.Render where

import           Concur.Core               (Widget)
import           Concur.Replica            (HTML)
import qualified Concur.Replica.DOM        as H
import qualified Concur.Replica.DOM.Events as E
import qualified Concur.Replica.DOM.Props  as P
import           Core.Types                (Employee (Employee), EmployeesTableField (Id, Login, Name, Salary))
import           Data.Scientific           (FPFormat (Fixed), formatScientific)
import           Data.Text                 (append, pack, unpack)

type FilterParams = (Double, Double, EmployeesTableField, Bool, Int)

renderDashboard :: Widget HTML FilterParams -> Widget HTML FilterParams -> Widget HTML FilterParams
renderDashboard filters table = H.div []
    [ H.div [P.className "border border-primary", P.style [("margin", "1em")]] [H.strong [] [H.text "Filters"], filters]
    , H.div [P.className "border border-primary", P.style [("margin", "1em")]] [H.strong [] [H.text "Employees"], table]
    ]

data FilterBoxWidgetAction = FBWAMinSalary Double | FBWAMaxSalary Double | FBWASortField EmployeesTableField | FBWASortAsc | FBWASubmit

renderFilterBox :: FilterParams -> Widget HTML FilterParams
renderFilterBox (minSalary, maxSalary, sortField, sortAsc, currentPage) = do

    action <- H.div []
        [ H.div [] [H.text "Min Salary ",  minSalaryBox]
        , H.div [] [H.text "Max Salary ", maxSalaryBox]
        , H.div [] [H.text "Sort by ", sortFieldBox]
        , H.div [] [H.text "Sort ascending? ", sortAscBox]
        , filterButton
        ]

    case action of
        FBWAMinSalary minSalary' -> renderFilterBox (minSalary', maxSalary, sortField, sortAsc, currentPage)
        FBWAMaxSalary maxSalary' -> renderFilterBox (minSalary, maxSalary', sortField, sortAsc, currentPage)
        FBWASortField sortField' -> renderFilterBox (minSalary, maxSalary, sortField', sortAsc, currentPage)
        FBWASortAsc -> renderFilterBox (minSalary, maxSalary, sortField, not sortAsc, currentPage)
        FBWASubmit -> return (minSalary, maxSalary, sortField, sortAsc, 1)

        where   minSalaryBox = FBWAMinSalary . read . unpack <$> H.input
                    [P.type_ "number", P.value $ pack . show $ minSalary, E.targetValue . E.target <$> E.onChange]
                maxSalaryBox = FBWAMaxSalary . read . unpack <$> H.input
                    [P.type_ "number", P.value $ pack . show $ maxSalary, E.targetValue . E.target <$> E.onChange]
                sortFieldBox = FBWASortField . read . unpack <$> H.select
                    [E.targetValue . E.target <$> E.onChange, P.value $ pack . show $ sortField]
                    [H.option [P.value $ pack . show $ x] [H.text $ pack . show $ x] | x <- [Id, Name, Login, Salary]]
                sortAscBox = FBWASortAsc <$ H.input [P.type_ "checkbox", P.id "sortAsc", E.targetValue . E.target <$> E.onChange, P.checked sortAsc]
                filterButton = FBWASubmit <$ H.button [E.onClick, P.className "btn btn-outline-secondary"] [H.text "Apply Filters"]

renderEmployeeTable :: FilterParams -> Int -> [Employee] -> Widget HTML FilterParams
renderEmployeeTable (minSalary, maxSalary, sortField, sortAsc, selectedPage) maxPage employees = do
    page <- H.div []
        [ H.div [P.className "table table-responsive"] [H.table [P.className "mx-auto"] (header : body)]
        , H.div [] [H.small [] [showCurrentFilters]]
        , H.div [] [H.text "Go to page: ", pageSelector]
        ]

    return (minSalary, maxSalary, sortField, sortAsc, page)

    where   pageSelector = read . unpack <$> H.select
                [E.targetValue . E.target <$> E.onChange, P.value $ pack . show $ selectedPage]
                [H.option [P.value $ pack . show $ x] [H.text $ pack . show $ x] | x <- [1..maxPage]]
            showCurrentFilters = H.text $
                "Salary: [" `append` (pack . show $ minSalary) `append`
                " - " `append` (pack . show $ maxSalary) `append`
                "] Sorted by: [" `append` (pack . show $ sortField) `append` "] Sort order: "
                `append` if sortAsc then " [Ascending]" else " [Descending]"

            renderRow (Employee id' login name salary) = H.tr []
                [ H.td [] [H.text id']
                , H.td [] [H.text name]
                , H.td [] [H.text login]
                , H.td [] [H.text $ pack $ formatScientific Fixed (Just 2) salary]
                , H.td [] [H.button [] [H.text "modify"], H.button [] [H.text "delete"]]
                ]
            body = map renderRow employees
            header = H.tr []
                [ H.th [] [H.text "Id"]
                , H.th [] [H.text "Name"]
                , H.th [] [H.text "Login"]
                , H.th [] [H.text "Salary"]
                , H.th [] [H.text "Action"]
                ]
