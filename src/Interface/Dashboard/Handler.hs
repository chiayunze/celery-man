module Interface.Dashboard.Handler where

import Concur.Core                (Widget)
import Concur.Replica             (HTML)
import Control.Concurrent         (threadDelay)
import Control.Monad.IO.Class     (liftIO)
import Core.Service               (getUsers)
import Core.Types                 (EmployeesTableField (Id))
import Data.Scientific            (fromFloatDigits)
import Interface.Dashboard.Render (renderDashboard, renderEmployeeTable,
                                   renderFilterBox)

handlerDashboard :: Widget HTML a
handlerDashboard = do

    let loop params@(minSalary, maxSalary, sortField, sortAsc, page) = do

            let minSalary' = fromFloatDigits minSalary
                maxSalary' = fromFloatDigits maxSalary
                offset = (page - 1) * employeesPerPage
                employeesPerPage = 10 -- modify this value to show more employees on a single page

            eitherEmployees <- liftIO $ getUsers minSalary' maxSalary' offset employeesPerPage sortField sortAsc
            case eitherEmployees of
                Left err -> do
                    liftIO $ do
                        print err
                        threadDelay 60000000 -- retry after 60s upon failure (of retrievals from database)
                    loop params
                Right (employees, records) -> do
                    newParams <- renderDashboard
                        (renderFilterBox params)
                        (renderEmployeeTable params (ceiling (fromIntegral records / fromIntegral employeesPerPage :: Double)) employees)
                    loop newParams

    loop (0, 1000000, Id, True, 1)
