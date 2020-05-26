module Interface.Layout.Handler where

import Concur.Core                 (Widget)
import Concur.Replica              (HTML)
import Interface.Dashboard.Handler (handlerDashboard)
import Interface.Layout.Render     (renderLayout)
import Interface.Upload.Handler    (handlerUpload)

handlerLayout :: Widget HTML a
handlerLayout = renderLayout handlerUpload handlerDashboard
