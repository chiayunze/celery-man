module Interface.UI where

import Concur.Core                   (step)
import Concur.Replica                (stepWidget)
import Interface.Index               (indexWithBootstrap)
import Interface.Layout.Handler      (handlerLayout)
import Network.Wai                   (Application)
import Network.Wai.Handler.Replica   (app)
import Network.WebSockets.Connection (defaultConnectionOptions)

ui :: Application
ui = app indexWithBootstrap defaultConnectionOptions id (step <$> const handlerLayout) stepWidget
