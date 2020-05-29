{-# LANGUAGE OverloadedStrings #-}

module Interface.Layout.Render where

import           Concur.Core              (Widget)
import           Concur.Replica           (HTML)
import qualified Concur.Replica.DOM       as H
import qualified Concur.Replica.DOM.Props as P

renderLayout :: Widget HTML a -> Widget HTML a -> Widget HTML a
renderLayout widget1 widget2 = H.div [P.className "window text-center"]
    [ H.h1 [] [H.text "Employee Salary Management"]
    , widget1
    , widget2
    ]
