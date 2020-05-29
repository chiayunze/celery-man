{-# LANGUAGE OverloadedStrings #-}

module Interface.Upload.Render where

import           Concur.Core              (Widget)
import           Concur.Replica           (HTML)
import qualified Concur.Replica.DOM       as H
import qualified Concur.Replica.DOM.Props as P

renderUploader :: Widget HTML a -> Widget HTML a -> Widget HTML a
renderUploader field message = H.div [P.className "border border-primary", P.style [("margin", "1em")]]
    [ H.strong [] [H.text "Upload"]
    , field
    , H.small [] [message]
    ]

renderUploadField :: Widget HTML a
renderUploadField = H.div []
    [ H.input [P.type_ "file", P.id "upload"]
    , H.button [P.className "btn btn-outline-secondary", P.type_ "button", P.id "basicUpload"] [ H.text "Upload"]
    ]

renderUploadMessage :: Widget HTML a
renderUploadMessage = H.text "Upload csv file to import into application"
