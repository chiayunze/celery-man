module Interface.Upload.Handler where

import Concur.Core             (Widget)
import Concur.Replica          (HTML)
import Interface.Upload.Render (renderUploadField, renderUploadMessage,
                                renderUploader)

handlerUpload :: Widget HTML a
handlerUpload = renderUploader renderUploadField renderUploadMessage
