module Main where

import API.Server               (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8081 app
