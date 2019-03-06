module Main where

import Web.Scotty as Scotty (get, post, scotty, json, param, jsonData, status)
import Data.Aeson 
import Database.Selda
import Database.Selda.SQLite
import Database.Selda.Migrations
import Customer
import Order
import GHC.Generics
import qualified ReqCustomer 
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Selda.Backend
import Network.HTTP.Types.Status (notFound404)
import FullOrder

main = 
    do
        conn <- sqliteOpen "customers.sqlite"
        runSeldaT createTables conn
        scotty 3000 $ do
            get "/customer" $ do
                customer <- liftIO $ runSeldaT (query getCustomers) conn
                Scotty.json customer
            get "/customer/:id" $ do
                id <- param "id"
                customers <- liftIO $ runSeldaT (query $ getCustomer id) conn
                case customers of 
                    [] ->
                        status notFound404
                    (customer:_) ->
                        Scotty.json customer
            post "/customer" $ do
                req <- jsonData
                liftIO $ runSeldaT (insert_ customers [reqToCustomer req]) conn
                Scotty.json req
            post "/order" $ do
                req <- jsonData
                liftIO $ runSeldaT (insert_ orders [reqToOrder req]) conn
            get "/order" $ do
                fullOrders <- liftIO $ runSeldaT (query getOrdersWithCustomer) conn
                Scotty.json $ orderTupToFullOrder fullOrders

                
                

createTables = do
    tryCreateTable customers
    tryCreateTable orders