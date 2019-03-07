module Main where

import Web.Scotty as Scotty (get, post, scotty, json, param, jsonData, status, notFound, defaultHandler)
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
import Network.HTTP.Types.Status (notFound404, internalServerError500)
import FullOrder

main = 
    do
        conn <- sqliteOpen "customers.sqlite"
        runSeldaT createTables conn
        scotty 3000 $ do
            defaultHandler $ \e -> do
                status internalServerError500
                liftIO $ print e
                Scotty.json e
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
                let customer = reqToCustomer req
                id <- liftIO $ runSeldaT (insertWithPK customers [customer]) conn
                Scotty.json $ fromId id
            post "/order" $ do
                req <- jsonData
                let order = reqToOrder req
                id <- liftIO $ runSeldaT (insertWithPK orders [order]) conn
                Scotty.json $ fromId id
            get "/order" $ do
                fullOrders <- liftIO $ runSeldaT (query getOrdersWithCustomer) conn
                Scotty.json $ orderTupToFullOrder fullOrders
            notFound $ do
                status notFound404
                Scotty.json Null

                
createTables = do
    tryCreateTable customers
    tryCreateTable orders