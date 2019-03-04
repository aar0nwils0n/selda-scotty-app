module Main where

import Web.Scotty as Scotty
import Data.Aeson 
import Database.Selda
import Database.Selda.SQLite
import Database.Selda.Migrations
import GHC.Generics
import qualified ReqCustomer 
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Selda.Backend


data Customer = Customer { customerID :: ID Customer
    , name :: Text
    , country :: Text
} deriving (Generic, Show)

instance SqlRow Customer

reqToCustomer req = Customer {
    customerID = toId $ ReqCustomer.customerID req
    , name = ReqCustomer.name req
    , country = ReqCustomer.country req
}


instance ToJSON Customer where
    toJSON (Customer customerID name country) =
        object [ "customerID"  .= fromId customerID
                , "name"   .= name
                , "country" .= country
            ]


customers :: Table Customer
customers = table "customers" [#customerID :- autoPrimary]

main = 
    do
        conn <- sqliteOpen "customers.sqlite"
        runSeldaT (tryCreateTable customers) conn
        scotty 3000 $ do
            get "/customer" $ do
                id <- params
                customer <- liftIO $ runSeldaT getCustomers conn
                Scotty.json customer
            post "/customer" $ do
                req <- jsonData
                liftIO $ runSeldaT (insert_ customers [reqToCustomer req]) conn
                Scotty.json req
        

getCustomers = query $ do
    customer <- select customers
    return customer