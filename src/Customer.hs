module Customer where

import Data.Aeson 
import qualified ReqCustomer 
import GHC.Generics
import Database.Selda
import Database.Selda.SQLite
import Database.Selda.Migrations

data Customer = Customer { customerID :: ID Customer
    , name :: Text
    , country :: Text 
} deriving (Generic, Show)

instance SqlRow Customer

reqToCustomer req = Customer {
    customerID = toId 0
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

getCustomer :: Int -> Query s (Row s Customer)
getCustomer id = do 
    customer <- select customers
    restrict (customer ! #customerID .== (literal $ toId id))
    return customer 

getCustomers :: Query s (Row s Customer)
getCustomers = do
    customers <- select customers
    return customers 