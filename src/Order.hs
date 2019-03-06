module Order where

import GHC.Generics (Generic)
import Database.Selda hiding (Order)
import Database.Selda.SQLite
import Database.Selda.Migrations
import Customer
import Data.Aeson 

data Order = Order { orderID :: ID Order
    , relatedCustomer :: ID Customer 
    , orderTotal :: Double 
} deriving (Generic, Show)

data OrderReq = OrderReq {
    reqRelatedCustomer :: Int
    , reqOrderTotal :: Double
}

instance FromJSON OrderReq where
    parseJSON (Object v) = OrderReq
        <$> v .: "relatedCustomer"
        <*> v .: "orderTotal"

instance SqlRow Order

reqToOrder req = Order { orderID = toId 0
    , relatedCustomer = toId $ reqRelatedCustomer $ req 
    , orderTotal = reqOrderTotal req }

orders :: Table Order
orders = table "order" [ #orderID :- autoPrimary
    , #relatedCustomer :- (foreignKey customers #customerID) 
    ]


getOrdersWithCustomer = do
    order <- select orders
    customer <- leftJoin (\customer -> customer ! #customerID .== order ! #relatedCustomer)
                        (select customers)
    return (order :*: customer) 