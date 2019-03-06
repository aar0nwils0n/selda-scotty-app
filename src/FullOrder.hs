module FullOrder where

import qualified Order
import qualified Customer
import Database.Selda
import Data.Maybe

import Data.Aeson 

data FullOrder = FullOrder { orderID :: Int
    , orderTotal :: Double 
    , customerName :: Text
    , country :: Text 
} deriving (Generic, Show)

instance ToJSON FullOrder

defaultCustomer = Customer.Customer (toId 0) (""::Text) (""::Text)

orderTupToFullOrder = map (\(order :*: dbCustomer) ->
    let customer = fromMaybe defaultCustomer dbCustomer
    in FullOrder { orderID = fromId $ Order.orderID order
  , orderTotal = Order.orderTotal order
  , customerName = Customer.name customer
  , country = Customer.country customer
})
