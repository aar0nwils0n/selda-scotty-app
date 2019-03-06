module ReqCustomer where

import Data.Aeson 
import GHC.Generics
import Database.Selda

data ReqCustomer = ReqCustomer { name :: Text
    , country :: Text
} deriving (Generic, Show)

instance ToJSON ReqCustomer

instance FromJSON ReqCustomer