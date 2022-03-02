module Data where

import Data.Map (Map)

data Json
    = JsonString String
    | JsonInt Int
    | JsonFloat Double
    | JsonBool Bool
    | JsonNull
    | JsonList [Json]
    | JsonObject (Map String Json)
    deriving (Show, Eq, Ord)