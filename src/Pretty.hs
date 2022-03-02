module Format where

import Data (Json (..))
import Data.List as List ( intercalate )
import Data.Map (fromList, toList)
import Text.Printf

quote :: String -> String
quote = printf "\"%s\""

indent :: String -> String
indent = List.intercalate "\n" . map ('\t' :) . lines

joinComma :: [String] -> String
joinComma = List.intercalate ",\n"

prettyJson :: Json -> String
prettyJson js =
  case js of
    JsonNull -> "null"
    JsonInt n -> show n
    JsonFloat f -> show f
    JsonString s -> quote s
    JsonBool b -> (if b then "true" else "false")
    JsonList [] -> "[]"
    JsonList l -> printf "[\n%s\n]" $ indent $ joinComma $ map prettyJson l
    JsonObject kv -> printf "{\n%s\n}" $ indent $ joinComma $ map pairToStr (toList kv)
     where
      pairToStr (key, val) = printf "%s: %s" (quote key) (prettyJson val)