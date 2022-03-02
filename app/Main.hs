module Main where

import Lib
import Pretty
  
main :: IO ()
main = case run "{ \"user\": {\"active\": true,\"props\": {\"name\":  \"nogw\",\"age\": 24,\"year\": {\"idk\": \"i really idk\",\"fav\": [1, 2, 3, 4, 5, \"sexo\"]}}}}" of
  Right a -> putStrLn $ prettyJson a
  Left err -> print err