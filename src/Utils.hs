module Utils where

import Data (Json (JsonObject))
import qualified Data.Map as M
import Pretty (prettyJson)
import Lib (run)
import System.IO

key :: String -> Json -> Either String Json
key mem obj = case obj of
  JsonObject ob -> case M.lookup mem ob of
    Nothing -> Left "key not found"
    Just js -> Right js
  _ -> Left "expect a object"

keys :: Json -> Either String [String]
keys obj = case obj of
  JsonObject ob -> Right $ map fst (M.toList ob)
  _ -> Left "expect a object"

prettyJsonFile :: String -> IO ()
prettyJsonFile filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  case run contents of
    Left pe -> print pe
    Right js -> do
      hClose handle ; 
      writeFile filename $ prettyJson js