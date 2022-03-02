module Lib where

import Data (Json (..))
import Data.Char
import qualified Data.Functor.Identity
import Data.Map (Map, fromList)
import Pretty (prettyJson)
import Text.Parsec
import Text.Parsec.Error

jsonString :: Parsec String () Json
jsonString = JsonString <$> between (char '"') (char '"') (many $ noneOf ['"'])

jsonNumber :: Parsec String () Json
jsonNumber = do
  int <- many1 digit
  flt <- optionMaybe (char '.' >> many digit)
  case flt of
    Nothing -> return $ JsonInt $ read int
    Just d ->
      return $
        JsonFloat $
          read $ int ++ "." ++ d ++ "0"

jsonBool :: Parsec String () Json
jsonBool =
  fmap JsonBool $
    False <$ string "false"
      <|> True <$ string "true"

jsonNull :: Parsec String () Json
jsonNull = JsonNull <$ string "null"

jsonList :: Parsec String () Json
jsonList = JsonList <$> between (char '[') (char ']') (try (spaces *> items <* spaces) <|> pure [])
 where
  items = (spaces *> parser <* spaces) `sepBy` (spaces *> char ',' <* spaces)

jsonObject :: Parsec String () Json
jsonObject = JsonObject . fromList <$> between (char '{') (char '}') (try (spaces *> items `sepBy` (spaces *> char ',' <* spaces) <* spaces))
 where
  items = do
    key <- between (char '"') (char '"') (many $ noneOf ['"'])
    _ <- spaces *> char ':' <* spaces
    val <- spaces *> parser <* spaces
    pure (key, val)

parser :: ParsecT String () Data.Functor.Identity.Identity Json
parser = try jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonList <|> jsonObject

run :: String -> Either ParseError Json
run = parse parser ""