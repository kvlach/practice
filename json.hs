import Data.Char
import Data.List
import Data.Map

data JSON
  = JsonNull
  | JsonBool Bool
  | JsonInt Int
  | JsonString String
  | JsonArray [JSON]
  | JsonObject (Data.Map.Map JSON JSON)
  deriving (Show, Eq, Ord)

data Parsed = Parsed {jval :: JSON, rest :: String} deriving (Show)

skipWhitespace :: String -> String
skipWhitespace (x : xs)
  | isSpace x = skipWhitespace xs
  | otherwise = x : xs

parseIntHelper :: String -> String -> Parsed
parseIntHelper (x : xs) digits
  | isDigit x = parseIntHelper xs (digits ++ [x])
  | isSpace x || x == ',' || x == ']' || x == '}' = Parsed (JsonInt (read digits)) (x : xs)
  | otherwise = error ("Unexpected character '" ++ [x, '\''])

parseInt :: String -> Parsed
parseInt json = parseIntHelper json ""

parseStringHelper :: String -> Bool -> String -> Parsed
parseStringHelper json start string
  | start && head json /= '"' = error "Expected opening '\"'"
  | start = parseStringHelper (tail json) False string
  | head json == '\\' && head (tail json) == '"' = parseStringHelper (tail (tail json)) start (string ++ ['"'])
  | head json == '"' = Parsed (JsonString string) (tail json)
  | otherwise = parseStringHelper (tail json) start (string ++ [head json])

parseString :: String -> Parsed
parseString json = parseStringHelper json True ""

parseArrayHelper :: String -> Bool -> Bool -> [JSON] -> Parsed
parseArrayHelper json start needComma arr
  | start && head json /= '[' = error "Expected opening '['"
  | start = parseArrayHelper (tail json) False needComma arr
  | head json == ']' = Parsed (JsonArray arr) (tail json)
  | isSpace (head json) = parseArrayHelper (skipWhitespace json) start needComma arr
  | needComma && head json /= ',' = error "Expected ','"
  | needComma = parseArrayHelper (tail json) start False arr
  | isDigit (head json) = do
      let parsed = parseInt json
      parseArrayHelper (rest parsed) start True (arr ++ [jval parsed])
  | head json == '"' = do
      let parsed = parseString json
      parseArrayHelper (rest parsed) start True (arr ++ [jval parsed])
  | head json == '[' = do
      let parsed = parseArray json
      parseArrayHelper (rest parsed) start True (arr ++ [jval parsed])
  | head json == '{' = do
      let parsed = parseObject json
      parseArrayHelper (rest parsed) start True (arr ++ [jval parsed])
  | "null" `isPrefixOf` json = do
      parseArrayHelper (Prelude.drop 4 json) start True (arr ++ [JsonNull])
  | "true" `isPrefixOf` json = do
      parseArrayHelper (Prelude.drop 4 json) start True (arr ++ [JsonBool True])
  | "false" `isPrefixOf` json = do
      parseArrayHelper (Prelude.drop 5 json) start True (arr ++ [JsonBool False])
  | otherwise = error ("Unexpected character '" ++ [head json, '\''])

parseArray :: String -> Parsed
parseArray json = parseArrayHelper json True False []

parseObjectHelper :: String -> Bool -> Bool -> Bool -> Bool -> JSON -> Data.Map.Map JSON JSON -> Parsed
parseObjectHelper json start needKey needColon needComma lastKey obj
  | start && head json /= '{' = error "Expected opening '{'"
  | start = parseObjectHelper (tail json) False needKey needColon needComma lastKey obj
  | head json == '}' = Parsed (JsonObject obj) (tail json)
  | isSpace (head json) = parseObjectHelper (skipWhitespace json) start needKey needColon needComma lastKey obj
  | needColon && head json /= ':' = error "Expected ':'"
  | needColon = parseObjectHelper (tail json) start needKey False needComma lastKey obj
  | needComma && head json /= ',' = error "Expected ','"
  | needComma = parseObjectHelper (tail json) start needKey needColon False lastKey obj
  | needKey && head json /= '"' = error "Only strings can be keys"
  | needKey = do
      let parsed = parseString json
      let obj2 = Data.Map.insert (jval parsed) JsonNull obj
      parseObjectHelper (rest parsed) start False True False (jval parsed) obj2
  | isDigit (head json) = do
      let parsed = parseInt json
      let obj2 = Data.Map.insert lastKey (jval parsed) obj
      parseObjectHelper (rest parsed) start True False True lastKey obj2
  | head json == '"' = do
      let parsed = parseString json
      let obj2 = Data.Map.insert lastKey (jval parsed) obj
      parseObjectHelper (rest parsed) start True False True lastKey obj2
  | head json == '[' = do
      let parsed = parseArray json
      let obj2 = Data.Map.insert lastKey (jval parsed) obj
      parseObjectHelper (rest parsed) start True False True lastKey obj2
  | head json == '{' = do
      let parsed = parseObject json
      let obj2 = Data.Map.insert lastKey (jval parsed) obj
      parseObjectHelper (rest parsed) start True False True lastKey obj2
  | "null" `isPrefixOf` json = do
      let obj2 = Data.Map.insert lastKey JsonNull obj
      parseObjectHelper (Prelude.drop 4 json) start True False True lastKey obj2
  | "true" `isPrefixOf` json = do
      let obj2 = Data.Map.insert lastKey (JsonBool True) obj
      parseObjectHelper (Prelude.drop 4 json) start True False True lastKey obj2
  | "false" `isPrefixOf` json = do
      let obj2 = Data.Map.insert lastKey (JsonBool False) obj
      parseObjectHelper (Prelude.drop 5 json) start True False True lastKey obj2
  | otherwise = error ("Unexpected character '" ++ [head json, '\''])

parseObject :: String -> Parsed
parseObject json = parseObjectHelper json True True False False JsonNull Data.Map.empty

parse :: String -> JSON
parse json
  | head json == '{' = do
      let parsed = parseObject json
      jval parsed
  | head json == '[' = do
      let parsed = parseArray json
      jval parsed
  | otherwise = error ("Unexpected character '" ++ [head json, '\''])

main = do
  let json = "{\"bool\": true, \"abc\": 123, \"obj\": {\"nested\": \"key\", \"arr\": [1, 2, 3]}}"
  -- let json = "[true, true, false]"
  let parsed = parse json
  print parsed
