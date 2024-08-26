import Data.Char
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
  | otherwise = error ("Unexpected character: " ++ [x])

parseInt :: String -> Parsed
parseInt json = parseIntHelper json ""

parseStringHelper :: String -> Bool -> String -> Parsed
parseStringHelper json start string
  | start && head json /= '"' = error "Expected opening '\"'"
  | start = parseStringHelper (tail json) False string
  | json == "" = error "Expected closing '\"'"
  | head json == '\\' && head (tail json) == '"' = parseStringHelper (tail (tail json)) start (string ++ ['"'])
  | head json == '"' = Parsed (JsonString string) (tail json)
  | otherwise = parseStringHelper (tail json) start (string ++ [head json])

parseString :: String -> Parsed
parseString json = parseStringHelper json True ""

parseArrayHelper :: String -> Bool -> Bool -> [JSON] -> Parsed
parseArrayHelper json start needComma arr
  | start && head json /= '[' = error "Expected opening '['"
  | start = parseArrayHelper (tail json) False needComma arr
  | json == "" = error "Expected closing ']'"
  | head json == ']' = Parsed (JsonArray arr) (tail json)
  | isSpace (head json) = parseArrayHelper (skipWhitespace json) start needComma arr
  | needComma && head json /= ',' = error "Expected ','"
  | needComma = parseArrayHelper (tail json) start False arr
  | head json == '"' = do
      let parsed = parseString json
      parseArrayHelper (rest parsed) start True (arr ++ [jval parsed])
  | isDigit (head json) = do
      let parsed = parseInt json
      parseArrayHelper (rest parsed) start True (arr ++ [jval parsed])
  | head json == '{' = do
      let parsed = parseObject json
      parseArrayHelper (rest parsed) start True (arr ++ [jval parsed])
  | otherwise = error ("Unexpected character: '" ++ [head json, '\''])

parseArray :: String -> Parsed
parseArray json = parseArrayHelper json True False []

parseObjectHelper :: String -> Bool -> Bool -> JSON -> Bool -> Bool -> Data.Map.Map JSON JSON -> Parsed
parseObjectHelper json start expectKey lastKey foundColon needComma obj
  | start && head json /= '{' = error "Expected opening '{'"
  | start = parseObjectHelper (tail json) False expectKey lastKey foundColon needComma obj
  | json == "" = error "Expected closing '}'"
  | head json == '}' = Parsed (JsonObject obj) (tail json)
  | isSpace (head json) = parseObjectHelper (skipWhitespace json) start expectKey lastKey foundColon needComma obj
  | needComma && head json /= ',' = error "Expected ','"
  | needComma = parseObjectHelper (tail json) start expectKey lastKey foundColon False obj
  | expectKey && head json /= '"' = error "Keys can only be strings"
  | expectKey && head json == '"' = do
      let parsed = parseString json
      let obj2 = Data.Map.insert (jval parsed) JsonNull obj
      parseObjectHelper (rest parsed) start False (jval parsed) foundColon needComma obj2
  | foundColon && head json == ':' = error "Found more than one ':'"
  | not expectKey && head json == ':' = parseObjectHelper (tail json) start expectKey lastKey True needComma obj
  | not expectKey && head json == '"' = do
      let parsed = parseString json
      let obj2 = Data.Map.insert lastKey (jval parsed) obj
      parseObjectHelper (rest parsed) start True lastKey False True obj2
  | isDigit (head json) = do
      let parsed = parseInt json
      let obj2 = Data.Map.insert lastKey (jval parsed) obj
      parseObjectHelper (rest parsed) start True lastKey False True obj2
  | head json == '[' = do
      let parsed = parseArray json
      let obj2 = Data.Map.insert lastKey (jval parsed) obj
      parseObjectHelper (rest parsed) start True lastKey False True obj2
  | otherwise = error ("Unexpected character: '" ++ [head json, '\''])

parseObject :: String -> Parsed
parseObject json = parseObjectHelper json True True JsonNull False False Data.Map.empty

main = do
  let json = "{\"abc\": 123, \"second\": \"def\", \"arr\": [1, {\"neste\": 56}, 3]}"
  let parsed = parseObject json
  print parsed
