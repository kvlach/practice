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
parseIntHelper (x : xs) container
  | isDigit x = parseIntHelper xs (container ++ [x])
  | isSpace x || x == ',' || x == ':' = Parsed (JsonInt (read container)) (x : xs)
  | otherwise = error ("Unexpected character" ++ [x])

parseInt :: String -> Parsed
parseInt json = parseIntHelper json ""

parseStringHelper :: String -> Bool -> String -> Parsed
parseStringHelper json start string
  | start && head json /= '"' = error "Expected opening '\"'"
  | start = parseStringHelper (tail json) False ""
  | head json == '\\' && head (tail json) == '"' = parseStringHelper (tail (tail json)) False (string ++ ['"'])
  | head json == '"' = Parsed (JsonString string) (tail json)
  | otherwise = parseStringHelper (tail json) False (string ++ [head json])

parseString :: String -> Parsed
parseString json = parseStringHelper json True ""

parseObjectHelper :: String -> Bool -> Bool -> JSON -> Bool -> Data.Map.Map JSON JSON -> Parsed
parseObjectHelper json start expectKey lastKey foundColon obj
  | start && head json /= '{' = error "Expected opening '{'"
  | start = parseObjectHelper (tail json) False True lastKey foundColon obj
  | isSpace (head json) = parseObjectHelper (skipWhitespace json) start expectKey lastKey foundColon obj
  | expectKey && head json == ':' = error "Expected key, not ':'"
  | head json == ':' = parseObjectHelper (tail json) False expectKey lastKey True obj
  | not expectKey && not foundColon = error "Expected colon"
  | foundColon && head json == ':' = error "Found more than one ':'"
  | expectKey && head json == '"' = do
      let parsed = parseString json
      let obj2 = Data.Map.insert (jval parsed) JsonNull obj
      parseObjectHelper (rest parsed) False False (jval parsed) False obj2
  | not expectKey && head json == '"' = do
      let parsed = parseString json
      let obj2 = Data.Map.insert lastKey (jval parsed) obj
      parseObjectHelper (rest parsed) False True lastKey False obj2
  | head json == '}' = Parsed (JsonObject obj) (tail json)
  | otherwise = error ("Unexpected character: " ++ [head json])

parseObject :: String -> Parsed
parseObject json = parseObjectHelper json True True JsonNull False Data.Map.empty

main = do
  let json = "{\"abc\": \"def\"}"
  let parsed = parseObject json
  print parsed
