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

skipWhitespace :: String -> String
skipWhitespace (x : xs)
  | isSpace x = skipWhitespace xs
  | otherwise = x : xs

subString :: Int -> Int -> String -> String
subString start end s = Prelude.drop start (Prelude.take end s)

data Parsed = Parsed {jval :: JSON, rest :: String} deriving (Show)

parseStringHelper :: String -> Bool -> String -> Parsed
parseStringHelper json start string
  | start && head json /= '"' = error "Expected opening '\"'"
  | start = parseStringHelper (tail json) False ""
  | head json == '"' = Parsed (JsonString string) (tail json)
  | head json == '\\' && head (tail json) == '"' = parseStringHelper (tail (tail json)) False (string ++ ['"'])
  | otherwise = parseStringHelper (tail json) False (string ++ [head json])

parseString :: String -> Parsed
parseString json = parseStringHelper json True ""

parseObjectHelper :: String -> Bool -> Bool -> JSON -> Bool -> Data.Map.Map JSON JSON -> Parsed
parseObjectHelper json start expectKey lastKey gotSemicolon obj
  | start && head json /= '{' = error "Expected opening '{'"
  | start = parseObjectHelper (tail json) False expectKey lastKey gotSemicolon obj
  | isSpace (head json) = parseObjectHelper (skipWhitespace json) False expectKey lastKey gotSemicolon obj
  | head json == '"' && expectKey = do
      let parsed = parseString json
      let obj2 = Data.Map.insert (jval parsed) JsonNull obj
      parseObjectHelper (rest parsed) False False (jval parsed) False obj2
  | not expectKey && head json == '"' && not gotSemicolon = error "Expected ':' before value"
  | gotSemicolon && head json == ':' = error "More than one ':' found"
  | not expectKey && head json == ':' = parseObjectHelper (tail json) False expectKey lastKey True obj
  | not expectKey && head json == '"' = do
      let parsed = parseString json
      let obj2 = Data.Map.insert lastKey (jval parsed) obj
      parseObjectHelper (rest parsed) False True lastKey False obj2
  | head json == '}' = Parsed (JsonObject obj) (tail json)
  | otherwise = error "Unexpected character"

parseObject :: String -> Parsed
parseObject json = parseObjectHelper json True True JsonNull False Data.Map.empty

main = do
  let json = "{\"abc\": \"def\"}, {}"
  let parsed = parseObject json
  print parsed
