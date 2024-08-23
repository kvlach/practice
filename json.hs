import Data.Char
import Data.Map

data JSON
  = JsonNull
  | JsonBool Bool
  | JsonInt Int
  | JsonString String
  | JsonArray [JSON]
  | JsonObject (Data.Map.Map String JSON)
  deriving (Show)

skipWhitespace :: String -> String
skipWhitespace (x : xs)
  | isSpace x = skipWhitespace xs
  | otherwise = x : xs

subString :: Int -> Int -> String -> String
subString start end s = Prelude.drop start (Prelude.take end s)

data Parsed = Parsed {jval :: JSON, rest :: String} deriving (Show)

parseStringHelper :: String -> Bool -> String -> Parsed
parseStringHelper json start val
  | start && head json /= '"' = error "Expected opening '\"'"
  | start = parseStringHelper (tail json) False ""
  | head json == '\\' && head (tail json) == '"' = parseStringHelper (tail (tail json)) False (val ++ ['"'])
  | not start && head json == '"' = Parsed (JsonString val) (skipWhitespace (tail json))
  | otherwise = parseStringHelper (tail json) False (val ++ [head json])

parseString :: String -> Parsed
parseString json = parseStringHelper json True ""

parseObjectHelper :: String -> Bool -> Bool -> Data.Map.Map String JSON -> Parsed
parseObjectHelper json start expectKey val
  | start && head json /= '{' = error "Expected opening '{'"
  | start = parseObjectHelper (tail json) False expectKey val
  | isSpace (head json) = parseObjectHelper (skipWhitespace json) False expectKey val
  | head json == '}' = Parsed (JsonObject val) (skipWhitespace (tail json))
  | head json == '"' && expectKey = do
      let parsed = parseString json
      let val = Data.Map.insert (case jval parsed of JsonString s -> s) JsonNull val
      parseObjectHelper (rest parsed) False False val
  | otherwise = error ("Unexpected character '" ++ [head json] ++ "'")

-- let parsed = parseString json
-- parseObjectHelper (rest parsed) False False (Data.Map.insert (jval parsed) JsonNull val) -- Accumulate key-value pairs in the Map

-- \| otherwise = parseObjectHelper (tail json) False (Map.insert )

parseObject :: String -> Parsed
parseObject json = parseObjectHelper json True True Data.Map.empty

main = do
  -- let json = "\"ab\\\"c\": 12345, "
  -- let json = "\"abc\": 12345, "
  let json = "{\"abc\": 123}"
  let parsed = parseObject json
  print parsed
