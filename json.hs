import Data.Char

-- import Data.Map

data JSON
  = JsonNull
  | JsonBool Bool
  | JsonInt Int
  | JsonString String
  | JsonArray [JSON]
  | JsonObject [(String, JSON)]
  deriving (Show)

skipWhitespace :: String -> String
skipWhitespace (x : xs)
  | isSpace x = skipWhitespace xs
  | otherwise = x : xs

subString :: Int -> Int -> String -> String
subString start end s = drop start (take end s)

data Parsed = Parsed {decoded :: JSON, rest :: String} deriving (Show)

parseStringHelper :: String -> Int -> String -> Parsed
parseStringHelper json index string
  | index == 0 && head json /= '"' = error "Expected opening '\"'"
  | index == 0 && head json == '"' = parseStringHelper (tail json) (index + 1) ""
  | head json == '\\' && head (tail json) == '"' = parseStringHelper (tail (tail json)) (index + 1) (string ++ ['"'])
  | head json == '"' = Parsed (JsonString string) (skipWhitespace (tail json))
  | otherwise = parseStringHelper (tail json) (index + 1) (string ++ [head json])

parseString :: String -> Parsed
parseString json = parseStringHelper json 0 ""

parseObjectHelper :: String -> Int -> JSON -> Parsed
parseObjectHelper json index obj
  | index == 0 && head json /= '{' = error "Expected opening '{'"
  | index == 0 && head json == '{' = parseObjectHelper (tail json) (index + 1) obj
  | head json == '"' = do
      let parsed = parseString json
      let obj = Map.insert (decoded parsed) JsonNull obj
      parseObjectHelper (rest parsed) 1 obj
  | otherwise = parseObjectHelper json index obj

parseObject :: String -> Parsed
parseObject json = parseObjectHelper json 0 (JsonObject [])

parse :: String -> JSON
parse (x : xs)
  | isSpace x = parse (skipWhitespace (x : xs))
  | x == '{' = decoded (parseObject (x : xs))

main = do
  let json = "\"abc\": 123}, "
  let parsed = parseString json
  print parsed
