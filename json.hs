import Data.Char

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
  | isSpace (x) = skipWhitespace xs
  | otherwise = x : xs

parseStringHelper :: String -> Int -> Int
parseStringHelper (x : xs) index
  | x == '"' && index == 0 = parseStringHelper xs (index + 1)
  | (x == '\\') && (head xs == '"') = parseStringHelper (tail xs) (index + 2)
  | x == '"' && index /= 0 = index
  | otherwise = parseStringHelper xs (index + 1)

data ParsedString = ParsedString {str :: String, rest :: String}

subString :: String -> Int -> Int -> String
subString str start end = take (end - start) (drop start str)

parseString :: String -> ParsedString
parseString s = do
  let index = parseStringHelper s 0
  ParsedString (subString s 1 index) (subString s index (length s))

parseHelper :: String -> JSON -> JSON
parseHelper (x : xs) json
  | x == '"' = do
      let parsedString = parseString (x : xs)
      parseHelper (rest parsedString) (JsonString (str parsedString))
  | otherwise = json

parse :: String -> JSON
parse s = parseHelper s JsonNull

main = do
  let json = "{\"abc\": 123}"
  let parsed = parse json
  print parsed
