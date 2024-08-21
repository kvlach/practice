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
  | isSpace x = skipWhitespace xs
  | otherwise = x : xs

subString :: String -> Int -> Int -> String
subString s start end = drop start (take end s)

expect :: Char -> String -> ()
expect c s
  | c /= head s = error "Unexpected character"

data ParsedString = ParsedString {str :: JSON, end :: Int} deriving (Show)

parseStringHelper :: String -> Int -> String -> ParsedString
parseStringHelper s index parsed
  | index == 0 && head s /= '"' = error "Expected opening '\"'"
  | index == length s && head s /= '"' = error "Expected closing '\"'"
  | index == 0 = parseStringHelper (tail s) (index + 1) ""
  | head s == '\\' && head (tail s) == '"' = parseStringHelper (tail (tail s)) (index + 2) (parsed ++ ['"'])
  | head s == '"' = ParsedString (JsonString parsed) (index + 1)
  | otherwise = parseStringHelper (tail s) (index + 1) (parsed ++ [head s])

parseString :: String -> ParsedString
parseString s = do
  let parsed = parseStringHelper s 0 ""
  parsed

main = do
  let json = "\"ab\\\"c\""
  let parsed = parseString json
  print parsed
