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

parseStringHelper :: String -> Int -> String -> JSON
parseStringHelper (x : xs) index parsed
  | index == 0 && x /= '"' = error "expected opening '\"'"
  | index == 0 = parseStringHelper xs (index + 1) parsed
  | x == '\\' && head xs == '"' = parseStringHelper (tail xs) (index + 2) (parsed ++ "\"")
  | x == '"' && index /= 0 = JsonString parsed
  | otherwise = do parseStringHelper xs (index + 1) (parsed ++ [x])

parseString :: String -> JSON
parseString s = parseStringHelper s 0 ""

parse :: String -> JSON
parse = parseString

main = do
  -- let json = "{\"abc\": 123}"
  let json = "\"abcded\""
  print json
  print (parse json)
