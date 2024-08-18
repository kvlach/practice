import Data.Char

data JSON
  = JsonNull
  | JsonBool Bool
  | JsonInt Int
  | JsonString String
  | JsonArray [JSON]
  | JsonObject [(String, JSON)]
  deriving (Show)

data ExtractedInt = ExtractedInt {int :: String, rest :: String}

skipWhitespace :: String -> String
skipWhitespace (x : xs)
  | isSpace x = skipWhitespace xs
  | otherwise = x : xs

parseInt :: String -> ExtractedInt
parseInt (x : xs)
  | isDigit x = ExtractedInt [x] xs

-- parseInt s = ParsedInt (JsonInt (read s)) ()

parse :: String -> JSON
parse s
  | isSpace (head s) = parse (skipWhitespace s)

-- \| isDigit (head s) = do
--     let n = parseInt s
-- 	  parse s

-- parse s = do
--   let s = skipWhitespace s
--   JsonObject [("abc", JsonInt 123)]

json = "  {\"abc\": 123}"

main = do
  putStrLn json
  putStrLn (skipWhitespace json)
  print (parse json)
