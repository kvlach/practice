data JSON
  = JsonNull
  | JsonBool Bool
  | JsonInt Integer
  | JsonString String
  | JsonArray [JSON]
  | JsonObject [(String, JSON)]
  deriving (Show)

determineType :: Char -> Maybe JSON
determineType c = case c of
  '1' -> Just JsonInt

-- determineType 1 = Just Num
-- determineType '"' = Just String
-- determineType '[' = Just List
-- determineType '{' = Just Map

parse :: String -> JSON
parse s = JsonInt 1

-- parse

json = "{\"abc\": 123}"

main = print (parse json)
