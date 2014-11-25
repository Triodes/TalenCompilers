import ParseLib.Abstract as PL
import Text.Regex


-- Starting Framework


-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year { unYear :: Int }  deriving (Eq, Ord)
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day { unDay :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour { unHour :: Int } deriving (Eq, Ord)
newtype Minute = Minute { unMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { unSecond :: Int } deriving (Eq, Ord)


-- | The main interaction function. Used for IO, do not edit.
data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

main :: IO ()
main = interact (printOutput . processCheck . processInput)
    where
        processInput = map (run parseDateTime) . lines
        processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
        printOutput  = unlines . map show



-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* symbol 'T' <*> parseTime <*> parseUTC

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseYear :: Parser Char Year
parseYear = Year <$> parseNInteger 4

parseMonth :: Parser Char Month
parseMonth = Month <$> parseNInteger 2

parseDay :: Parser Char Day
parseDay = Day <$> parseNInteger 2

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseHour :: Parser Char Hour
parseHour = Hour <$> parseNInteger 2

parseMinute :: Parser Char Minute
parseMinute = Minute <$> parseNInteger 2

parseSecond :: Parser Char Second
parseSecond = Second <$> parseNInteger 2

parseNInteger :: Int -> Parser Char Int
parseNInteger n = (const negate <$> (symbol '-')) `option` id <*> (naturalN n)
    where
        naturalN n = foldl (\ a b -> a * 10 + b) 0 <$> (PL.sequence (replicate n newdigit))

parseUTC :: Parser Char Bool
parseUTC = const True  <$> symbol 'Z'
       <|> const False <$> epsilon

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p l = case success of
    [] -> Nothing
    [(a,b)] -> Just a
    where
        success = filter isParsed (parse p l)
        isParsed (_, []) = True
        isParsed (_, _ ) = False


-- Exercise 3
printDateTime :: DateTime -> String
printDateTime x = undefined



-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s




-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined

regDateTime :: (String,String) -> Bool
regDateTime (x,y) = (regDate x) && (regTime y)

evalRegex :: String -> Regex -> Bool
evalRegex x y = case matchRegex y x of
    Just _ -> True
    Nothing -> False

regDate :: String -> Bool
regDate x = evalRegex x regexDate

regTime x = evalRegex x regexTime

regexTime = mkRegex "^(([0-1]{1}[0-9]{1})|(2[0-3]{1}))([0-5]{1}[0-9]{1}){2}$"

regexDate = mkRegex "^((1[6789]|[2-9][0-9])[0-9]{2}(0[13578]|1[02])(0[1-9]|[12][0-9]|3[01]))$|^((1[6789]|[2-9][0-9])[0-9]{2}(0[469]|11)(0[1-9]|[12][0-9]|30))$|^((16|[248][048]|[3579][26])00)|(1[6789]|[2-9][0-9])(0[48]|[13579][26]|[2468][048])02(0[1-9]|1[0-9]|2[0-9])$|^(1[6789]|[2-9][0-9])[0-9]{2}02(0[1-9]|1[0-9]|2[0-8])$"


-- Exercise 6

