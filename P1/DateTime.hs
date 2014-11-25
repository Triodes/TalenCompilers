-- Jelle Hol 3760685
-- Aron List 3896536

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

-- PARSERS
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* symbol 'T' <*> parseTime <*> parseUTC

-- Date
parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseYear :: Parser Char Year
parseYear = Year <$> parseNInteger 4

parseMonth :: Parser Char Month
parseMonth = Month <$> parseNInteger 2

parseDay :: Parser Char Day
parseDay = Day <$> parseNInteger 2

-- Time
parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseHour :: Parser Char Hour
parseHour = Hour <$> parseNInteger 2

parseMinute :: Parser Char Minute
parseMinute = Minute <$> parseNInteger 2

parseSecond :: Parser Char Second
parseSecond = Second <$> parseNInteger 2

-- Slightly modified versions of 'integer' and 'natural'
-- Uses 'sequence' over a replicated list instead of 'many'
parseNInteger :: Int -> Parser Char Int
parseNInteger n = (const negate <$> symbol '-') `option` id <*> naturalN n
    where
        naturalN n = foldl (\ a b -> a * 10 + b) 0 <$> PL.sequence (replicate n newdigit)

parseUTC :: Parser Char Bool
parseUTC = const True  <$> symbol 'Z'
       <|> const False <$> epsilon

-- Exercise 2
-- Discards results that didn't parse completely
run :: Parser a b -> [a] -> Maybe b
run p l = case success of
    [] -> Nothing
    [(a,b)] -> Just a
    where
        success = filter isParsed (parse p l)
        isParsed (_, []) = True
        isParsed (_, _ ) = False

-- Exercise 3

-- transforms a datetime into a string
printDateTime :: DateTime -> String
printDateTime dt = show4 (unYear $ year d) ++ show2 (unMonth $ month d) ++ show2 (unDay $ day d)
    ++ "T" ++ show2 (unHour $ hour t) ++ show2 (unMinute $ minute t) ++ show2 (unSecond $ second t)
    ++ z
    where
        d = date dt
        t = time dt
        z = if utc dt then "Z" else ""

-- Enhanced 'show' functions pad integers with zeros to 2 or 4 places
show2 :: Int -> String
show2 i = replicate (2 - length s) '0' ++ s
    where s = show i
show4 :: Int -> String
show4 i = replicate (4 - length s) '0' ++ s
    where s = show i

-- Alternative printer for use by the regexes
-- Returns time and date in a tuple, without 'T' or 'Z'
splitDateTime :: DateTime -> (String,String)
splitDateTime dt = (show4 (unYear $ year d) ++ show2 (unMonth $ month d) ++ show2 (unDay $ day d)
    , show2 (unHour $ hour t) ++ show2 (unMinute $ minute t) ++ show2 (unSecond $ second t) )
    where
        d = date dt
        t = time dt

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5

-- checks if the specified datetime comfirms to the datetime specification
checkDateTime :: DateTime -> Bool
checkDateTime = regDateTime . splitDateTime


-- runs the date and time regexes over their respective tuple parts
regDateTime :: (String,String) -> Bool
regDateTime (x,y) = regDate x && regTime y
    where
        regDate x = evalRegex x regexDate
        regTime x = evalRegex x regexTime

-- evaluates a regex to true and false instead of a Maybe
evalRegex :: String -> Regex -> Bool
evalRegex x y = case matchRegex y x of
    Just _  -> True
    Nothing -> False


--regex validating a time
regexTime = mkRegex "^(([0-1]{1}[0-9]{1})|(2[0-3]{1}))([0-5]{1}[0-9]{1}){2}$"

-- regex validating a date
regexDate = mkRegex "^((1[6789]|[2-9][0-9])[0-9]{2}(0[13578]|1[02])(0[1-9]|[12][0-9]|3[01]))$|^((1[6789]|[2-9][0-9])[0-9]{2}(0[469]|11)(0[1-9]|[12][0-9]|30))$|^((16|[248][048]|[3579][26])00)|(1[6789]|[2-9][0-9])(0[48]|[13579][26]|[2468][048])02(0[1-9]|1[0-9]|2[0-9])$|^(1[6789]|[2-9][0-9])[0-9]{2}02(0[1-9]|1[0-9]|2[0-8])$"


-- Exercise 6

