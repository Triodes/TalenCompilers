--module ICalendar where

import ParseLib.Abstract as PL
import Data.Maybe
import Text.PrettyPrint
import System.IO

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


data Calendar = Calendar { prodId :: String
                         , events :: [VEvent] }
    deriving (Eq, Show)

data VEvent = VEvent { dtStamp     :: DateTime
                     , uid         :: String
                     , dtStart     :: DateTime
                     , dtEnd       :: DateTime
                     , description :: Maybe String
                     , summary     :: Maybe String
                     , location    :: Maybe String }
    deriving (Eq, Show)


run :: Parser a b -> [a] -> Maybe b
run p s = listToMaybe [p | (p, []) <- parse p s]

instance Show DateTime where
    show = printDateTime

printDateTime :: DateTime -> String
printDateTime (DateTime d t u) = show4 (unYear $ year d) ++ show2 (unMonth $ month d) ++ show2 (unDay $ day d)
    ++ "T" ++ show2 (unHour $ hour t) ++ show2 (unMinute $ minute t) ++ show2 (unSecond $ second t)
    ++ if u then "Z" else ""

-- Enhanced 'show' functions pad integers with zeros to 2 or 4 places
show2 :: Int -> String
show2 i = replicate (2 - length s) '0' ++ s
    where s = show i
show4 :: Int -> String
show4 i = replicate (4 - length s) '0' ++ s
    where s = show i


-- "Main" block, DO NOT EDIT.
-- If you want to run the parser + pretty-printing, rename this module (first line) to "Main".
-- DO NOT forget to rename the module back to "ICalendar" before submitting to DomJudge.
--main = do Just cal <- readCalendar "examples/rooster_infotc.ics"
--          putStrLn $ show $ ppMonth (Year 2012) (Month 11) $ cal

main :: IO()
main = interact (show . parse parseCalendar)

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

-- Slightly modified version of 'natural'
-- Uses 'sequence' over a replicated list instead of 'many'
parseNInteger :: Int -> Parser Char Int
parseNInteger n = foldl (\ a b -> a * 10 + b) 0 <$> PL.sequence (replicate n newdigit)

parseUTC :: Parser Char Bool
parseUTC = True  <$ symbol 'Z'
       <|> False <$ epsilon

data TestType = TestType { myTime :: DateTime
                         , myDate :: DateTime }

instance Show TestType where
    show  = printTestType

printTestType :: TestType -> String
printTestType (TestType x y) = printDateTime x ++ "\n" ++ printDateTime y

-- Exercise 1
parseCalendar :: Parser Char Calendar
parseCalendar = pack (parseBegin "VCALENDAR") (parseVersion *> (Calendar <$> parseProdId <*> many parseEvent)) (parseEnd "VCALENDAR")

eol :: Parser Char String
eol = token "\r\n" <|> token "\n"

parseTilEnd :: Parser Char String
parseTilEnd = many (satisfy (\x -> x /= '\n' && x /= '\r')) <* eol

parseEvent :: Parser Char VEvent
parseEvent = pack (parseBegin "VEVENT") (VEvent <$> parseDtStamp <*> parseUid <*> parseDtStart <*> parseDtEnd <*> optional parseDesc <*> optional parseSum <*> optional parseLoc ) (parseEnd "VEVENT")

parseVersion :: Parser Char String
parseVersion = parseProperty "VERSION" parseTilEnd

parseProdId :: Parser Char String
parseProdId = parseProperty "PRODID" parseTilEnd

parseUid :: Parser Char String
parseUid = parseProperty "UID" parseTilEnd

parseLoc :: Parser Char String
parseLoc = parseProperty "LOCATION" parseTilEnd

parseDesc :: Parser Char String
parseDesc = parseProperty "DESCRIPTION" parseTilEnd

parseSum :: Parser Char String
parseSum = parseProperty "SUMMARY" parseTilEnd

parseBegin :: String -> Parser Char String
parseBegin s = parseProperty "BEGIN" (token s) <* eol

parseEnd :: String -> Parser Char String
parseEnd s = parseProperty "END" (token s) <* eol

parseProperty :: String -> Parser Char a -> Parser Char a
parseProperty s p = token s *> symbol ':' *> p

parseTimeStamp :: String -> Parser Char DateTime
parseTimeStamp s = parseProperty s parseDateTime

parseDtStamp :: Parser Char DateTime
parseDtStamp = parseTimeStamp "DTSTAMP" <* eol

parseDtStart :: Parser Char DateTime
parseDtStart = parseTimeStamp "DTSTART" <* eol

parseDtEnd :: Parser Char DateTime
parseDtEnd = parseTimeStamp "DTEND" <* eol

-- Exercise 2
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar path = do 
	fh <- openFile path ReadMode
	txt <- hGetContents fh
	return (run parseCalendar txt)

-- Exercise 3
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar = undefined


-- Exercise 4
countEvents :: Calendar -> Int
countEvents = undefined

findEvents :: DateTime -> Calendar -> [VEvent]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined



-- Exercise 5
ppMonth :: Year -> Month -> Calendar -> Doc
ppMonth = undefined

