module Main where

import ParseLib.Abstract as PL
import Data.Maybe
import Text.PrettyPrint
import qualified Data.Time as T
import System.IO

data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

-- why not synonyms by using type instead of newtype????

newtype Year  = Year { unYear :: Int }  deriving (Eq, Ord)
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day { unDay :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

-- why not synonyms by using type instead of newtype????

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

data Property = DtStamp DateTime
              | Uid String
              | DtStart DateTime
              | DtEnd DateTime
              | Description String
              | Summary String
              | Location String
    deriving (Eq, Ord, Show)

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

recogniseCalendar :: String -> Maybe Calendar
recogniseCalendar = run parseCalendar

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









-- Exercise 1
parseCalendar :: Parser Char Calendar
parseCalendar = pack (parseBegin "VCALENDAR") (parseVersion *> (Calendar <$> parseProdId <*> parseEvents)) (parseEnd "VCALENDAR")

parseCalendarBody :: Parser Char Calendar
parseCalendarBody = pc1 <|> pc2
    where pc1 = parseVersion *> (Calendar <$> parseProdId <*> parseEvents)
          pc2 = Calendar <$> parseProdId <* parseVersion <*> parseEvents

eol :: Parser Char String
eol = token "\r\n" <|> token "\n"

multiLine :: Parser Char Char
multiLine = const '|' <$ eol <*> (token " " <|> token "\t")

parseTilEnd :: Parser Char String
--parseTilEnd = many anyNoEol <* eol
parseTilEnd = many (anyNoEol <|> multiLine) <* eol
    where anyNoEol = satisfy (\x -> x /= '\n' && x /= '\r')

parseEventProps :: Parser Char [Property]
parseEventProps = pack (parseBegin "VEVENT") (many parseProperty) (parseEnd "VEVENT")
--  where
--    parseBody = flip VEvent <$> parseUid <*> parseDtStamp <*> parseDtStart <*> parseDtEnd <*> optional parseDesc <*> optional parseSum <*> optional parseLoc

parseEvents :: Parser Char [VEvent]
parseEvents = f <$> many parseEvent
    where f x = map fromJust $ filter isJust x

parseEvent :: Parser Char (Maybe VEvent)
parseEvent = f <$> parseEventProps
    where f props = do dtstamp  <- exactlyOnce [p | (DtStamp p) <- props]
                       uid      <- exactlyOnce [p | (Uid p) <- props]
                       dtstart  <- exactlyOnce [p | (DtStart p) <- props]
                       dtend    <- exactlyOnce [p | (DtEnd p) <- props]
                       desc     <- zeroOrOnce [p | (Description p) <- props]
                       summary  <- zeroOrOnce [p | (Summary p) <- props]
                       location <- zeroOrOnce [p | (Location p) <- props]
                       return (VEvent dtstamp uid dtstart dtend desc summary location)


exactlyOnce :: [a] -> Maybe a
exactlyOnce []     = Nothing
exactlyOnce (x:[]) = Just x
exactlyOnce (x:xs) = Nothing

zeroOrOnce :: [a] -> Maybe (Maybe a)
zeroOrOnce []     = Just Nothing
zeroOrOnce (x:[]) = Just (Just x)
zeroOrOnce (x:xs) = Nothing

parseVersion :: Parser Char String
parseVersion = parseLabel "VERSION" parseTilEnd

parseProdId :: Parser Char String
parseProdId = parseLabel "PRODID" parseTilEnd

parseProperty = choice [parseUid,
                        parseDtStamp,
                        parseDtStart,
                        parseDtEnd,
                        parseSum,
                        parseDesc,
                        parseLoc]

parseUid :: Parser Char Property
parseUid = Uid <$> parseLabel "UID" parseTilEnd

parseLoc :: Parser Char Property
parseLoc = Location <$> parseLabel "LOCATION" parseTilEnd

parseDesc :: Parser Char Property
parseDesc = Description <$> parseLabel "DESCRIPTION" parseTilEnd

parseSum :: Parser Char Property
parseSum = Summary <$> parseLabel "SUMMARY" parseTilEnd

parseBegin :: String -> Parser Char String
parseBegin s = parseLabel "BEGIN" (token s) <* eol

parseEnd :: String -> Parser Char String
parseEnd s = parseLabel "END" (token s) <* eol

parseLabel :: String -> Parser Char a -> Parser Char a
parseLabel s p = token s *> symbol ':' *> p

parseTimeStamp :: String -> Parser Char DateTime
parseTimeStamp s = parseLabel s parseDateTime

parseDtStamp :: Parser Char Property
parseDtStamp = DtStamp <$> parseTimeStamp "DTSTAMP" <* eol

parseDtStart :: Parser Char Property
parseDtStart = DtStart <$> parseTimeStamp "DTSTART" <* eol

parseDtEnd :: Parser Char Property
parseDtEnd = DtEnd <$> parseTimeStamp "DTEND" <* eol










-- Exercise 2
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar path = do 
    fh <- openFile path ReadMode
    txt <- hGetContents fh
    return (run parseCalendar txt)

-- Exercise 3
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar x = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\n"

printEvent :: VEvent -> String
printEvent x = "BEGIN:VEVENT\r\n" ++ "" ++ "END:VEVENT"

-- Exercise 4
countEvents :: Calendar -> Int
countEvents = length . events

timeInEvent :: DateTime -> VEvent -> Bool
timeInEvent t e = t >= dtStart e && t <= dtEnd e

findEvents :: DateTime -> Calendar -> [VEvent]
findEvents t c = filter (timeInEvent t) (events c)

-- ask if situation where begin and end are equal counts as overlap

checkOverlapping :: Calendar -> Bool
checkOverlapping cal = or $ map (eventOverlap $ evts) evts
    where
        evts = events cal

eventOverlap :: [VEvent] -> VEvent -> Bool
eventOverlap evts evt = or $ map (overlap evt) evts

overlap :: VEvent -> VEvent -> Bool
overlap u v = timeInEvent (dtStart u) v || timeInEvent (dtEnd u) v

timeSpent :: String -> Calendar -> Int
timeSpent s c = sum $ map eventTime $ filter (hasSummary s) (events c)

-- First argument is latest date
daysApart :: Date -> Date -> Integer
daysApart (Date (Year y1) (Month m1) (Day d1)) (Date (Year y2) (Month m2) (Day d2)) = T.diffDays (getDay y1 m1 d1) (getDay y2 m2 d2)
    where 
        getDay y m d = T.fromGregorian (toInteger y) m d

hasSummary :: String -> VEvent -> Bool
hasSummary [] e = Nothing == summary e
hasSummary s  e = Just s  == summary e

eventTime :: VEvent -> Int
eventTime e = ((fromInteger $ daysApart de ds) * 24 * 60)
    + tDiff te ts
    where
        dts = dtStart e
        ds = date dts
        ts = time dts
        dte = dtEnd e
        de = date dte
        te = time dte

tDiff :: Time -> Time -> Int
tDiff end start = (h1 - h2) * 60 + m1 - m2
    where
        h1 = unHour $ hour end
        h2 = unHour $ hour start
        m1 = unMinute $ minute end
        m2 = unMinute $ minute start

-- Exercise 5
ppMonth :: Year -> Month -> Calendar -> Doc
ppMonth = undefined

