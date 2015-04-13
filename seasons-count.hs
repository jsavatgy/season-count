import Data.Time
import Data.Time.Clock.POSIX
import Data.List
import Data.Maybe
import System.Locale (defaultTimeLocale)

data Entry = Entry {
  abbrev :: String, 
  date :: String 
} deriving (Show,Eq)

secondsFrom :: POSIXTime -> POSIXTime -> Double
secondsFrom startPt endPt =
  a - b
  where
    a = ptToDouble endPt
    b = ptToDouble startPt

ptToDouble :: POSIXTime -> Double
ptToDouble t  = fromRational (toRational t)

-- ... 'T minus 3', 'T minus 2', 'T minus 1', 'Liftoff', 'T plus 1', ...
plusMinus sDouble 
  | sDouble >= 0.0 = "+" -- T plus n, event in past
  | otherwise      = "−" -- T minus n, event in future

secsToDaysHoursMinsSecs :: Double -> String
secsToDaysHoursMinsSecs sDouble =
  plusMinus sDouble ++ " " ++ dStr ++ hStr ++ minStr where
    dStr   = if   d /= 0 then   (show d) ++ " d "   else ""
    hStr   = if   h /= 0 then   (show h) ++ " h "   else ""
    minStr = if min /= 0 then (show min) ++ " min " else ""
    sInt = round (abs sDouble)
    (d,hLeft)   = divMod sInt 86400
    (h,minLeft) = divMod hLeft 3600
    (min,s)     = divMod minLeft 60

earlierFst (Entry abbrev1 date1) (Entry abbrev2 date2) =
  date1 `compare` date2

ptEntry pt =
  Entry { 
    abbrev = abbrev, 
    date = date 
  } where
    abbrev = "NOW"
    date = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (
             utcToLocalTime utc (posixSecondsToUTCTime pt))

createEntry (abbrev,date) = 
  Entry {
    abbrev = abbrev, 
    date = date
  } 

readSeasons = do
  let entries = map createEntry seasonsUtcList
      sortedEntries = sortBy earlierFst entries
  return sortedEntries

timeParsed :: String -> UTCTime
timeParsed line =
  fromJust t where
    t = parseTime defaultTimeLocale "%Y-%m-%d %H:%M" line

idx now entries =
  fromJust e where
    e = elemIndex now testEntries
    testEntries = insertBy earlierFst now entries
 
entriesAround pt r entries =
  take (2*r) dropList 
  where
    dropList = drop (i-r) entries
    i = idx (ptEntry pt) entries

countersAround currentPt entries =
  -- (unwords . words) is used to remove possible double spaces
  [ (unwords . words) ((fDiffShow x) ++ " " ++ (abbrev x)) | x <- around ]
  where
    fDiffShow = diffShow . diffSecs . datePt . date
    around = entriesAround currentPt 1 entries
    datePt = utcTimeToPOSIXSeconds . timeParsed
    diffSecs = \datePt -> secondsFrom datePt currentPt
    diffShow = secsToDaysHoursMinsSecs

newLine = do
  putStrLn ""

main = do
  pt  <- getPOSIXTime
  entries <- readSeasons
  mapM_ putStrLn (countersAround pt entries)

seasonsUtcList = [ -- $ runhaskell parse-list-to-tuples.hs >paste-here.txt
  ("VE","2014-03-20 16:57"),
  ("SS","2014-06-21 10:51"),
  ("AE","2014-09-23 02:29"),
  ("WS","2014-12-21 23:03"),
  ("VE","2015-03-20 22:45"),
  ("SS","2015-06-21 16:38"),
  ("AE","2015-09-23 08:20"),
  ("WS","2015-12-22 04:48"),
  ("VE","2016-03-20 04:30"),
  ("SS","2016-06-20 22:34"),
  ("AE","2016-09-22 14:21"),
  ("WS","2016-12-21 10:44"),
  ("VE","2017-03-20 10:29"),
  ("SS","2017-06-21 04:24"),
  ("AE","2017-09-22 20:01"),
  ("WS","2017-12-21 16:28"),
  ("VE","2018-03-20 16:15"),
  ("SS","2018-06-21 10:07"),
  ("AE","2018-09-23 01:54"),
  ("WS","2018-12-21 22:22"),
  ("VE","2019-03-20 21:58"),
  ("SS","2019-06-21 15:54"),
  ("AE","2019-09-23 07:50"),
  ("WS","2019-12-22 04:19"),
  ("VE","2020-03-20 03:50"),
  ("SS","2020-06-20 21:43"),
  ("AE","2020-09-22 13:31"),
  ("WS","2020-12-21 10:02"),
  ("VE","2021-03-20 09:37"),
  ("SS","2021-06-21 03:32"),
  ("AE","2021-09-22 19:21"),
  ("WS","2021-12-21 15:59"),
  ("VE","2022-03-20 15:33"),
  ("SS","2022-06-21 09:13"),
  ("AE","2022-09-23 01:04"),
  ("WS","2022-12-21 21:47"),
  ("VE","2023-03-20 21:24"),
  ("SS","2023-06-21 14:57"),
  ("AE","2023-09-23 06:50"),
  ("WS","2023-12-22 03:27"),
  ("VE","2024-03-20 03:06"),
  ("SS","2024-06-20 20:51"),
  ("AE","2024-09-22 12:43"),
  ("WS","2024-12-21 09:20"),
  ("VE","2025-03-20 09:01"),
  ("SS","2025-06-21 02:42"),
  ("AE","2025-09-22 18:19"),
  ("WS","2025-12-21 15:02"),
  ("VE","2026-03-20 14:45"),
  ("SS","2026-06-21 08:25"),
  ("AE","2026-09-23 00:05"),
  ("WS","2026-12-21 20:49"),
  ("VE","2027-03-20 20:25"),
  ("SS","2027-06-21 14:10"),
  ("AE","2027-09-23 06:01"),
  ("WS","2027-12-22 02:41"),
  ("VE","2028-03-20 02:17"),
  ("SS","2028-06-20 20:01"),
  ("AE","2028-09-22 11:45"),
  ("WS","2028-12-21 08:19"),
  ("VE","2029-03-20 08:01"),
  ("SS","2029-06-21 01:48"),
  ("AE","2029-09-22 17:37"),
  ("WS","2029-12-21 14:14"),
  ("VE","2030-03-20 13:51"),
  ("SS","2030-06-21 07:31"),
  ("AE","2030-09-22 23:27"),
  ("WS","2030-12-21 20:09"),
  ("VE","2031-03-20 19:41"),
  ("SS","2031-06-21 13:17"),
  ("AE","2031-09-23 05:15"),
  ("WS","2031-12-22 01:55"),
  ("VE","2032-03-20 01:22"),
  ("SS","2032-06-20 19:08"),
  ("AE","2032-09-22 11:10"),
  ("WS","2032-12-21 07:55"),
  ("VE","2033-03-20 07:23"),
  ("SS","2033-06-21 01:01"),
  ("AE","2033-09-22 16:51"),
  ("WS","2033-12-21 13:45"),
  ("VE","2034-03-20 13:17"),
  ("SS","2034-06-21 06:44"),
  ("AE","2034-09-22 22:39"),
  ("WS","2034-12-21 19:33"),
  ("VE","2035-03-20 19:03"),
  ("SS","2035-06-21 12:32"),
  ("AE","2035-09-23 04:38"),
  ("WS","2035-12-22 01:30"),
  ("VE","2036-03-20 01:02"),
  ("SS","2036-06-20 18:31"),
  ("AE","2036-09-22 10:23"),
  ("WS","2036-12-21 07:12"),
  ("VE","2037-03-20 06:49"),
  ("SS","2037-06-21 00:22"),
  ("AE","2037-09-22 16:12"),
  ("WS","2037-12-21 13:07"),
  ("VE","2038-03-20 12:40"),
  ("SS","2038-06-21 06:09"),
  ("AE","2038-09-22 22:02"),
  ("WS","2038-12-21 19:01"),
  ("VE","2039-03-20 18:32"),
  ("SS","2039-06-21 11:57"),
  ("AE","2039-09-23 03:49"),
  ("WS","2039-12-22 00:40"),
  ("VE","2040-03-20 00:11"),
  ("SS","2040-06-20 17:46"),
  ("AE","2040-09-22 09:44"),
  ("WS","2040-12-21 06:32"),
  ("VE","2041-03-20 06:06"),
  ("SS","2041-06-20 23:36"),
  ("AE","2041-09-22 15:26"),
  ("WS","2041-12-21 12:17"),
  ("VE","2042-03-20 11:53"),
  ("SS","2042-06-21 05:16"),
  ("AE","2042-09-22 21:11"),
  ("WS","2042-12-21 18:03"),
  ("VE","2043-03-20 17:27"),
  ("SS","2043-06-21 10:57"),
  ("AE","2043-09-23 03:06"),
  ("WS","2043-12-22 00:01"),
  ("VE","2044-03-19 23:20"),
  ("SS","2044-06-20 16:50"),
  ("AE","2044-09-22 08:47"),
  ("WS","2044-12-21 05:43"),
  ("VE","2045-03-20 05:07"),
  ("SS","2045-06-20 22:33"),
  ("AE","2045-09-22 14:32"),
  ("WS","2045-12-21 11:34"),
  ("VE","2046-03-20 10:57"),
  ("SS","2046-06-21 04:14"),
  ("AE","2046-09-22 20:21"),
  ("WS","2046-12-21 17:27"),
  ("VE","2047-03-20 16:52"),
  ("SS","2047-06-21 10:02"),
  ("AE","2047-09-23 02:07"),
  ("WS","2047-12-21 23:07"),
  ("VE","2048-03-19 22:33"),
  ("SS","2048-06-20 15:53"),
  ("AE","2048-09-22 08:00"),
  ("WS","2048-12-21 05:01"),
  ("VE","2049-03-20 04:28"),
  ("SS","2049-06-20 21:47"),
  ("AE","2049-09-22 13:42"),
  ("WS","2049-12-21 10:51"),
  ("VE","2050-03-20 10:19"),
  ("SS","2050-06-21 03:32"),
  ("AE","2050-09-22 19:28"),
  ("WS","2050-12-21 16:37"),
  ("VE","2051-03-20 15:58"),
  ("SS","2051-06-21 09:18"),
  ("AE","2051-09-23 01:26"),
  ("WS","2051-12-21 22:33"),
  ("VE","2052-03-19 21:55"),
  ("SS","2052-06-20 15:15"),
  ("AE","2052-09-22 07:15"),
  ("WS","2052-12-21 04:16"),
  ("VE","2053-03-20 03:46"),
  ("SS","2053-06-20 21:03"),
  ("AE","2053-09-22 13:05"),
  ("WS","2053-12-21 10:09"),
  ("VE","2054-03-20 09:34"),
  ("SS","2054-06-21 02:46"),
  ("AE","2054-09-22 18:59"),
  ("WS","2054-12-21 16:09"),
  ("VE","2055-03-20 15:28"),
  ("SS","2055-06-21 08:39"),
  ("AE","2055-09-23 00:48"),
  ("WS","2055-12-21 21:55"),
  ("VE","2056-03-19 21:10"),
  ("SS","2056-06-20 14:28"),
  ("AE","2056-09-22 06:39"),
  ("WS","2056-12-21 03:50"),
  ("VE","2057-03-20 03:07"),
  ("SS","2057-06-20 20:19"),
  ("AE","2057-09-22 12:23"),
  ("WS","2057-12-21 09:42"),
  ("VE","2058-03-20 09:04"),
  ("SS","2058-06-21 02:03"),
  ("AE","2058-09-22 18:08"),
  ("WS","2058-12-21 15:24"),
  ("VE","2059-03-20 14:44"),
  ("SS","2059-06-21 07:46"),
  ("AE","2059-09-23 00:02"),
  ("WS","2059-12-21 21:17"),
  ("VE","2060-03-19 20:38"),
  ("SS","2060-06-20 13:45"),
  ("AE","2060-09-22 05:48"),
  ("WS","2060-12-21 03:00") ]

