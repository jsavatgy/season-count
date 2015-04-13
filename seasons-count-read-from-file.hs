import Data.Time
import Data.Time.Clock.POSIX
import Data.List
import Data.Maybe
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)

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
  | otherwise      = "-" -- T minus n, event in future

secsToDaysHoursMinsSecs :: Double -> String
secsToDaysHoursMinsSecs sDouble =
  plusMinus sDouble ++ " " ++ dStr ++ hStr ++ minStr where
    dStr   = if   d /= 0 then   (show d) ++ " d "  else ""
    hStr   = if   h /= 0 then   (show h) ++ " h "  else ""
    minStr = if min /= 0 then (show min) ++ " min" else ""
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

createEntry :: String -> Entry
createEntry str = 
  Entry {
    abbrev = abbrev, 
    date = date
  } where
    abbrev = take 2 str
    date = drop 3 str

readSeasons = do
  -- You may want to use the full path when compiled:
  content <- readFile "seasons-utc-list.txt"
  let fileLines = lines content
      entries = map createEntry fileLines
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
  [(fDiffShow x) ++ " " ++ (abbrev x) | x <- around]
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
  --mapM_ (putStrLn . show) entries
  --newLine
  --putStrLn (show (ptEntry pt))
  --newLine
  --putStrLn (show (idx (ptEntry pt) entries))
  --newLine
  --mapM_ (putStrLn . show) (entriesAround pt 2 entries)
  --newLine
  mapM_ putStrLn (countersAround pt entries)


