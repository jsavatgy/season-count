import Data.Time
import Data.Time.Clock.POSIX
import Data.List
import Data.Maybe
import System.Locale (defaultTimeLocale)


data Entry = Entry {
  abbrev :: String, 
  date :: String 
} deriving (Show,Eq)

earlierFst (Entry abbrev1 date1) (Entry abbrev2 date2) =
  date1 `compare` date2

createEntry :: String -> Entry
createEntry str = 
  Entry {
    abbrev = abbrev, 
    date = date
  } where
    abbrev = take 2 str
    date = drop 3 str

newLine = do
  putStrLn ""

readSeasons = do
  -- You may want to use the full path when compiled:
  content <- readFile "seasons-utc-list.txt"
  let fileLines = lines content
      entries = map createEntry fileLines
      sortedEntries = sortBy earlierFst entries
  return sortedEntries

showTuple x =
  "  (\"" ++ (abbrev x) ++ "\",\"" ++ (date x) ++ "\"),"

main = do
  entries <- readSeasons
  mapM_ (putStrLn . showTuple) entries
  newLine



