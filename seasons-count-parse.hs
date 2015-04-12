{-# LANGUAGE OverloadedStrings #-}

import Data.Word
import Data.Time
import Data.Attoparsec.Char8 
import Control.Applicative
import qualified Data.ByteString as B
import Data.Text.Encoding
import Data.List
import System.Locale (defaultTimeLocale)

logFile :: FilePath
logFile = "seasons-list.txt" 

monthAbbrev = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

type Entry = (String, LocalTime)
-- for example: ("WS", "2014-12-21 23:03")
type SeasonList = [Entry]

showEntry :: Entry -> String
showEntry (str, localtime) =
  str ++ " " ++
  formatTime defaultTimeLocale "%Y-%m-%d %H:%M" localtime

justMonth mm =
  case elemIndex mm monthAbbrev of
    Just x -> x + 1
    Nothing -> 0

abbrSeas 12 = "WS" -- Winter Solstice
abbrSeas  3 = "VE" -- Vernal Equinox
abbrSeas  6 = "SS" -- Summer Solstice
abbrSeas  9 = "AE" -- Autumnal Equinox


seasParser :: Parser String
seasParser = 
     (string "Solstice Day " >> return "Solstice")
 <|> (string "Equinox Day "  >> return "Equinox")

entryParser :: Parser Entry
entryParser = do
  Data.Attoparsec.Char8.take 5
  d <- count 2 digit -- "20"
  char ' '
  mm <- takeTill (== ' ') -- "Mar"
  char ' '
  y <- count 4 digit -- "2014"
  string ": " 
  season <- seasParser
  h <- count 2 digit
  char ':'
  min <- count 2 digit
  takeTill (\x -> x == '\n')
  return ((
    abbrSeas (justMonth mm),
    LocalTime { 
      localDay = fromGregorian (read y) (justMonth mm) (read d),
      localTimeOfDay = TimeOfDay (read h) (read min) 0.0 } ))

seasonParser :: Parser SeasonList
seasonParser = many $ entryParser <* endOfLine

pstr s = do
  putStr (show s)

pstr1 (Right s) = do
 putStr (show (head s))

pstr2 (Right s) = do
  mapM_ (putStrLn . showEntry) s

main :: IO ()
main = do
  B.readFile logFile >>= pstr2 . parseOnly seasonParser

-- $ runhaskell seasons-count-parse.hs >seasons-utc-list.txt
