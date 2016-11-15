module Time (newstart) where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

newstart :: UTCTime -> Int -> UTCTime
newstart now r =
  let
    weeks = r - 8
    secs = seconds weeks
    diff = toEnum (-secs)
    weektime = addUTCTime diff now
    day = prevMonday $ utctDay weektime
  in
    UTCTime day 0

seconds :: Int -> Int
seconds weeks = weeks*7*24*60*60

prevMonday :: Day -> Day
prevMonday day = fromWeekDate year week 1
  where
    (year, week, _) = toWeekDate day

