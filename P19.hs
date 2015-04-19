--file project-euler/P19.hs
--Counting Sundays

 
data Date = Date
    {year :: Int
    ,month :: Month
    ,day   :: Int
    ,weekday :: Day
    }deriving(Show,Eq,Ord)

data Day = Mon|Tue|Wed|Thur|Fri|Sat|Sun
    deriving(Show,Eq,Ord)
data Month = Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec
    deriving(Show,Eq,Ord)

main = do
    let dates = enumerDateFromTo (Date 1900 Jan 1 Mon) 2001
    let sundays = filterDate (compare 1900 . year) LT $ filterDate weekday Sun $ filterDate day 1 dates 
    putStrLn $ show $ length sundays

filterDate :: (Eq a) => (Date -> a) -> a -> [Date] -> [Date]
filterDate _ _ [] = []
filterDate f p (x:xs) =
    case (f x == p) of
         True -> x : filterDate f p xs
         False -> filterDate f p xs

enumerDateFromTo :: Date -> Int -> [Date]
enumerDateFromTo date1 y
    | (year date1) == y = []
    | otherwise = date1 : enumerDateFromTo (incDate date1) y
incDate :: Date -> Date
incDate date
    | (month date == Dec) && (day date == 31)      = newYear
    | (day date == 29) && (feb)                    = newMonth
    | (day date == 28) && (feb) && (leapYear date) = newDay
    | (day date == 28) && (feb)                    = newMonth
    | (day date == 30) && (shortmonth date)        = newMonth
    | (day date == 31) && (not $ shortmonth date)  = newMonth
    | otherwise                                    = newDay
    where y        = year date
          m        = month date
          d        = day date
          wd       = weekday date
          feb      = (month date) == Feb
          newYear  = Date (y+1) Jan 1 (nextDay wd)
          newMonth = Date (y) (nextMonth m) 1 (nextDay wd)
          newDay   = Date (y) (m) (d + 1) (nextDay wd)

shortmonth :: Date -> Bool
shortmonth date =
    case month date of
         Sep -> True
         Apr -> True
         Jun -> True
         Nov -> True
         _   -> False

leapYear :: Date -> Bool
leapYear date
    | century date = (year date) `mod` 400 == 0
    | otherwise    = (year date) `mod` 4 == 0
    where century date = (year date) `mod` 100 == 0
nextMonth :: Month -> Month
nextMonth m =
    case m of
         Jan -> Feb
         Feb -> Mar
         Mar -> Apr
         Apr -> May
         May -> Jun
         Jun -> Jul
         Jul -> Aug
         Aug -> Sep
         Sep -> Oct
         Oct -> Nov
         Nov -> Dec
         Dec -> Jan
nextDay :: Day -> Day
nextDay wd =
    case wd of
         Mon -> Tue
         Tue -> Wed
         Wed -> Thur
         Thur -> Fri
         Fri -> Sat
         Sat -> Sun
         Sun -> Mon
