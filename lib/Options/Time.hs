{-# LANGUAGE CPP #-}

-- |
-- Module: Options.Time
-- License: MIT
--
-- The @options-time@ package provides 'OptionType' implementations and
-- associated 'SimpleOption' instances for types related to dates and times.
module Options.Time
	( optionType_duration
	, optionType_date
	, optionType_time
	, optionType_localTime
	, optionType_utcTime
	, optionType_zonedTime
	) where

import           Data.Fixed (divMod')
import           Data.Ratio (numerator)
import qualified Data.Time as T
import           Options
#if MIN_VERSION_time(1,3,0)
import           Data.Time.LocalTime (makeTimeOfDayValid)
#else
import           Data.Fixed (Pico)
#endif
#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format (defaultTimeLocale)
#else
import           System.Locale (defaultTimeLocale)
#endif
import qualified Text.ParserCombinators.ReadP as R

-- | Store an option as a 'T.DiffTime'. The duration is specified in a format
-- such as @\"2h30m\"@ for two hours and thirty minutes.
--
-- Available units are:
--
-- * \"h\" for hours.
-- * \"m\" for minutes.
-- * \"s\" for seconds.
-- * \"ms\" for milliseconds.
-- * \"us\" or \"&#181;s\" for microseconds.
-- * \"ns\" for nanoseconds.
-- * \"ps\" for picoseconds.
--
-- Larger units are not available to avoid ambiguity when dealing with daylight
-- savings time.
optionType_duration :: OptionType T.DiffTime
optionType_duration = optionType "duration" 0 parseDuration formatDuration

instance SimpleOptionType T.DiffTime where
	simpleOptionType = optionType_duration

parseDuration :: String -> Either String T.DiffTime
parseDuration = parsedOrErr where
	parsedOrErr s = case R.readP_to_S parser s of
		(duration,_):_ -> Right duration
		[] -> Left (show s ++ " could not be parsed as a duration.")
	parser = orderedChoice [zero, units]
	zero = do
		_ <- R.char '0'
		R.eof
		return (toPicoseconds 0)
	units = do
		minus <- R.option 1 (R.char '-' >> return (-1))
		acc <- loop (toPicoseconds 0) 0
		return (minus * acc)
	loop acc dropIdx = do
		digits <- R.munch1 (\c -> c >= '0' && c <= '9')
		(multiplier, dropIdx')  <- orderedChoice (drop dropIdx parsers)
		let acc' = acc + multiplier (read digits)
		orderedChoice [done acc', loop acc' dropIdx']
	done acc = R.eof >> return acc
	parsers =
		[ R.char 'h' >> return (toHours, 1)
		, justMinuteSuffix >> return (toMinutes, 2)
		, R.char 's' >> return (toSeconds, 3)
		, R.string "ms" >> return (toMilliseconds, 4)
		, R.string "us" >> return (toMicroseconds, 6)
		, R.string "\181s" >> return (toMicroseconds, 6)
		, R.string "ns" >> return (toNanoseconds, 7)
		, R.string "ps" >> return (toPicoseconds, 8)
		]
	justMinuteSuffix = do
		ahead <- R.look
		case ahead of
			'm':'s':_ -> R.pfail
			_ -> R.char 'm'

orderedChoice :: [R.ReadP a] -> R.ReadP a
orderedChoice ps = case ps of
	[] -> R.pfail
	[p] -> p
	(p:ps') -> p R.<++ orderedChoice ps'

toPicoseconds :: Integer -> T.DiffTime
toPicoseconds = T.picosecondsToDiffTime

toNanoseconds :: Integer -> T.DiffTime
toNanoseconds = toPicoseconds . (*1000)

toMicroseconds :: Integer -> T.DiffTime
toMicroseconds = toNanoseconds . (*1000)

toMilliseconds :: Integer -> T.DiffTime
toMilliseconds = toMicroseconds . (*1000)

toSeconds :: Integer -> T.DiffTime
toSeconds = T.secondsToDiffTime

toMinutes :: Integer -> T.DiffTime
toMinutes = toSeconds . (*60)

toHours :: Integer -> T.DiffTime
toHours = toMinutes . (*60)

formatDuration :: T.DiffTime -> String
formatDuration t = formatted where
	formatted = if t == 0
		then "0s"
		else concat chunks
	(negative, absolute) = if t < 0
		then (True, t * (- 1))
		else (False, t)
	(rawSeconds, rawPicoFraction) = divMod' absolute 1 :: (Integer, T.DiffTime)
	(hours, rawMinutes) = divMod rawSeconds 3600
	(minutes, seconds) = divMod rawMinutes 60
	rawPicos = numerator (toRational (rawPicoFraction*1000000000000))
	(milliseconds, rawMicros) = divMod rawPicos 1000000000
	(microseconds, rawNanos) = divMod rawMicros 1000000
	(nanoseconds, picoseconds) = divMod rawNanos 1000
	chunks =
		[ if negative then "-" else ""
		, chunk hours "h"
		, chunk minutes "m"
		, chunk seconds "s"
		, chunk milliseconds "ms"
		, chunk microseconds "us"
		, chunk nanoseconds "ns"
		, chunk picoseconds "ps"
		]
	chunk 0 _ = ""
	chunk n suffix = show n ++ suffix

-- | Store an option as a 'T.Day'. Supported formats are:
--
-- * \"YYYY-MM-DD\"
-- * \"YYYYMMDD\"
-- * \"YYYY-DDD\"
optionType_date :: OptionType T.Day
optionType_date = optionType "date" (T.fromGregorian 1970 1 1) parseDate formatDate

instance SimpleOptionType T.Day where
	simpleOptionType = optionType_date

parseDate :: String -> Either String T.Day
parseDate s = parsedOrErr where
	parsedOrErr = case parsed of
		Just day -> Right day
		Nothing -> Left (show s ++ " could not be parsed as a date.")
	parsed = firstJust
		[ checkedParse "%Y-%m-%d" s
		, checkedParse "%Y-%j" s
		, checkedParse "%Y%m%d" s
		]

formatDate :: T.Day -> String
formatDate = T.formatTime defaultTimeLocale "%Y-%m-%d"

-- | Store an option as a 'T.TimeOfDay'. Supported formats are:
--
-- * \"HH:MM\"
-- * \"HH:MM:SS\"
-- * \"HH:MM:SS.FFFF\"
--
-- For example, the value @\"10:11:12.5\"@ is half a second past
-- 10:11:12 AM.
optionType_time :: OptionType T.TimeOfDay
optionType_time = optionType "time" T.midnight parseTime formatTime

instance SimpleOptionType T.TimeOfDay where
	simpleOptionType = optionType_time

parseTime :: String -> Either String T.TimeOfDay
parseTime s = parsedOrErr where
	parsedOrErr = case parsed >>= validateTime of
		Just time -> Right time
		Nothing -> Left (show s ++ " could not be parsed as a time.")
	parsed = firstJust
		[ checkedParse "%H:%M" s
		, checkedParse "%H:%M:%S%Q" s
		]

formatTime :: T.TimeOfDay -> String
formatTime = T.formatTime defaultTimeLocale "%H:%M:%S%Q"

validateTime :: T.TimeOfDay -> Maybe T.TimeOfDay
validateTime t = makeTimeOfDayValid (T.todHour t) (T.todMin t) (T.todSec t)

#if !MIN_VERSION_time(1,3,0)
-- Based on time-1.3:Data/Time/LocalTime/TimeOfDay.hs
makeTimeOfDayValid :: Int -> Int -> Pico -> Maybe T.TimeOfDay
makeTimeOfDayValid h m s = do
	_ <- clipValid 0 24 h
	_ <- clipValid 0 60 m
	_ <- clipValid 0 61 s
	return (T.TimeOfDay h m s)

-- Based on time-1.3:Data/Time/Calendar/Private.hs
clipValid :: (Ord t) => t -> t -> t -> Maybe t
clipValid a _ x | x < a = Nothing
clipValid _ b x | x >= b = Nothing
clipValid _ _ x = Just x
#endif

-- | Store an option as a 'T.LocalTime'. Supported formats are a combination
-- of those for 'optionType_date' and 'optionType_time'.
optionType_localTime :: OptionType T.LocalTime
optionType_localTime = optionType "local time" localEpoch parseLocalTime formatLocalTime

instance SimpleOptionType T.LocalTime where
	simpleOptionType = optionType_localTime

localEpoch :: T.LocalTime
localEpoch = T.LocalTime (T.fromGregorian 1970 1 1) T.midnight

parseLocalTime :: String -> Either String T.LocalTime
parseLocalTime s = parsedOrErr where
	parsedOrErr = case parsed >>= validateLocalTime of
		Just time -> Right time
		Nothing -> Left (show s ++ " could not be parsed as a local time.")
	parsed = firstJust $ do
		ymd <- ["%Y-%m-%d", "%Y-%j", "%Y%m%d"]
		hms <- ["%H:%M", "%H:%M:%S%Q", "%H%M%S%Q"]
		sep <- [" ", "T"]
		[checkedParse (ymd ++ sep ++ hms) s]

formatLocalTime :: T.LocalTime -> String
formatLocalTime = T.formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

validateLocalTime :: T.LocalTime -> Maybe T.LocalTime
validateLocalTime t = do
	_ <- validateTime (T.localTimeOfDay t)
	return t

-- | Store an option as a 'T.UTCTime'. Supported formats are a combination
-- of those for 'optionType_date' and 'optionType_time'.
optionType_utcTime :: OptionType T.UTCTime
optionType_utcTime = optionType "utc time" utcEpoch parseUtcTime formatUtcTime

instance SimpleOptionType T.UTCTime where
	simpleOptionType = optionType_utcTime

utcEpoch :: T.UTCTime
utcEpoch = T.UTCTime (T.fromGregorian 1970 1 1) 0

parseUtcTime :: String -> Either String T.UTCTime
parseUtcTime s = parsedOrErr where
	parsedOrErr = case parsed >>= validateUtcTime of
		Just time -> Right time
		Nothing -> Left (show s ++ " could not be parsed as a UTC time.")
	parsed = firstJust $ do
		ymd <- ["%Y-%m-%d", "%Y-%j", "%Y%m%d"]
		hms <- ["%H:%M", "%H:%M:%S%Q", "%H%M%S%Q"]
		sep <- [" ", "T"]
		[checkedParse (ymd ++ sep ++ hms) s]

formatUtcTime :: T.UTCTime -> String
formatUtcTime = T.formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

validateUtcTime :: T.UTCTime -> Maybe T.UTCTime
validateUtcTime t = case T.utctDayTime t of
	x | x >= 0 && x < 86401 -> Just t
	_ -> Nothing

-- | Store an option as a 'T.ZonedTime'. Supported formats are a combination
-- of those for 'optionType_date' and 'optionType_time'.
optionType_zonedTime :: OptionType T.ZonedTime
optionType_zonedTime = optionType "zoned time" zonedEpoch parseZonedTime formatZonedTime

instance SimpleOptionType T.ZonedTime where
	simpleOptionType = optionType_zonedTime

zonedEpoch :: T.ZonedTime
zonedEpoch = T.ZonedTime localEpoch T.utc

parseZonedTime :: String -> Either String T.ZonedTime
parseZonedTime s = parsedOrErr where
	parsedOrErr = case parsed >>= validateZonedTime of
		Just time -> Right time
		Nothing -> Left (show s ++ " could not be parsed as a zoned time.")
	parsed = firstJust $ do
		ymd <- ["%Y-%m-%d", "%Y-%j", "%Y%m%d"]
		hms <- ["%H:%M", "%H:%M:%S%Q", "%H%M%S%Q"]
		sep <- [" ", "T"]
		(tz, fixtz) <- [("", id), ("Z", setUTC), ("%z", id), (" %z", id), (" %Z", id)]
		-- TODO: This doesn't support +01:00 because checkedParse will format
		-- that to +0100 and fail.
		[fixtz `fmap` checkedParse (ymd ++ sep ++ hms ++ tz) s]
	setUTC t = t { T.zonedTimeZone = T.utc }

formatZonedTime :: T.ZonedTime -> String
formatZonedTime = T.formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q %Z"

validateZonedTime :: T.ZonedTime -> Maybe T.ZonedTime
validateZonedTime t = do
	_ <- validateLocalTime (T.zonedTimeToLocalTime t)
	return t

checkedParse :: (T.FormatTime t, T.ParseTime t) => String -> String -> Maybe t
checkedParse fmt input = do
	parsed <- T.parseTime defaultTimeLocale fmt input
	-- Be fairly strict about the input format, because T.parseTime will
	-- try to silently fix invalid inputs.
	--
	-- For example, "2014-20-12" is parsed as "2014-12-12".
	if input == T.formatTime defaultTimeLocale fmt parsed
		then Just parsed
		else Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust xs = case [x | Just x <- xs] of
	x:_ -> Just x
	[] -> Nothing
