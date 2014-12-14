{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Data.Time as T
import           Options
import           Options.Time
import           Test.Chell

test_Duration :: Test
test_Duration = assertions "duration" $ do
	let parse = optionTypeParse optionType_duration
	let format = optionTypeShow optionType_duration
	$expect $ equal
		(parse "10h9m8s7ms6us5ns4ps")
		(Right (sum
			[ 10*60*60
			, 9*60
			, 8
			, 7*0.001
			, 6*0.000001
			, 5*0.000000001
			, 4*0.000000000001
			]))
	$expect $ equal (parse "1h9s") (Right 3609)
	$expect $ equal (parse "1h9ms") (Right 3600.009)
	$expect $ equal (parse "0") (Right 0)
	$expect $ equal (parse "-1h9s") (Right (-3609))
	$expect $ equal (parse "1s2us3ns") (parse "1s2\181s3ns")
	$expect $ equal
		(parse "bogus")
		(Left "\"bogus\" could not be parsed as a duration.")
	$expect $ equal
		(parse "1ps2ps")
		(Left "\"1ps2ps\" could not be parsed as a duration.")
	$expect $ equal
		(format (sum
			[ 10*60*60
			, 9*60
			, 8
			, 7*0.001
			, 6*0.000001
			, 5*0.000000001
			, 4*0.000000000001
			]))
		"10h9m8s7ms6us5ns4ps"
	$expect $ equal (format 0) "0s"
	$expect $ equal (format 3609) "1h9s"
	$expect $ equal (format (-3609)) "-1h9s"

test_Date :: Test
test_Date = assertions "date" $ do
	let parse = optionTypeParse optionType_date
	let format = optionTypeShow optionType_date
	$expect $ equal
		(parse "2011-12-13")
		(Right (T.fromGregorian 2011 12 13))
	$expect $ equal
		(parse "20111213")
		(Right (T.fromGregorian 2011 12 13))
	$expect $ equal
		(parse "2011-347")
		(Right (T.fromGregorian 2011 12 13))
	$expect $ equal
		(parse "bogus")
		(Left "\"bogus\" could not be parsed as a date.")
	$expect $ equal
		(parse "2011-10-32")
		(Left "\"2011-10-32\" could not be parsed as a date.")
	$expect $ equal (format (T.fromGregorian 2011 12 13)) "2011-12-13"

test_Time :: Test
test_Time = assertions "time" $ do
	let parse = optionTypeParse optionType_time
	let format = optionTypeShow optionType_time
	$expect $ equal
		(parse "10:11")
		(Right (T.TimeOfDay 10 11 0))
	$expect $ equal
		(parse "20:21")
		(Right (T.TimeOfDay 20 21 0))
	$expect $ equal
		(parse "20:21:22")
		(Right (T.TimeOfDay 20 21 22))
	$expect $ equal
		(parse "20:21:22.232425")
		(Right (T.TimeOfDay 20 21 22.232425))
	$expect $ equal
		(parse "bogus")
		(Left "\"bogus\" could not be parsed as a time.")
	$expect $ equal
		(parse "25:21:00")
		(Left "\"25:21:00\" could not be parsed as a time.")
	$expect $ equal
		(parse "20:21:60")
		(Right (T.TimeOfDay 20 21 60))
	$expect $ equal
		(parse "20:21:61")
		(Left "\"20:21:61\" could not be parsed as a time.")
	$expect $ equal (format (T.TimeOfDay 20 21 0)) "20:21:00"
	$expect $ equal (format (T.TimeOfDay 20 21 22.232425)) "20:21:22.232425"

test_LocalTime :: Test
test_LocalTime = assertions "local time" $ do
	let parse = optionTypeParse optionType_localTime
	let format = optionTypeShow optionType_localTime
	let lt y mo d h mi s = T.LocalTime (T.fromGregorian y mo d) (T.TimeOfDay h mi s)
	let rlt y mo d h mi s = Right (lt y mo d h mi s)
	$expect $ equal (parse "2011-12-13 14:15") (rlt 2011 12 13 14 15 0)
	$expect $ equal (parse "2011-12-13 14:15:16") (rlt 2011 12 13 14 15 16)
	$expect $ equal (parse "2011-12-13 14:15:16.1718") (rlt 2011 12 13 14 15 16.1718)
	$expect $ equal (parse "2011-347 14:15") (rlt 2011 12 13 14 15 0)
	$expect $ equal (parse "20111213 14:15") (rlt 2011 12 13 14 15 0)
	$expect $ equal (parse "20111213T141516") (rlt 2011 12 13 14 15 16)
	$expect $ equal (parse "20111213T141516.1718") (rlt 2011 12 13 14 15 16.1718)
	$expect $ equal
		(parse "bogus")
		(Left "\"bogus\" could not be parsed as a local time.")
	$expect $ equal (format (lt 2011 12 13 14 15 0)) "2011-12-13 14:15:00"
	$expect $ equal (format (lt 2011 12 13 14 15 16)) "2011-12-13 14:15:16"
	$expect $ equal (format (lt 2011 12 13 14 15 16.1718)) "2011-12-13 14:15:16.1718"

test_UtcTime :: Test
test_UtcTime = assertions "utc time" $ do
	let parse = optionTypeParse optionType_utcTime
	let format = optionTypeShow optionType_utcTime
	let ut y mo d h mi s = T.UTCTime (T.fromGregorian y mo d) (h*60*60 + mi*60 + s)
	let rut y mo d h mi s = Right (ut y mo d h mi s)
	$expect $ equal (parse "2011-12-13 14:15") (rut 2011 12 13 14 15 0)
	$expect $ equal (parse "2011-12-13 14:15:16") (rut 2011 12 13 14 15 16)
	$expect $ equal (parse "2011-12-13 14:15:16.1718") (rut 2011 12 13 14 15 16.1718)
	$expect $ equal (parse "2011-347 14:15") (rut 2011 12 13 14 15 0)
	$expect $ equal (parse "20111213 14:15") (rut 2011 12 13 14 15 0)
	$expect $ equal (parse "20111213T141516") (rut 2011 12 13 14 15 16)
	$expect $ equal (parse "20111213T141516.1718") (rut 2011 12 13 14 15 16.1718)
	$expect $ equal (parse "2011-12-13 23:59:60") (rut 2011 12 13 23 59 60)
	$expect $ equal
		(parse "bogus")
		(Left "\"bogus\" could not be parsed as a UTC time.")
	$expect $ equal
		(parse "2011-12-13 23:59:61")
		(Left "\"2011-12-13 23:59:61\" could not be parsed as a UTC time.")
	$expect $ equal (format (ut 2011 12 13 14 15 0)) "2011-12-13 14:15:00"
	$expect $ equal (format (ut 2011 12 13 14 15 16)) "2011-12-13 14:15:16"
	$expect $ equal (format (ut 2011 12 13 14 15 16.1718)) "2011-12-13 14:15:16.1718"

test_ZonedTime :: Test
test_ZonedTime = assertions "zoned time" $ do
	let parse = optionTypeParse optionType_zonedTime
	let format = optionTypeShow optionType_zonedTime
	let zt y mo d h mi s tz = T.ZonedTime (T.LocalTime (T.fromGregorian y mo d) (T.TimeOfDay h mi s)) tz
	let rzt y mo d h mi s tz = Right (zt y mo d h mi s tz)
	let noZone = T.TimeZone 0 False ""
	let tzPlus60 = T.TimeZone 60 False ""
	let tzMinus60 = T.TimeZone (-60) False ""
	let tzABC = T.TimeZone 0 False "ABC"
	$expect $ equal (parse "2011-12-13 14:15") (rzt 2011 12 13 14 15 0 noZone)
	$expect $ equal (parse "2011-12-13 14:15Z") (rzt 2011 12 13 14 15 0 T.utc)
	$expect $ equal (parse "2011-12-13 14:15+0100") (rzt 2011 12 13 14 15 0 tzPlus60)
	$expect $ equal (parse "2011-12-13 14:15 +0100") (rzt 2011 12 13 14 15 0 tzPlus60)
	$expect $ equal (parse "2011-12-13 14:15-0100") (rzt 2011 12 13 14 15 0 tzMinus60)
	$expect $ equal (parse "2011-12-13 14:15 -0100") (rzt 2011 12 13 14 15 0 tzMinus60)
	$expect $ equal (parse "2011-12-13 14:15 ABC") (rzt 2011 12 13 14 15 0 tzABC)
	$expect $ equal (parse "2011-12-13 14:15") (rzt 2011 12 13 14 15 0 noZone)
	$expect $ equal (parse "2011-12-13 14:15:16") (rzt 2011 12 13 14 15 16 noZone)
	$expect $ equal (parse "2011-12-13 14:15:16.1718") (rzt 2011 12 13 14 15 16.1718 noZone)
	$expect $ equal (parse "2011-347 14:15") (rzt 2011 12 13 14 15 0 noZone)
	$expect $ equal (parse "20111213 14:15") (rzt 2011 12 13 14 15 0 noZone)
	$expect $ equal (parse "20111213T141516") (rzt 2011 12 13 14 15 16 noZone)
	$expect $ equal (parse "20111213T141516.1718") (rzt 2011 12 13 14 15 16.1718 noZone)
	$expect $ equal
		(parse "bogus")
		(Left "\"bogus\" could not be parsed as a zoned time.")
	$expect $ equal (format (zt 2011 12 13 14 15 0 noZone)) "2011-12-13 14:15:00 +0000"
	$expect $ equal (format (zt 2011 12 13 14 15 0 T.utc)) "2011-12-13 14:15:00 UTC"
	$expect $ equal (format (zt 2011 12 13 14 15 0 tzPlus60)) "2011-12-13 14:15:00 +0100"
	$expect $ equal (format (zt 2011 12 13 14 15 0 tzMinus60)) "2011-12-13 14:15:00 -0100"
	$expect $ equal (format (zt 2011 12 13 14 15 16 noZone)) "2011-12-13 14:15:16 +0000"
	$expect $ equal (format (zt 2011 12 13 14 15 16.1718 noZone)) "2011-12-13 14:15:16.1718 +0000"

instance Eq T.ZonedTime where
	x == y = key x == key y where
		key t = (T.zonedTimeToLocalTime t, T.zonedTimeZone t)

main :: IO ()
main = Test.Chell.defaultMain [suite "tests"
	[ test_Duration
	, test_Date
	, test_Time
	, test_LocalTime
	, test_UtcTime
	, test_ZonedTime
	]]
