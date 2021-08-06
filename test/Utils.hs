module Utils where

import Data.Time ( UTCTime (..), TimeOfDay (..), timeToTimeOfDay )
import Data.Time.Format.ISO8601 ( iso8601ParseM )
import Data.Maybe (fromJust)
import Test.QuickCheck
import Arbitrary ()
import SwissEphemeris.Time (JulianDayUT1, utcToJulian, coerceUT, gregorianToJulianDayUT)

ephePath :: FilePath
ephePath = "./swedist/sweph_18"

mkUTC :: String -> UTCTime
mkUTC  = fromJust . iso8601ParseM

-- | Generate an arbitrary 'UTCTime' before the time of writing these
-- tests. Due to the nature of leap seconds, we can't assert authoritatively
-- that producing a julian day from a utc time will not fail: if
-- a new leap second is documented and this library hasn't been updated,
-- conversion /will/ fail and it can /only/ be resolved by updating
-- the Swiss Ephemeris version (and thus updating this library,) or
-- providing a file with additional leap seconds. See
-- [8.3.  Handling of leap seconds and the file seleapsec.txt](https://www.astro.com/swisseph/swephprg.htm#_Toc71121195).
-- [Known leap seconds](https://www.ietf.org/timezones/data/leap-seconds.list)
-- should, however, work with no complaints from the library.
civilTime :: Gen UTCTime
civilTime =
  arbitrary `suchThat` isNotLeapSecond
  where
    -- The `arbitrary` instance for `UTCTime` will produce fake
    -- leap seconds such as @-5779-06-16T23:59:60Z@, which is
    -- _not_ a valid leap second, even if it's a valid 'UTCTime';
    -- the C library doesn't like fake leap seconds.
    isNotLeapSecond (UTCTime _ time) =
      sec < 60
      where
        TimeOfDay _ _ sec = timeToTimeOfDay time

genJulianInRange :: UTCTime -> UTCTime -> Gen JulianDayUT1
genJulianInRange min' max' = do
  utc <- civilTime `suchThat` (\t -> t > min' && t < max')
  let jdut = utcToJulian utc
  pure $ coerceUT jdut
  
genJulianBefore :: UTCTime -> Gen JulianDayUT1
genJulianBefore minT = do
  utc <- civilTime `suchThat` (< minT)
  let jdut = utcToJulian utc
  pure $ coerceUT jdut
  
genJulianAfter :: UTCTime -> Gen JulianDayUT1
genJulianAfter maxT = do
  utc <- civilTime `suchThat` (> maxT)
  let jdut = utcToJulian utc
  pure $ coerceUT jdut
  
mkJulian :: Integer -> Int -> Int -> Double -> JulianDayUT1
mkJulian y m d h = coerceUT $ gregorianToJulianDayUT y m d h

minTestEpheT :: UTCTime
minTestEpheT = mkUTC "1800-01-01T00:00:00Z"

maxTestEpheT :: UTCTime
maxTestEpheT = mkUTC "2399-12-31T21:36:00Z"
