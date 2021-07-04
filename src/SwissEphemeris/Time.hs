{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module: SwissEphemeris.Time
-- License: AGPL-3
-- Maintainer: swiss-ephemeris@lfborjas.com
-- Portability: POSIX
--
-- Functions and types for conversion between Haskell time types
-- and Swiss Ephemeris time values.
--
-- @since 1.4.0.0

module SwissEphemeris.Time (
  -- * The many faces of time
  TimeStandard(..),
  JulianDay,
  -- * Impure conversion typeclasses
  ToJulianDay(..),
  FromJulianDay(..),
  ConversionResult,
  getConversionResult,
  -- * Pure utility functions
  coerceUT,
  julianNoon,
  julianMidnight,
  -- ** Pure conversion functions
  gregorianFromJulianDayUT,
  gregorianToJulianDayUT,
  julianDay,
  gregorianDateTime,
  utcToJulianUT,
  utcToJulian,
  julianUTToUTC,
  julianToUTC
) where

import Foreign.SwissEphemeris
import SwissEphemeris.Internal

import Data.Time
import System.IO.Unsafe (unsafePerformIO)
import Foreign
import Foreign.C.String

data TimeStandard
  = TT
  -- ^ Terrestrial Time (successor to Ephemeris Time)
  | UT1
  -- ^ Universal Time, explicitly in its @UT1@ form.
  | UT
  -- ^ Universal Time, in any of its forms; depending
  -- on how it was constructed (in most cases, UTC)
    deriving (Eq, Show)

-- A @JulianDay@ can have different provenances, witnessed
-- by its accompanying phantom type:
--
-- * It could've been converted, purely, from a UTC value,
--   as such, its witness is 'UTC'
-- * It could'be been produced by consulting tidal/leap second
--   information, as done by the Swiss Ephemeris library,
--   in which case it's 'TT' (aka, somewhat wrongly, as Ephemeris
--   time,) or 'UT1'.
newtype JulianDay (s :: TimeStandard) =
  MkJulianDay { getJulianDay :: Double }
  deriving (Eq, Show, Enum)

-- | A type that encodes an attempt to convert between
-- temporal types. 
newtype ConversionResult dt =
  ConversionResult { getConversionResult :: Either String dt}
  deriving (Show, Functor, Applicative, Monad)

instance MonadFail ConversionResult where
  fail err = ConversionResult $ Left err


-- | Conversion from a temporal value of type @from@
-- to a 'JulianDay' in the 'TimeStandard' @jd@.
-- It's bound to IO _and_ a containing 'MonadFail' since
-- in the general case, we need to interact with
-- the outside world, and may fail, when consulting
-- the necessary data.
class MonadFail m => ToJulianDay m jd from where
  toJulianDay :: from -> IO (m (JulianDay jd))

instance MonadFail m => ToJulianDay m 'UT UTCTime where
  toJulianDay = return . pure . utcToJulianUT
  
instance MonadFail m => ToJulianDay m 'UT1 UTCTime where
  toJulianDay = utcToJulianUT1

instance MonadFail m => ToJulianDay m 'TT UTCTime where
  toJulianDay = utcToJulianTT

-- | Conversion from a 'JulianDay' in the 'TimeStandard'
-- @jd@ to a temporal value of type @to@
-- It's bound to IO _and_ a containing functor since
-- in the general case, we need to interact with
-- the outside world, and may fail, when consulting
-- the necessary data.
class FromJulianDay jd to where
  fromJulianDay :: JulianDay jd -> to

instance FromJulianDay 'UT UTCTime where
  fromJulianDay = julianUTToUTC

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------

-- | Coerce a value obtained directly from UTC (without
-- consulting historical data) into a UT1 Julian Day.
-- The difference should be less than 1 second, and
-- if you've used Haskell's own 'UTCTime' as the source
-- it /should/ be negligible for most use cases.
-- If you want to be precise... you'll have to go into 'IO'.
coerceUT :: JulianDay 'UT -> JulianDay 'UT1
coerceUT (MkJulianDay jd) = MkJulianDay jd

-- | Historically, Julian Days started at noon,
-- which is why the point with no fractional part
-- is noon (not midnight).
julianNoon :: JulianDay ts -> JulianDay ts
julianNoon (MkJulianDay d) = toEnum . floor $ d

-- | The half-day in Julian Days is midnight, so
-- midnight of a given date is halfway through the _previous_
-- day.
julianMidnight :: JulianDay ts -> JulianDay ts
julianMidnight = MkJulianDay . subtract 0.5 . getJulianDay . julianNoon

-------------------------------------------------------------------------------
-- Generic UT functions
-------------------------------------------------------------------------------

-- | Given components of a gregorian day (and time,)
-- produce a 'JulianDay' in the generic 'UT' time standard;
-- the precision of the resulting Julian Day will only be as good
-- as its input; if obtained by other means than via a 'UTCTime',
-- it's likely to be off by up to a second when compared with a 'UT1' value.
-- (on the other hand, it doesn't consult any data so it's not bound to 'IO')
gregorianToJulianDayUT :: Integer -> Int -> Int -> Double -> JulianDay 'UT
gregorianToJulianDayUT year month day hour =
  MkJulianDay . realToFrac $ c_swe_julday y m d h gregorian
  where
    y = fromIntegral year
    m = fromIntegral month
    d = fromIntegral day
    h = realToFrac hour

{-# DEPRECATED julianDay "Use 'gregorianToJulianDayUT' instead." #-}
julianDay :: Int -> Int -> Int -> Double -> JulianDay 'UT
julianDay intYear = gregorianToJulianDayUT (fromIntegral intYear)

-- | Given a 'JulianDay' in the 'UT' standard,
-- produce the date/time components of a gregorian date.
gregorianFromJulianDayUT :: JulianDay 'UT -> (Integer, Int, Int, Double)
gregorianFromJulianDayUT (MkJulianDay jd) =
  unsafePerformIO $
    alloca $ \jyear -> alloca $ \jmon -> alloca $ \jday -> alloca $ \jut -> do
      _ <-
        c_swe_revjul
          (realToFrac jd)
          gregorian
          jyear
          jmon
          jday
          jut
      year <- peek jyear
      month <- peek jmon
      day <- peek jday
      time <- peek jut
      return (fromIntegral year, fromIntegral month, fromIntegral day, realToFrac time)

{-# DEPRECATED gregorianDateTime "Use 'gregorianFromJulianDayUT' instead" #-}
gregorianDateTime :: JulianDay 'UT -> (Int, Int, Int, Double)
gregorianDateTime jd =
  (fromIntegral y, m, d, h)
  where
   (y, m, d, h) = gregorianFromJulianDayUT jd

picosecondsInHour :: Double
picosecondsInHour = 3600 * 1e12

-- | Given a 'UTCTime', produce a 'JulianDay' purely.
-- It can only be said to be in 'UT', since Haskell's
-- UTC is an approximation of 'UT1', off to up to a second.
-- If you want precision, use 'utcToJulianDays' (which returns
-- both the 'UT1' and 'TT' timestamps,) or 'utcToJulianUT1'.
-- Keep in mind though, that they're both in 'IO' /and/ may
-- return errors.
utcToJulianUT :: UTCTime -> JulianDay 'UT
utcToJulianUT (UTCTime day time) =
  julianDay (fromIntegral y) m d h
  where
    (y, m, d) = toGregorian day
    h = (1 / picosecondsInHour) * fromIntegral (diffTimeToPicoseconds time)

{-# DEPRECATED utcToJulian "Use 'utcToJulianUT' instead." #-}
utcToJulian :: UTCTime -> JulianDay 'UT
utcToJulian = utcToJulianUT

-- | Given a JulianDay in the vague 'UT' time standard,
-- produce a 'UTCTime' purely.
julianUTToUTC :: JulianDay 'UT -> UTCTime
julianUTToUTC jd =
  UTCTime day dt
  where
    (y, m, d, h) = gregorianDateTime jd
    day = fromGregorian (fromIntegral y) m d
    dt = picosecondsToDiffTime $ round $ h * picosecondsInHour

{-# DEPRECATED julianToUTC "Use 'julianUTToUTC', or a 'fromJulian' implementation" #-}

julianToUTC :: JulianDay 'UT -> UTCTime
julianToUTC = julianUTToUTC

-- | Utility function to split a 'UTCTime' into the constituent
-- parts expected by the underlying lib.
splitUTC :: UTCTime -> (Integer, Int, Int, TimeOfDay)
splitUTC (UTCTime day time) =
  (y, m, d, tod)
  where
    (y, m, d) = toGregorian day
    tod = timeToTimeOfDay time

-------------------------------------------------------------------------------
-- Generic UTC->(UT1,TT) functions
-------------------------------------------------------------------------------

utcToJulianDays :: MonadFail m => UTCTime -> IO (m (JulianDay 'TT, JulianDay 'UT1))
utcToJulianDays ut =
  let (y, m, d, TimeOfDay h mn s) = splitUTC ut
  in allocaArray 2 $ \dret -> allocaErrorMessage $ \serr -> do
    retval <-
      c_swe_utc_to_jd
        (fromIntegral y)
        (fromIntegral m)
        (fromIntegral d)
        (fromIntegral h)
        (fromIntegral mn)
        (realToFrac s)
        gregorian
        dret
        serr

    if retval < 0 then do
      msg <- peekCAString serr
      return $ fail msg
    else do
      (tt:ut1:_) <- peekArray 2 dret
      return $ pure (MkJulianDay . realToFrac $ tt, MkJulianDay . realToFrac  $ ut1)

utcToJulianTT :: MonadFail m => UTCTime -> IO (m (JulianDay 'TT))
utcToJulianTT ut =
  fmap fst <$> utcToJulianDays ut

utcToJulianUT1 :: MonadFail m => UTCTime -> IO (m (JulianDay 'UT1))
utcToJulianUT1 ut =
  fmap snd <$> utcToJulianDays ut


-------------------------------------------------------------------------------
-- Generic (UT1,TT) -> UTC functions
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Delta Time
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Sidereal Time 
-------------------------------------------------------------------------------


{- NOTES:

Given the following UTCTime:
2021-07-03 23:05:54.696005 UTC

if we ask for a UT JD:
jd <- toJulianDay now :: (IO (Maybe (JulianDay 'UT)))

we get:
Just (MkJulianDay {getJulianDay = 2459399.4624386113})

plugging into the nasa conversion tool (https://ssd.jpl.nasa.gov/tc.cgi#top)
they say:
2021-Jul-03 23:05:54.7

a ut1:
Just (MkJulianDay {getJulianDay = 2459399.46243737})
for Nasa:
2021-Jul-03 23:05:54.58

and a TT:
Just (MkJulianDay {getJulianDay = 2459399.463239352})

for Nasa:
2021-Jul-03 23:07:03.88

if we use the deltaT function for the UT1 ts:
deltaTime (JulianTime 2459399.46243737)
we get:
8.019823376913656e-4

and then:
2459399.46243737 + 8.019823376913656e-4
should give us TT?
the result is:
2459399.463239352

which is exactly TT!!!
-}
