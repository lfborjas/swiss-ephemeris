{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

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
module SwissEphemeris.Time
  ( -- * The many faces of time
    -- $timeDoc
    TimeStandard (..),
    JulianDay,
    JulianDayUT,
    JulianDayTT,
    JulianDayUT1,
    getJulianDay,
    SiderealTime,
    getSiderealTime,
    -- ** singletons
    SingTimeStandard(..),
    SingTSI(..),

    -- * Impure conversion typeclasses
    ToJulianDay (..),
    FromJulianDay (..),

    -- ** Wrapper for fail-able conversions
    ConversionResult,
    getConversionResult,

    -- * Pure utility functions
    mkJulianDay,
    coerceUT,
    julianNoon,
    julianMidnight,

    -- ** Impure conversion functions
    utcToJulianDays,
    -- ** Pure conversion functions
    -- *** Lossy conversion of a @Day@ value
    dayFromJulianDay,
    dayToJulianDay,
    -- *** 'Fake' (innacurate) conversions of datetime components
    gregorianToFakeJulianDayTT,
    gregorianFromFakeJulianDayTT,
    -- *** Lossy UT conversions of datetime components
    gregorianToJulianDayUT,
    gregorianFromJulianDayUT,
    -- *** Lossy UT conversions of an @UTC@ value
    utcToJulianDayUT,
    julianDayUTToUTC,
    -- (less ugly aliases)
    utcToJulian,
    julianToUTC,

    -- * Delta Time
    addDeltaTime,
    subtractDeltaTime,
    unsafeDeltaTime,
    deltaTime,
    safeDeltaTime,
    deltaTimeSE,
    universalToTerrestrial,
    universalToTerrestrialSafe,
    universalToTerrestrialSE,
    
    -- * Sidereal time
    julianToSiderealSimple,
    julianToSidereal
  )
where

import qualified Control.Monad.Fail as Fail
import Data.Time
import Foreign
import Foreign.C.String
import Foreign.SwissEphemeris
import SwissEphemeris.Internal
import System.IO.Unsafe (unsafePerformIO)
import Data.Kind (Type)

{- $timeDoc
   This module offers conversions between some Haskell time values, and astronomical
   time values as defined by @SwissEphemeris@. The most important types in this
   module are 'TimeStandard', which refers to different "standards" of time
   such as Universal Time and Terrestial Time, and 'JulianDay', which codifies
   an absolute floating point number of fractional "days" since an epoch
   in the distant past. A 'SiderealTime' is also provided, though it figures
   less prominently in the Swiss Ephemeris API, and the conversions are more
   self-explanatory.
   
   As far as this library is concerned, a [Julian Day](https://en.wikipedia.org/wiki/Julian_day)
   can represent either a moment in [Universal Time](https://en.wikipedia.org/wiki/Universal_Time),
   which takes into account the Earth's rotation (either the
   more specific @UT1@ standard, or a generic @UT@ time whose precision is left up
   to the caller -- we provide ways of converting a @UTCTime@ into a @JulianDayUT@, for example,)
   or [Terrestrial Time](https://en.wikipedia.org/wiki/Terrestrial_Time), which is independent of the Earth's rotation and is used
   in astronomical measurements from a theoretical point on the surface of the Earth.
   Most functionality in Swiss Ephemeris uses Terrestrial Time (the documentation
   also refers to it using the now-superseded moniker of Ephemeris Time, but current
   versions of the library actually don't use the time standard by that name, and instead
   adhere to TT.) 

   An absolute moment in time will /not/ be the same in UT1 and TT: TT is /ahead/ of
   UT1 by a quantity known as [Delta Time](https://en.wikipedia.org/wiki/%CE%94T_(timekeeping\)),
   which is not neatly predictable but which is expected to increase with the passage of time;
   given this reality, functions in this module make it mostly impossible to "coerce" a Julian Day
   obtained from a moment in Universal Time to Terrestrial Time (and vice-versa: ) Delta Time /must/ be calculated,
   and [leap seconds](https://en.wikipedia.org/wiki/Leap_second) in UT /must/ be taken into account. 
   Swiss Ephemeris provides functions to do these conversions safely by consulting historical data (hence the @IO@ restriction,)
   and the 'ToJulian' and 'FromJulian' typeclasses govern the interface for conversion for any
   given type: currently only @UTCTime@ from the Haskell time taxonomy is supported: a @Day@
   can trivially be first converted to/from @UTCTime@, and other values such as Haskell's
   own notion of @UniversalTime@ don't have immediate astronomical significance.
   
   The only somewhat "safe" coercion between time standards that doesn't go through IO
   is between @UT@ and @UT1@, though for @UTCTime@, this will be off by less than a second
   due to the nature of UTC vs. UT1. 
   
   For convenience, we provide a way of converting between 'Day' and any 'JulianDay' values purely,
   which relies on temporally unsound assumptions about the difference
   between the supported time standards; this works fine for dates, but is categorically
   wrong whenever a time of day is necessary. Go through the typeclass methods in that case.

   Some further reading:
   
   * https://wiki.haskell.org/Time
   * https://en.wikipedia.org/wiki/Terrestrial_Time
   * https://en.wikipedia.org/wiki/Universal_Time
   * https://en.wikipedia.org/wiki/Leap_second
   * https://en.wikipedia.org/wiki/%CE%94T_(timekeeping)
   * https://www.nist.gov/pml/time-and-frequency-division/time-realization/leap-seconds
   * https://www.ietf.org/timezones/data/leap-seconds.list

-}

-- | Various standards for measuring time that can be expressed as
-- Julian Days.
data TimeStandard
  = -- | Terrestrial Time (successor to Ephemeris Time)
    TT
  | -- | Universal Time, explicitly in its @UT1@ form.
    UT1
  | -- | Universal Time, in any of its forms; depending
    -- on how it was constructed (in most cases, UTC)
    UT
  deriving (Eq, Show)

----------------------------------------------------------
--- SINGLETONS
-- thanks to: https://blog.jle.im/entry/introduction-to-singletons-1.html
-- if this gets more use, consider using the 'singletons' package:
-- https://hackage.haskell.org/package/singletons-3.0
----------------------------------------------------------
-- | Singletons for pseudo-dependent type programming with
-- time standards. 
data SingTimeStandard :: TimeStandard -> Type where
  STT :: SingTimeStandard 'TT
  SUT1 :: SingTimeStandard 'UT1
  SUT :: SingTimeStandard 'UT
  
-- | Typeclass to recover the singleton for a given time standard
class SingTSI a where
  singTS :: SingTimeStandard a 

instance SingTSI 'TT where
  singTS = STT
instance SingTSI 'UT1 where
  singTS = SUT1
instance SingTSI 'UT where
  singTS = SUT
 
-- | A @JulianDay@ can have different provenances, witnessed
-- by its accompanying phantom type:
--
-- * It could've been converted, purely, from a UTC value,
--   as such, its witness is 'UT'
-- * It could'be been produced by consulting tidal/leap second
--   information, as done by the Swiss Ephemeris library,
--   in which case it's 'TT' (aka, somewhat wrongly, as Ephemeris
--   time,) or 'UT1'.
newtype JulianDay (s :: TimeStandard) = MkJulianDay {
                                          -- | Get the underlying 'Double' in 
                                          -- a 'JulianDay'. We intentionally do /not/
                                          -- export a way to finagle a 'Double' into a
                                          -- 'JulianDay': you'll have to obtain it
                                          -- through the various temporal conversion functions.
                                          getJulianDay :: Double}
  deriving (Eq, Show, Enum, Ord)

 
-- Aliases for those who dislike datakinds

-- | A terrestrial time as a Julian Day
type JulianDayTT = JulianDay 'TT

-- | A generic universal time as a Julian Day
type JulianDayUT = JulianDay 'UT

-- | A @UT1@ universal time as a Julian Day
type JulianDayUT1 = JulianDay 'UT1

-- | Represents an instant in sidereal time
newtype SiderealTime = SiderealTime {getSiderealTime:: Double}
  deriving (Show, Eq, Ord)

-- | A type that encodes an attempt to convert between
-- temporal types. 
newtype ConversionResult dt = ConversionResult {getConversionResult :: Either String dt}
  deriving (Show, Functor, Applicative, Monad)

instance Fail.MonadFail ConversionResult where
  fail = ConversionResult . Left

-- | Conversion from a temporal value of type @from@
-- to a 'JulianDay' in the 'TimeStandard' @jd@.
-- It's bound to IO _and_ a containing 'MonadFail' since
-- in the general case, we need to interact with
-- the outside world, and may fail, when consulting
-- the necessary data.
-- How can it fail? In short: at least for valid temporal
-- values constructed via the @time@ library, pretty much only
-- if you have an old version of Swiss Ephemeris that's not aware
-- of a recent leap second. 
class Fail.MonadFail m => ToJulianDay m jd from where
  toJulianDay :: from -> IO (m (JulianDay jd))

instance Fail.MonadFail m => ToJulianDay m 'UT UTCTime where
  toJulianDay = return . pure . utcToJulianDayUT

instance Fail.MonadFail m => ToJulianDay m 'UT1 UTCTime where
  toJulianDay = utcToJulianUT1

instance Fail.MonadFail m => ToJulianDay m 'TT UTCTime where
  toJulianDay = utcToJulianTT

-- | Conversion from a 'JulianDay' in the 'TimeStandard'
-- @jd@ to a temporal value of type @to@
-- It's bound to IO since historical data may need to be consulted;
-- however, as per the underlying library, it /cannot/ fail.
class FromJulianDay jd to where
  fromJulianDay :: JulianDay jd -> IO to

instance FromJulianDay 'UT UTCTime where
  fromJulianDay = pure . julianDayUTToUTC

instance FromJulianDay 'UT1 UTCTime where
  fromJulianDay = julianUT1ToUTC

instance FromJulianDay 'TT UTCTime where
  fromJulianDay = julianTTToUTC

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------

-- | Constructor with chaperone: you have to provide a witness to a time standard
-- to produce a 'JulianDay' directly from a 'Double'. This is mostly
-- intended for internal use, if you find yourself using this function,
-- you're probably producing an unreliable value: consider using the
-- 'ToJulianDay' instance of a reliable temporal type
-- (like 'UTCTime',) before reaching for this function.
mkJulianDay :: SingTimeStandard ts -> Double -> JulianDay ts
mkJulianDay _ = MkJulianDay


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

-- | Add time to catch up UT to TT; doesn't make sense
-- for other time standards.
addDeltaTime :: JulianDay 'UT1 -> Double -> JulianDay 'TT
addDeltaTime (MkJulianDay jd) dt = MkJulianDay (jd + dt)

-- | Subtract time to 'slow down' TT to UT; doesn't make
-- sense for other time standards.
subtractDeltaTime :: JulianDay 'TT -> Double -> JulianDay 'UT1
subtractDeltaTime (MkJulianDay jd) dt = MkJulianDay (jd - dt)


-------------------------------------------------------------------------------
-- Lossy (pure, and /almost/ accurate for the contemporary timescale) conversions
-- for Day values.
-------------------------------------------------------------------------------

-- | Convenience "pure" function that pretends
-- that a day at noon can be converted to /any/ JulianDay;
-- in reality, it pretends that a JulianDay /in UT/ stands
-- in for any other (e.g. in 'UT1' or 'TT') -- this is "good enough"
-- for a day at noon since, at least in 2021, UT is only off
-- by less than a second from UT1, and only behind TT by a few
-- seconds 
dayToJulianDay :: Day -> JulianDay ts
dayToJulianDay day =
  gregorianToJulian y m d 12
  where
    (y, m, d) = toGregorian day
    
-- | Convenience "pure" function that takes an arbitrary
-- 'JulianDay' value in any time standard, converts it to noon,
-- and then to the corresponding 'Day.' Exploits the same circumstantial
-- truths about time as 'dayToJulianDay'
dayFromJulianDay :: JulianDay ts -> Day
dayFromJulianDay jd =
  fromGregorian y m d
  where
  (y,m,d,_) = gregorianFromJulianDay . julianNoon $ jd
  
-------------------------------------------------------------------------------
-- Lossy (but pure, and /almost/ accurate) conversion functions for UT values. 
-------------------------------------------------------------------------------

-- | Can produce a 'JulianDay' in any scale, but only values in 'UT'
-- are considered truthful. Hence the @fake@ moniker in the 'TT'
-- specialization, below.
gregorianToJulian :: Integer -> Int -> Int -> Double -> JulianDay ts
gregorianToJulian year month day hour =
  MkJulianDay . realToFrac $ c_swe_julday y m d h gregorian
  where
    y = fromIntegral year
    m = fromIntegral month
    d = fromIntegral day
    h = realToFrac hour
 
-- | Given components of a gregorian day (and time,)
-- produce a 'JulianDay' in the generic 'UT' time standard;
-- the precision of the resulting Julian Day will only be as good
-- as its input; if obtained by other means than via a 'UTCTime',
-- it's likely to be off by up to a second when compared with a 'UT1' value.
-- (on the other hand, it doesn't consult any data so it's not bound to 'IO')
-- This is provided for convenience, but if you have date components, you'd
-- be better off producing a valid 'UTCTime' to send to the 'toJulian'
-- family of functions, via e.g. [@fromGregorianValid@](https://hackage.haskell.org/package/time-1.12/docs/Data-Time-Calendar.html#v:fromGregorianValid)
-- and [@makeTimeOfDayValid@](https://hackage.haskell.org/package/time-1.12/docs/Data-Time-LocalTime.html#v:makeTimeOfDayValid)
gregorianToJulianDayUT :: Integer -> Int -> Int -> Double -> JulianDay 'UT
gregorianToJulianDayUT = gregorianToJulian
    
-- | If you care about accuracy, don't use this function!!! It's merely provided
-- as a facility for testing or situations where you don't really care about
-- the truth: the /actual/ Julian Day produced by this function is an absolute,
-- universal time, we just naughtily repackage it as a terrestrial time here.
-- If you want a /real/ TerrestrialTime, either convert a valid temporal value
-- through the 'toJulianDay' polymorphic function, or use 'universalToTerrestrial'
-- if you already have a 'UT1' value.
gregorianToFakeJulianDayTT :: Integer -> Int -> Int -> Double -> JulianDay 'TT
gregorianToFakeJulianDayTT = gregorianToJulian


-- | Given a 'JulianDay' in any standard,
-- produce the date/time components of a gregorian date.
gregorianFromJulianDay :: JulianDay ts -> (Integer, Int, Int, Double)
gregorianFromJulianDay (MkJulianDay jd) =
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

-- | Given a JulianDay in UT, produce the equivalent Gregorian date's components.
gregorianFromJulianDayUT :: JulianDay 'UT -> (Integer, Int, Int, Double)
gregorianFromJulianDayUT = gregorianFromJulianDay

-- | This is a bit of a misnomer: the "fake" value isn't the input,
-- it's the output: it produces a value as if the input was in UT, thus
-- running afoul of both leap seconds and delta time. Only useful
-- in contexts where accuracy is not valued. To get a somewhat more
-- trustworthy value, and still not have to go into 'IO', check out
-- 'dayFromJulianDay', which produces only the 'Day' part of a date.
gregorianFromFakeJulianDayTT :: JulianDay 'TT -> (Integer, Int, Int, Double)
gregorianFromFakeJulianDayTT = gregorianFromJulianDay

picosecondsInHour :: Double
picosecondsInHour = 3600 * 1e12

-- | Given a 'UTCTime', produce a 'JulianDay' purely.
-- It can only be said to be in 'UT', since Haskell's
-- UTC is an approximation of 'UT1', off to up to a second.
-- If you want precision, use 'utcToJulianDays' (which returns
-- both the 'UT1' and 'TT' timestamps,) or 'utcToJulianUT1'.
-- Keep in mind though, that they're both in 'IO' /and/ may
-- return errors.
utcToJulianDayUT :: UTCTime -> JulianDay 'UT
utcToJulianDayUT (UTCTime day time) =
  gregorianToJulianDayUT y m d h
  where
    (y, m, d) = toGregorian day
    h = (1 / picosecondsInHour) * fromIntegral (diffTimeToPicoseconds time)

-- | Given a JulianDay in the vague 'UT' time standard,
-- produce a 'UTCTime' purely.
julianDayUTToUTC :: JulianDay 'UT -> UTCTime
julianDayUTToUTC jd =
  UTCTime day dt
  where
    (y, m, d, h) = gregorianFromJulianDayUT jd
    day = fromGregorian y m d
    dt = picosecondsToDiffTime $ round $ h * picosecondsInHour

-- | See 'utcToJulianDayUT' -- this function is provided
-- for convenience in contexts where the ~1s accuracy gain
-- is not worth the more complicated type signature of
-- 'toJulian', but you'll get a "lesser" JulianDay
-- that's only as precise as its input.
utcToJulian :: UTCTime -> JulianDay 'UT
utcToJulian = utcToJulianDayUT

-- | See 'julianDayUTToUTC' -- this function is provided
-- for convenience in contexts where a slightly innacurate
-- JulianDay is worth it to stay in a pure context, otherwise,
-- see 'fromJulian'.
julianToUTC :: JulianDay 'UT -> UTCTime
julianToUTC = julianDayUTToUTC

-- | Utility function to split a 'UTCTime' into the constituent
-- parts expected by the underlying lib.
splitUTC :: UTCTime -> (Integer, Int, Int, TimeOfDay)
splitUTC (UTCTime day time) =
  (y, m, d, tod)
  where
    (y, m, d) = toGregorian day
    tod = timeToTimeOfDay time

-------------------------------------------------------------------------------
-- UTC->(UT1,TT) functions
-------------------------------------------------------------------------------

-- | Convert a 'UTCTime' into a tuple of Terrestrial Time and UT1 Julian Days;
-- the underlying C function can return errors if:
--
-- * Any of the individual date components are invalid
-- * The given date has a leap second that it is not aware of (due to either
--   input error or the library not being out of date.)
--
-- A legitimately obtained 'UTCTime' (i.e. not crafted by hand, but by some means
-- of validated time input/ingestion) is very unlikely to error out in the former
-- of those scenarios, but there /is/ a chance it may fail in the latter; if you
-- encounter this, the first step would be to update the Swiss Ephemeris library,
-- since they bundle an array of leap seconds; otherwise, you can provide a file
-- called @seleapsec.txt@ in your configured ephemeris path,
-- see: [8.3.  Handling of leap seconds and the file seleapsec.txt](https://www.astro.com/swisseph/swephprg.htm#_Toc71121195)
utcToJulianDays :: Fail.MonadFail m => UTCTime -> IO (m (JulianDay 'TT, JulianDay 'UT1))
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

        if retval < 0
          then do
            msg <- peekCAString serr
            return $ Fail.fail msg
          else do
            (tt : ut1 : _) <- peekArray 2 dret
            return $ pure (MkJulianDay . realToFrac $ tt, MkJulianDay . realToFrac $ ut1)

utcToJulianTT :: Fail.MonadFail m => UTCTime -> IO (m (JulianDay 'TT))
utcToJulianTT ut =
  fmap fst <$> utcToJulianDays ut

utcToJulianUT1 :: Fail.MonadFail m => UTCTime -> IO (m (JulianDay 'UT1))
utcToJulianUT1 ut =
  fmap snd <$> utcToJulianDays ut

-------------------------------------------------------------------------------
-- (UT1,TT) -> UTC functions
-------------------------------------------------------------------------------

gregorianFromJulianDayTT :: JulianDay 'TT -> IO (Integer, Int, Int, TimeOfDay)
gregorianFromJulianDayTT (MkJulianDay tt) = do
  alloca $ \jyear -> alloca $ \jmon -> alloca $ \jday -> alloca $
    \jhour -> alloca $ \jmin -> alloca $ \jsec -> do
      _ <-
        c_swe_jdet_to_utc
          (realToFrac tt)
          gregorian
          jyear
          jmon
          jday
          jhour
          jmin
          jsec
      year <- peek jyear
      month <- peek jmon
      day <- peek jday
      hour <- peek jhour
      minute <- peek jmin
      second <- peek jsec
      return
        ( fromIntegral year,
          fromIntegral month,
          fromIntegral day,
          TimeOfDay (fromIntegral hour) (fromIntegral minute) (realToFrac second)
        )

gregorianFromJulianDayUT1 :: JulianDay 'UT1 -> IO (Integer, Int, Int, TimeOfDay)
gregorianFromJulianDayUT1 (MkJulianDay ut1) = do
  alloca $ \jyear -> alloca $ \jmon -> alloca $ \jday -> alloca $
    \jhour -> alloca $ \jmin -> alloca $ \jsec -> do
      _ <-
        c_swe_jdut1_to_utc
          (realToFrac ut1)
          gregorian
          jyear
          jmon
          jday
          jhour
          jmin
          jsec
      year <- peek jyear
      month <- peek jmon
      day <- peek jday
      hour <- peek jhour
      minute <- peek jmin
      second <- peek jsec
      return
        ( fromIntegral year,
          fromIntegral month,
          fromIntegral day,
          TimeOfDay (fromIntegral hour) (fromIntegral minute) (realToFrac second)
        )

julianTTToUTC :: JulianDay 'TT -> IO UTCTime
julianTTToUTC tt = do
  (y, m, d, tod) <- gregorianFromJulianDayTT tt
  pure $ UTCTime (fromGregorian y m d) (timeOfDayToTime tod)

julianUT1ToUTC :: JulianDay 'UT1 -> IO UTCTime
julianUT1ToUTC ut1 = do
  (y, m, d, tod) <- gregorianFromJulianDayUT1 ut1
  pure $ UTCTime (fromGregorian y m d) (timeOfDayToTime tod)

-------------------------------------------------------------------------------
-- Delta Time
-------------------------------------------------------------------------------

-- | Somewhat naïve delta time calculation: if no ephemeris
-- mode has been selected, it will use the default tidal
-- acceleration value as per the DE431 JPL ephemeris,
-- otherwise, it will use whatever ephemeris is currently set.
-- It's considered unsafe since switching ephemeris modes will
-- result in an incongruent delta time. See 'safeDeltaTime'
unsafeDeltaTime :: JulianDay 'UT1 -> IO Double
unsafeDeltaTime (MkJulianDay jd) =
  realToFrac <$> c_swe_deltat (realToFrac jd)

-- | Alias for 'unsafeDeltaTime'
deltaTime :: JulianDay 'UT1 -> IO Double
deltaTime = unsafeDeltaTime

-- | Same as 'deltaTime', but fails if the given 'EphemerisOption'
-- doesn't agree with the current ephemeris mode.
safeDeltaTime :: Fail.MonadFail m => EphemerisOption -> JulianDay 'UT1 -> IO (m Double)
safeDeltaTime epheOption (MkJulianDay jd) =
  allocaErrorMessage $ \serr -> do
    dt <- c_swe_deltat_ex (realToFrac jd) (ephemerisOptionToFlag epheOption) serr
    if dt < 0
      then do
        err <- peekCAString serr
        return $ Fail.fail err
      else do
        return . pure . realToFrac $ dt

-- | Try to produce a delta time for the @SwissEphemeris@ ephemeris mode,
-- will fail if the current mode isn't set to @SwissEphemeris@.
deltaTimeSE :: Fail.MonadFail m => JulianDay 'UT1 -> IO (m Double)
deltaTimeSE = safeDeltaTime UseSwissEphemeris

-- | Convert between an instant in UT1 to TT, as a @JulianDay@, may
-- produce inaccurate results if an ephemeris mode isn't set explicitly.
universalToTerrestrial :: JulianDay 'UT1 -> IO (JulianDay 'TT)
universalToTerrestrial jdut = do
  deltaT <- unsafeDeltaTime jdut
  pure $ addDeltaTime jdut deltaT

-- | Convert between an instant in UT1 to TT, as a @JulianDay@, using an explicit
-- ephemeris mode; fails if not currently working in the expected mode.
universalToTerrestrialSafe :: Fail.MonadFail m => EphemerisOption -> JulianDay 'UT1 -> IO (m (JulianDay 'TT))
universalToTerrestrialSafe eo jdut = do
  deltaT <- safeDeltaTime eo jdut
  pure $ addDeltaTime jdut <$> deltaT

-- | 'universaltoTerrestrialSafe', set to @SwissEphemeris@
universalToTerrestrialSE :: Fail.MonadFail m => JulianDay 'UT1 -> IO (m (JulianDay 'TT))
universalToTerrestrialSE = universalToTerrestrialSafe UseSwissEphemeris

-------------------------------------------------------------------------------
-- Sidereal Time
-------------------------------------------------------------------------------
--
-- | Given `JulianDay`, get `SiderealTime`. May consult ephemerides data, hence it being in IO,
-- will have to calculate obliquity at the given julian time, so it'll be slightly slower than
-- `calculateSiderealTime`.
julianToSiderealSimple :: JulianDay 'UT1 -> IO SiderealTime
julianToSiderealSimple (MkJulianDay jt) = do
  sidTime <- c_swe_sidtime (realToFrac jt)
  pure . SiderealTime $ realToFrac sidTime

-- | Given a `JulianDay` and `ObliquityInformation`, calculate the equivalent `SiderealTime`.
-- prefer it over `calculateSiderealTimeSimple` if you already obtained `ObliquityInformation`
-- for another calculation.
julianToSidereal :: JulianDay 'UT1 -> ObliquityInformation -> IO SiderealTime
julianToSidereal (MkJulianDay jt) on = do
  let obliq = realToFrac $ eclipticObliquity on
      nut = realToFrac $ nutationLongitude on
  sidTime <- c_swe_sidtime0 (realToFrac jt) obliq nut
  pure . SiderealTime $ realToFrac sidTime

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
deltaTime jdut1 -- (JulianDay 2459399.46243737)
we get:
8.019823376913656e-4

and then:
2459399.46243737 + 8.019823376913656e-4
should give us TT?
the result is:
2459399.463239352

which is exactly TT!!!
-}
