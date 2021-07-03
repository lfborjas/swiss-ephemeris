{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-| 
Module: Foreign.SwissEphemeris
Description: Declarations of bindings to the underlying C library. Import at your own risk!

Exposes very low-level FFI bindings to the C library. Use the @SwissEphemeris@ module and its more
Haskell-friendly exports.
-}


module Foreign.SwissEphemeris where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <swephexp.h>

newtype PlanetNumber = PlanetNumber
  { unPlanetNumber :: CInt } deriving (Eq, Show)

instance Storable PlanetNumber where
  sizeOf _ = sizeOf (undefined::CInt)
  alignment  = sizeOf
  peek ptr = do
    n <- peek $ castPtr ptr
    pure $ PlanetNumber n
  poke ptr (PlanetNumber n) =
    poke (castPtr ptr) n


newtype EpheFlag = EpheFlag
  { unEpheFlag :: CInt } deriving (Eq, Show)

newtype GregFlag = GregFlag
  { unGregFlag :: CInt } deriving (Eq, Show)

newtype CalcFlag = CalcFlag
  { unCalcFlag :: CInt } deriving (Eq, Show)

newtype SplitDegFlag = SplitDegFlag
  { unSplitDegFlag :: CInt } deriving (Eq, Show)

-- following:
-- https://en.wikibooks.org/wiki/Haskell/FFI#Enumerations

#{enum PlanetNumber, PlanetNumber,
  sun  = SE_SUN
 , moon = SE_MOON
 , mercury = SE_MERCURY
 , venus = SE_VENUS
 , mars = SE_MARS
 , jupiter = SE_JUPITER
 , saturn = SE_SATURN
 , uranus = SE_URANUS
 , neptune = SE_NEPTUNE
 , pluto = SE_PLUTO
 , meanNode    = SE_MEAN_NODE
 , trueNode = SE_TRUE_NODE
 , meanApog = SE_MEAN_APOG
 , oscuApog = SE_OSCU_APOG
 , earth    = SE_EARTH
 , chiron = SE_CHIRON
 , specialEclNut = SE_ECL_NUT
 }

#{enum GregFlag, GregFlag
 , julian = SE_JUL_CAL
 , gregorian = SE_GREG_CAL
 }

-- there are _many_ more, see `swephexp.h:186-215`
#{enum CalcFlag, CalcFlag
 , speed = SEFLG_SPEED
 , swissEph = SEFLG_SWIEPH
 , equatorialPositions = SEFLG_EQUATORIAL
 }

#{enum EpheFlag, EpheFlag
, useSwissEph = SEFLG_SWIEPH
, useJplEph   = SEFLG_JPLEPH
, useMoshierEph = SEFLG_MOSEPH
}

#{enum SplitDegFlag, SplitDegFlag
 , splitRoundSec = SE_SPLIT_DEG_ROUND_SEC
 , splitRoundMin = SE_SPLIT_DEG_ROUND_MIN
 , splitRoundDeg = SE_SPLIT_DEG_ROUND_DEG
 , splitZodiacal = SE_SPLIT_DEG_ZODIACAL
 , splitNakshatra = SE_SPLIT_DEG_NAKSHATRA
 , splitKeepSign  = SE_SPLIT_DEG_KEEP_SIGN
 , splitKeepDeg   = SE_SPLIT_DEG_KEEP_DEG
 }

foreign import ccall unsafe "swephexp.h swe_set_ephe_path"
    c_swe_set_ephe_path :: CString -> IO ()

foreign import ccall unsafe "swephexp.h swe_close"
    c_swe_close :: IO ()

foreign import ccall unsafe "swephexp.h swe_julday"
    c_swe_julday :: CInt -- year
                 -> CInt -- month
                 -> CInt -- day 
                 -> CDouble -- hour
                 -> GregFlag
                 -> CDouble

-- | Reverse of `c_swe_julday`: produce a gregorian date
foreign import ccall unsafe "swephexp.h swe_revjul"
    c_swe_revjul :: CDouble
                 -> GregFlag
                 -> Ptr CInt -- year
                 -> Ptr CInt -- month
                 -> Ptr CInt -- day
                 -> Ptr CDouble -- hour
                 -> IO ()


-- | Calculate the position of a body, given a time in
-- Universal Time. Note that this is marginally more expensive than
-- @swe_calc@, but I use this one to keep consistency with @swe_houses@.
foreign import ccall unsafe "swephexp.h swe_calc_ut"
    c_swe_calc_ut :: CDouble
                  -> PlanetNumber
                  -> CalcFlag
                  -> Ptr CDouble
                  -> CString
                  -> (IO CalcFlag)

-- | Get the house cusps and other relevant angles for
-- a given time and place. Note that there's also a
-- @swe_houses_armc@ if one happens to have the ARMC
-- and the ecliptic obliquity handy from other calculations.
foreign import ccall unsafe "swephexp.h swe_houses"
    c_swe_houses :: CDouble -- in fact, a Julian day "Number"
                 -> CDouble -- Lat
                 -> CDouble -- Long
                 -> CInt -- house system (see .hs version of this file)
                 -> Ptr CDouble -- cusps, 13 doubles (or 37 in system G)
                 -> Ptr CDouble -- ascmc, 10 doubles
                 -> (IO CInt)

-- | Calculate the house a planet is in. Takes into account
-- obliquity of the ecliptic. Works for all house systems, 
-- except Koch.
foreign import ccall unsafe "swephexp.h swe_house_pos"
    c_swe_house_pos :: CDouble -- ARMC
                    -> CDouble -- Geographical latitude
                    -> CDouble -- Obliquity
                    -> CInt    -- house system
                    -> Ptr CDouble -- double[2], long/lat of body.
                    -> CString     -- char[256] for errors.
                    -> (IO CDouble)

-- | Low-level function to translate between coordinate systems, with speed position included.
foreign import ccall unsafe "swephexp.h swe_cotrans_sp"
    c_swe_cotrans_sp :: Ptr CDouble -- double[6]: lng, lat, distance
                     -> Ptr CDouble -- double[6]: ascension, declination, distance (or viceversa)
                     -> CDouble     -- obliquity of the ecliptic.
                     -> IO ()

-- | Split a given ecliptic longitude into sign (number)
-- degrees, minutes and seconds.
foreign import ccall unsafe "swephexp.h swe_split_deg"
    c_swe_split_deg :: CDouble -- longitude
                    -> SplitDegFlag -- behavior of rounding/assigning to signs
                    -> Ptr CInt -- degrees
                    -> Ptr CInt -- minutes
                    -> Ptr CInt -- seconds
                    -> Ptr CDouble -- seconds fraction
                    -> Ptr CInt    -- sign/nakshatra
                    -> IO ()       -- returns void.

-- | Calculate the delta time for a given julian time,
-- delta time + julian time = ephemeris time
-- NOTE: there's also @swe_deltat_ex@ which takes an ephemeris
-- flag explicitly, vs. the current global value.
-- my calculations work in one ephemeris, so this one is suitable.
foreign import ccall unsafe "swephexp.h swe_deltat"
    c_swe_deltat :: CDouble -- Julian time
                 -> (IO CDouble)

-- | Calculate the sidereal time for a given julian time.
-- NOTE: there's also @swe_sidtime0@ which requires obliquity
-- and nutation, this one computes them internally.
foreign import ccall unsafe "swephexp.h swe_sidtime"
    c_swe_sidtime :: CDouble -- Julian time
                   -> (IO CDouble)

-- | Calculate the sidereal time for a given julian time, obliquity and nutation.
foreign import ccall unsafe "swephexp.h swe_sidtime0"
    c_swe_sidtime0 :: CDouble -- Julian time
                   -> CDouble -- obliquity
                   -> CDouble -- nutation
                   -> (IO CDouble)

-- | Same as 'c_swe_deltat', but expects one to have explicitly
-- selected an ephemeris mode, and returns a warning if not.
foreign import ccall unsafe "swephexp.h swe_deltat_ex"
    c_swe_deltat_ex :: CDouble
                    -- ^ JulianTime, in a UT scale.
                    -> EpheFlag
                    -- ^ Ephemeris to use (for tidal acceleration data)
                    -> CString
                    -- ^ For warning/error message
                    -> IO CDouble
                    -- ^ Delta T, if the correct ephemeris
                    -- is being used.

{- TODO
  Not added:
 
 * swe_date_conversion (Haskell already has functions for this,
   in the time package)
 * swe_utc_time_zone (expects an offset, in which case the
   ZonedDateTime -> UTCTime conversion in Haskell also suffices.) 
-}

-- | Given a Universal Time input (UTC, but it's considered
-- to be UT1 if before 1971, or if the leap is too great.)
foreign import ccall unsafe "swephexp.h swe_utc_to_jd"
    c_swe_utc_to_jd :: CInt
                    -- ^ year
                    -> CInt
                    -- ^ month
                    -> CInt
                    -- ^ day
                    -> CInt
                    -- ^ hour
                    -> CInt
                    -- ^ min
                    -> CDouble
                    -- ^ sec
                    -> GregFlag
                    -- ^ gregorian/julian
                    -> Ptr CDouble
                    -- ^ @dret[2]@, where pos 0 is 
                    -- the Julian Day in TT (née ET)
                    -- and pos 1 is the Julian Day in UT1
                    -> CString
                    -- ^ error string
                    -> IO CInt
                    -- OK/ERR
                  
foreign import ccall unsafe "swephexp.h swe_jdet_to_utc"
    c_swe_jdet_to_utc :: CDouble
                      -- ^ JD
                      -> GregFlag
                      -- ^ julian/gregorian
                      -> Ptr CInt
                      -- ^ year
                      -> Ptr CInt
                      -- ^ month
                      -> Ptr CInt
                      -- ^ day 
                      -> Ptr CInt
                      -- ^ hour
                      -> Ptr CInt
                      -- ^ min
                      -> Ptr CDouble
                      -- ^ sec
                      -> IO ()
                      
foreign import ccall unsafe "swephexp.h swe_jut1_to_utc"
    c_swe_jut1_to_utc :: CDouble
                      -- ^ JD
                      -> GregFlag
                      -- ^ julian/gregorian
                      -> Ptr CInt
                      -- ^ year
                      -> Ptr CInt
                      -- ^ month
                      -> Ptr CInt
                      -- ^ day 
                      -> Ptr CInt
                      -- ^ hour
                      -> Ptr CInt
                      -- ^ min
                      -> Ptr CDouble
                      -- ^ sec
                      -> IO ()
