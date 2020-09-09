{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Foreign.SwissEphemeris where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <swephexp.h>

newtype PlanetNumber = PlanetNumber
  { unPlanetNumber :: CInt } deriving (Eq, Show)

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

-- | Calculate the position of a body, given a time in
-- Universal Time.
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

foreign import ccall unsafe "swephexp.h swe_cotrans_sp"
    c_swe_cotrans_sp :: Ptr CDouble -- double[6]: lng, lat, distance
                     -> Ptr CDouble -- double[6]: ascension, declination, distance (or viceversa)
                     -> CDouble     -- obliquity of the ecliptic.
                     -> IO ()

---
--- NOT IMPLEMENTED IN THE PUBLIC API YET:
--- 

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

-- | Calculate the position of a planet, given a time
-- in Ephemeris Time. swe_calc_ut calls it internally,
-- so if you already have the delta time for other purposes,
-- it'll be slightly faster to call this.
foreign import ccall unsafe "swephexp.h swe_calc"
    c_swe_calc :: CDouble -- Ephemeris time. This is Julian + delta time.
               -> PlanetNumber
               -> CalcFlag
               -> Ptr CDouble
               -> CString
               -> (IO CalcFlag)

-- | Calculate the delta time for a given julian time,
-- delta time + julian time = ephemeris time
-- NOTE: there's also swe_deltat_ex which takes an ephemeris
-- flag explicitly, vs. the current global value.
-- my calculations work in one ephemeris, so this one is suitable.
foreign import ccall unsafe "swephexp.h swe_deltat"
    c_swe_deltat :: CDouble -- Julian time
                 -> (IO CDouble)

-- | Calculate the sidereal time for a given julian time.
-- NOTE: there's also swe_sidtime0 which requires obliquity
-- and nutation, this one computes them internally.
foreign import ccall unsafe "swephexp.h swe_sidtime"
    c_swe_sidtime :: CDouble -- Julian time
                   -> (IO CDouble)

-- | Calculate the sidereal time for a given julian time.
-- NOTE: there's also swe_sidtime0 which requires obliquity
-- and nutation, this one computes them internally.
foreign import ccall unsafe "swephexp.h swe_sidtime0"
    c_swe_sidtime0 :: CDouble -- Julian time
                   -> CDouble -- obliquity
                   -> CDouble -- nutation
                   -> (IO CDouble)
