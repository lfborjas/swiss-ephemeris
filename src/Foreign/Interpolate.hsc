{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- |
-- Module: Foreign.Interpolate
-- 
-- Functions to find moments of exactitude; currently only an
-- interpolation of exact moment of direction change for a given planet.

module Foreign.Interpolate where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.SwissEphemeris

#include <interpolate.h>


foreign import ccall unsafe "interpolate.h swe_next_direction_change"
  c_swe_next_direction_change :: CDouble
                              -- ^ JD (TT) to start search
                              -> PlanetNumber
                              -- ^ planet
                              -> CalcFlag
                              -- ^ @iflag@ -- ephemeris flags
                              -> Ptr CDouble
                              -- ^ [return] JD(TT) when direction changes
                              -> Ptr CInt
                              -- ^ [return] <0 if new dir is retrograde
                              -> CString
                              -- ^ error message
                              -> IO CInt
                              -- ^ OK/ERR

foreign import ccall unsafe "interpolate.h swe_next_direction_change_ut"
  c_swe_next_direction_change_ut :: CDouble
                                 -- ^ JD (UT1) to start search
                                 -> PlanetNumber
                                 -- ^ planet
                                 -> CalcFlag
                                 -- ^ @iflag@ -- ephemeris flags
                                 -> Ptr CDouble
                                 -- ^ [return] JD(UT1) when direction changes
                                 -> Ptr CInt
                                 -- ^ [return] <0 if new dir is retrograde
                                 -> CString
                                 -- ^ error message
                                 -> IO CInt
                                 -- ^ OK/ERR

foreign import ccall unsafe "interpolate.h swe_next_direction_change_between"
  c_swe_next_direction_change_between :: CDouble
                                      -- ^ JD (TT) to start search
                                      -> CDouble
                                      -- ^ JD (TT) to end search
                                      -> PlanetNumber
                                      -- ^ planet
                                      -> CalcFlag
                                      -- ^ @iflag@ -- ephemeris flags
                                      -> Ptr CDouble
                                      -- ^ [return] JD(TT) when direction changes
                                      -> Ptr CInt
                                      -- ^ [return] <0 if new dir is retrograde
                                      -> CString
                                      -- ^ error message
                                      -> IO CInt
                                      -- ^ OK/ERR

foreign import ccall unsafe "interpolate.h swe_next_direction_change_ut_between"
  c_swe_next_direction_change_ut_between :: CDouble
                                         -- ^ JD (UT1) to start search
                                         -> CDouble
                                         -- ^ JD (UT1) to end search
                                         -> PlanetNumber
                                         -- ^ planet
                                         -> CalcFlag
                                         -- ^ @iflag@ -- ephemeris flags
                                         -> Ptr CDouble
                                         -- ^ [return] JD(UT1) when direction changes
                                         -> Ptr CInt
                                         -- ^ [return] <0 if new dir is retrograde
                                         -> CString
                                         -- ^ error message
                                         -> IO CInt
                                         -- ^ OK/ERR

foreign import ccall unsafe "interpolate.h swe_interpolate"
  c_swe_interpolate :: PlanetNumber
                    -- ^ planet crossing
                    -> CDouble
                    -- ^ longitude to cross
                    -> CDouble
                    -- ^ JD(TT) a moment before the crossing
                    -> CDouble
                    -- ^ JD(TT) a moment after the crossing
                    -> CalcFlag
                    -- iflag
                    -> Ptr CDouble
                    -- ^ moment of crossing
                    -> CString
                    -- err message
                    -> IO CInt
                    -- ^ OK/ERR

foreign import ccall unsafe "interpolate.h swe_interpolate_ut"
  c_swe_interpolate_ut :: PlanetNumber
                       -- ^ planet crossing
                       -> CDouble
                       -- ^ longitude to cross
                       -> CDouble
                       -- ^ JD(UT1) a moment before the crossing
                       -> CDouble
                       -- ^ JD(UT1) a moment after the crossing
                       -> CalcFlag
                       -- iflag
                       -> Ptr CDouble
                       -- ^ moment of crossing
                       -> CString
                       -- err message
                       -> IO CInt
                       -- ^ OK/ERR

foreign import ccall unsafe "interpolate.h swe_interpolate_moon_phase"
  c_swe_interpolate_moon_phase
    :: CDouble
    -- ^ phase angle
    -> CDouble
    -- ^ JD(TT) a moment before the phase is exact
    -> CDouble
    -- ^ JD(TT) a moment after the phase is exact
    -> CalcFlag
    -- ^ iflag
    -> Ptr CDouble
    -- ^ moment of exactitude
    -> CString
    -- ^ err message
    -> IO CInt
    -- ^ OK/ERR

foreign import ccall unsafe "interpolate.h swe_interpolate_moon_phase_ut"
  c_swe_interpolate_moon_phase_ut
    :: CDouble
    -- ^ phase angle
    -> CDouble
    -- ^ JD(UT1) a moment before the phase is exact
    -> CDouble
    -- ^ JD(UT1) a moment after the phase is exact
    -> CalcFlag
    -- ^ iflag
    -> Ptr CDouble
    -- ^ moment of exactitude
    -> CString
    -- ^ err message
    -> IO CInt
    -- ^ OK/ERR
