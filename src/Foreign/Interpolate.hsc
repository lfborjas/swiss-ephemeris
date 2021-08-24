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
