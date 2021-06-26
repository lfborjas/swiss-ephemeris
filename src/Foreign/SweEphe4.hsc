{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module: Foreign.SweEphe4
--
-- Exposes functions to interact with the "ephe4" format of pre-calculated
-- ephemeris. 

module Foreign.SweEphe4 where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <swephexp.h>
#include <configurable_sweephe4.h>

-- | "placalc" style enum of precalculate-able bodies. 
-- All are provided for convenience, but take note that
-- only planets up to 'lastPlanet' are guaranteed to be present
-- via this FFI in any files produced via the bundled source.
newtype PlacalcPlanet = PlacalcPlanet
  { unPlacalcPlanet :: CInt } deriving (Eq, Show, Enum)

#{enum PlacalcPlanet, PlacalcPlanet
, pSun = PLACALC_SUN
, pMoon = PLACALC_MOON
, pMercury = PLACALC_MERCURY
, pVenus = PLACALC_VENUS
, pMars = PLACALC_MARS
, pJupiter = PLACALC_JUPITER
, pSaturn = PLACALC_SATURN
, pUranus = PLACALC_URANUS
, pNeptune = PLACALC_NEPTUNE
, pPluto = PLACALC_PLUTO
, pMeanNode = PLACALC_MEAN_NODE
, pTrueNode = PLACALC_TRUE_NODE
, pChiron = PLACALC_CHIRON
, pLilith = PLACALC_LILITH
, pCeres = PLACALC_CERES
, pPallas = PLACALC_PALLAS
, pJuno = PLACALC_JUNO
, pVesta = PLACALC_VESTA
, pHeliocentricEarth = PLACALC_EARTHHEL
, pParsFortunae = PLACALC_PFORTUNAE
}

-- | Options for requesting pre-calculated ephemeris;
-- Unless the use case calls for it, it's cheap and fast to just
-- `includeAll` (which should evaluate to @0@.)
newtype PlanetListFlag = PlanetListFlag
  { unPlanetListFlag :: CInt } deriving (Eq, Show)

#{enum PlanetListFlag, PlanetListFlag
, includeAllPlanets = EP_ALL_PLANETS
, includeEcliptic = EP_ECL_BIT
, includeNutation = EP_NUT_BIT
, includeAll = EP_ALL_BITS
}

-- | Lengths and indices for pre-calculated ephemeris.
-- They're only true for a file **created by the same
-- code in use**. Memory corruption can occur if reading
-- a file created with fewer or more planets/bodies, or
-- in an incompatible architecture!
newtype EpheConst = EpheConst
  { unEpheConst :: CInt } deriving (Eq, Show)

-- These are also "planet flags," but it felt weird to
-- present them as options, when they're more like
numberOfFactors, numberOfPlanets, eclipticIndex, nutationIndex :: EpheConst
numberOfFactors = EpheConst #const EP_NP
numberOfPlanets = EpheConst #const EP_CALC_N
eclipticIndex = EpheConst #const EP_ECL_INDEX
nutationIndex = EpheConst #const EP_NUT_INDEX

-- | Convenience helper to turn an 'EpheConst'
-- into an int for indexed access.
constToIndex :: EpheConst -> Int
constToIndex = fromIntegral . unEpheConst 

-- | The last planet that the current pre-calculated
-- files will contain, if produced with the same sources
-- in `csrc`. 
-- You'll have to edit the C source
-- and compile, then run the `swegen` target to re-produce
-- files with more or fewer planets
lastPlanet :: PlacalcPlanet
lastPlanet = PlacalcPlanet . unEpheConst $ numberOfPlanets

-- | Options for additional computations when reading.
-- The authors recommend to always `includeSpeed`, since
-- it's a negligible overhead and can be useful.
-- If `mustUseStoredEphe` is not set, reading dates outside
-- of the precalculated range won't fail, and instead fall back
-- to the more expensive @swecalc@ -- which can still fail
-- if requesting a body that the current swiss ephemeris mode
-- is unable to calculate (e.g. requesting Chiron when no
-- asteroid data is present.)
newtype EpheCalcFlag = EpheCalcFlag
  { unEpheCalcFlag :: CInt } deriving (Eq, Show)

#{enum EpheCalcFlag, EpheCalcFlag
, includeSpeed = EP_BIT_SPEED
, mustUseStoredEphe = EP_BIT_MUST_USE_EPHE
}


-- | Set the base pre-calculated ephemeris path. This function is
-- called automatically by the read functions and will either
-- default to the environment variable @EP4_PATH@, or the library's
-- current hardcoded path; if unable to set environment variables reliably,
-- use this function. Otherwise it's an unnecessary complication outside of
-- testing.
foreign import ccall unsafe "configurable_sweephe4.h ephe4_set_ephe_path"
  c_ephe4_set_ephe_path :: CString -> IO ()

-- | Obtain an array of doubles containing all requested planets, ecliptic,
-- nutation, and their speeds.   
foreign import ccall unsafe "configurable_sweephe4.h dephread2"
  c_dephread2 :: CDouble
              -- ^ @jd@ (julian date)
              -> PlanetListFlag
              -- ^ @plalist@ -- bitwise flag for the position data to include
              -> EpheCalcFlag
              -- ^ @flag@ -- bitwise flag for the additional options
              -> CString
              -- ^ @char* errtext@ -- at least 256 characters for error message
              -> IO (Ptr CDouble)
              -- ^ array of the calculated data
