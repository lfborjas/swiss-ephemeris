{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module: SwissEphemeris.Precalculated
-- License: AGPL-3
-- Maintainer: swiss-ephemeris@lfborjas.com
-- Portability: POSIX
--
-- Functions for interacting with pre-calculated, file-persisted
-- ephemeris. You're responsible for providing a location for
-- @sep4_@ formatted files, as produced by the current version of
-- this library, in a compatible architecture. This can be done either
-- by setting the `EP4_PATH` environment variable to a valid directory path,
-- or via the 'setEph4Path' function.

module SwissEphemeris.Precalculated where

import Foreign
import Foreign.C.String
import Foreign.SweEphe4
import SwissEphemeris.Internal
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)

data EphemerisPosition = EphemerisPosition
  { ephePlanet :: !Planet
  , epheLongitude :: !Double
  , epheSpeed :: !Double
  } deriving (Eq, Show, Generic)

data Ephemeris = Ephemeris
  { epheEcliptic :: !(Maybe Double)
  , epheNutation :: !(Maybe Double)
  , ephePositions :: ![EphemerisPosition]
  } deriving (Eq, Show, Generic)

-- | Options for additional computations that are
-- allowed when reading a block of ephemeris.
data EpheCalcOption
  = IncludeSpeed
  | MustUseStoredEphe
  deriving (Eq, Show, Enum, Generic)

data PlanetListOption
  = IncludeAllPlanets
  | IncludeEcliptic
  | IncludeNutation
  | IncludeAll
  deriving (Eq, Show, Enum, Generic)

-- | Set path for base directory where @sep4_@ files are stored.
setEphe4Path :: FilePath -> IO ()
setEphe4Path path =
  withCString path $ \baseEphe4Path -> c_ephe4_set_ephe_path baseEphe4Path

-- | Get all ephemerides for a given set of planet and
-- calculation flags.
readEphemeris
  :: Maybe PlanetListFlag
  -> Maybe EpheCalcFlag
  -> JulianTime 
  -> IO (Either String [Double])
readEphemeris plalist flag time =
  allocaErrorMessage $ \serr -> do
    ephe <-
      c_dephread2
        (realToFrac . unJulianTime $ time)
        -- sending 0 is interpreted by the C fn as "everything"
        (fromMaybe (PlanetListFlag 0) plalist)
        -- sending 0 is interpreted by the C fn as "all opts"
        (fromMaybe (EpheCalcFlag 0) flag)
        serr
    if ephe == nullPtr then do
      err <- peekCAString serr
      return $ Left err
    else do
      let noFactors = fromIntegral $ 2 * unEpheConst numberOfFactors
      -- the underlying library _always_ allocates `EP_NP` factors
      -- times two (to also include speeds.)
      factors <- peekArray noFactors ephe
      pure $ Right $ map realToFrac factors

-- | For the most basic case: read ephemeris without falling back
-- to the non-stored variant, and always include speeds.
readEphemerisNoFallback :: JulianTime -> IO (Either String [Double])
readEphemerisNoFallback =
  readEphemeris Nothing (Just addSpeedNoFallback)
  where
    addSpeedNoFallback = 
      foldEpheCalcOptions $ map epheOptionToFlag [IncludeSpeed, MustUseStoredEphe]

-------------------------------------------------------------------------
-- UTILS FOR EPHE CALC OPTS
-------------------------------------------------------------------------

-- | Fold any given flags into one number with the combined flags set.
foldEpheCalcOptions :: [EpheCalcFlag] -> EpheCalcFlag
foldEpheCalcOptions = EpheCalcFlag . foldr ((.|.) . unEpheCalcFlag) 0

epheOptionToFlag :: EpheCalcOption -> EpheCalcFlag
epheOptionToFlag IncludeSpeed = includeSpeed
epheOptionToFlag MustUseStoredEphe = mustUseStoredEphe

defaultEpheCalcOptions :: [EpheCalcOption]
defaultEpheCalcOptions = [IncludeSpeed, MustUseStoredEphe]

optionsOrDefault :: [a] -> [a] -> [a]
optionsOrDefault defaults opts =
  if null opts then defaults else opts

withDefaultEpheCalcOptions :: [EpheCalcOption] -> [EpheCalcOption]
withDefaultEpheCalcOptions = optionsOrDefault defaultEpheCalcOptions


-------------------------------------------------------------------------
-- UTILS FOR PLANET OPTS
-------------------------------------------------------------------------

foldPlanetListOptions :: [PlanetListFlag] -> PlanetListFlag
foldPlanetListOptions = PlanetListFlag . foldr ((.|.) . unPlanetListFlag) 0

planetListOptionToFlag :: PlanetListOption -> PlanetListFlag
planetListOptionToFlag =
  \case
    IncludeAllPlanets -> includeAllPlanets
    IncludeEcliptic -> includeEcliptic
    IncludeNutation -> includeNutation
    IncludeAll -> includeAll

defaultPlanetListOptions :: [PlanetListOption]
defaultPlanetListOptions = [ IncludeAll ]

withDefaultPlanetListOptions :: [PlanetListOption] -> [PlanetListOption]
withDefaultPlanetListOptions = optionsOrDefault defaultPlanetListOptions

planetsAndOptionsToFlag :: [Planet] -> [PlanetListOption] -> PlanetListFlag
planetsAndOptionsToFlag ps opts =
  foldPlanetListOptions $ planets <> flags
  where
    planets = [planetsToListFlag ps]
    flags   = map planetListOptionToFlag opts

planetsToListFlag :: [Planet] -> PlanetListFlag
planetsToListFlag ps =
  PlanetListFlag $ foldl setBit zeroBits $ asOptions ps
  where
    asOptions :: [Planet] -> [Int]
    asOptions = map (fromIntegral . unPlacalcPlanet . planetToPlanetOption)

planetToPlanetOption :: Planet -> PlacalcPlanet
planetToPlanetOption =
  \case
    Sun -> pSun
    Moon -> pMoon
    Mercury -> pMercury
    Venus -> pVenus
    Mars -> pMars
    Jupiter -> pJupiter
    Saturn -> pSaturn
    Uranus -> pUranus
    Neptune -> pNeptune
    Pluto -> pPluto
    MeanNode -> pMeanNode
    TrueNode -> pTrueNode
    Chiron -> pChiron
    MeanApog -> pLilith
    -- TODO(luis) this isn't _fully_ correct, because the zeroth
    -- planet is the Sun; however, it's _monoidically_ innocuous;
    -- however, we've ommitted some interesting celestial bodies:
    -- Ceres, Pallas, Juno, Vesta, Heliocentric Earth and Pars Fortunae
    -- we should add them here when we add them to the regular ephemeris.
    _ -> PlacalcPlanet 0

-- | Given a `Planet`, determine how many planets would be in an
-- array of stored ephemeris
planetsCalculated :: Planet -> Int
planetsCalculated =
  (+ 1) . fromIntegral . unPlacalcPlanet . planetToPlanetOption

-------------------------------------------------------------------------
