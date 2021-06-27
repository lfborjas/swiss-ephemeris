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

module SwissEphemeris.Precalculated (
  EphemerisPosition(..),
  Ephemeris(..),
  EpheCalcOption(..),
  PlanetListOption(..),
  setEphe4Path,
  readEphemeris,
  readEphemerisSimple,
  readEphemerisSimpleNoFallback,
  readEphemerisRaw,
  readEphemerisRawNoFallback'
)where

import Foreign
import Foreign.C.String
import Foreign.SweEphe4
import SwissEphemeris.Internal
    ( JulianTime(unJulianTime),
      Planet(MeanApog, Sun, Moon, Mercury, Venus, Mars, Jupiter, Saturn,
             Uranus, Neptune, Pluto, MeanNode, TrueNode, Chiron),
      allocaErrorMessage )
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)

data EphemerisPosition = EphemerisPosition
  { ephePlanet :: !Planet
  , epheLongitude :: !Double
  , epheSpeed :: !Double
  } deriving (Eq, Show, Generic)

data Ephemeris = Ephemeris
  { epheEcliptic :: !Double
  , epheNutation :: !Double
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


-- | Given a list of 'PlanetListOption', a list of
-- 'EpheCalcOption' and a 'JulianTime', try to get an
-- 'Ephemeris' from a precalculated file on disk.
-- 
-- Note that sending empty lists of options defaults to:
-- 
-- * Getting positions /and/ speeds for all planets
-- * Including ecliptic and nutation
-- * Allowing falling back to non-stored ephemeris calculations
--   if out of range.
-- 
-- The authors of Swiss Ephemeris encourage always requesting all
-- planets, since they're stored in contiguous blocks anyway, and
-- maintain that including speed adds a negligible overhead that's
-- seldom worth omitting. 
--
-- Make sure you set the @EP4_PATH@ environment variable, or call
-- 'setEphe4Path' before calling this function, otherwise the
-- underlying library will try to locate the files in @ /home/ephe/ @.
readEphemeris  :: [PlanetListOption]
  -> [EpheCalcOption]
  -> JulianTime
  -> IO (Either String Ephemeris)
readEphemeris planetOptions calcOptions time = do
  let plalist =
        if null planetOptions
          then Nothing
          else Just
           . foldPlanetListOptions
           . map planetListOptionToFlag
           $ planetOptions
      flag =
        if null calcOptions
          then Nothing
          else Just
            . foldEpheCalcOptions
            . map epheOptionToFlag
            $ calcOptions
  ephe <- readEphemerisRaw plalist flag time
  pure $ mkEphemeris <$> ephe

mkEphemeris :: [Double] -> Ephemeris
mkEphemeris results =
  Ephemeris { 
    epheEcliptic = ecl, 
    epheNutation = nut, 
    ephePositions = ps
  }
  where
    unConst = fromIntegral . unEpheConst
    singleFactors = unConst numberOfFactors
    (factors, speeds) = splitAt singleFactors results
    ecl = factors !! unConst eclipticIndex
    nut = factors !! unConst nutationIndex 
    ps = zipWith3 mkEphePos factors speeds placalcOrdering
    mkEphePos planetPos planetSpeed planet' =
      EphemerisPosition{
        ephePlanet = planet'
      , epheLongitude = planetPos
      , epheSpeed = planetSpeed
      }

-- | A version of 'readEphemeris' that always gets all planets,
-- always includes speed, and allows falling back to non-stored
-- @swe_calc@ for dates/planets outside of the stored range.
readEphemerisSimple :: JulianTime -> IO (Either String Ephemeris)
readEphemerisSimple = readEphemeris [] []

-- | A version of 'readEphemerisSimple' that does /not/ allow fallback.
readEphemerisSimpleNoFallback :: JulianTime -> IO (Either String Ephemeris)
readEphemerisSimpleNoFallback = readEphemeris [] [IncludeSpeed, MustUseStoredEphe]

-- | Lower-level version of 'readEphemeris':
-- 
-- * Expects options as either Nothing (for the library's defaults,)
--   or bit flags set in a 'PlanetListFlag'; idem for 'EpheCalcFlag'
--   options
-- * Returns a simple list of 'Double's, where the first 'numberOfFactors'
--   elements are the planets, ecliptic and nutation; and the rest are speeds.
--   (the underlying library /always/ returns the full array, but if planets
--    ecliptic, nutation or ommitted, they won't be included.)
--
-- Due to the somewhat leaky/tricky nature of the underlying interface, this
-- function is provided merely for experimental usage -- you very likely want
-- 'readEphemeris' -- unless you don't like the record types it returns!
readEphemerisRaw 
  :: Maybe PlanetListFlag
  -> Maybe EpheCalcFlag
  -> JulianTime
  -> IO (Either String [Double])
readEphemerisRaw plalist flag time =
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
readEphemerisRawNoFallback' :: JulianTime -> IO (Either String [Double])
readEphemerisRawNoFallback' =
  readEphemerisRaw Nothing (Just addSpeedNoFallback)
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

-- | A list-version of the 'planetToPlanetOption' mapping
placalcOrdering :: [Planet]
placalcOrdering = [
    Sun ,
    Moon ,
    Mercury ,
    Venus ,
    Mars ,
    Jupiter ,
    Saturn ,
    Uranus ,
    Neptune ,
    Pluto ,
    MeanNode ,
    TrueNode ,
    Chiron ,
    MeanApog
  ]
-------------------------------------------------------------------------
