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
import Data.List.NonEmpty (NonEmpty, toList, fromList)
import Data.List (intersect)

data EphemerisPosition = EphemerisPosition
  { ephePlanet :: !Planet
  , epheLongitude :: !Double
  , epheSpeed :: !(Maybe Double)
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


-- | Given a list of 'PlanetListOption', a list of
-- 'EpheCalcOption' and a 'JulianTime', try to get an
-- 'Ephemeris' from a precalculated file on disk, or, if
-- allowed in the specified options, fall back to regular Swiss
-- Ephemeris calculations, using the current ephemeris mode.
-- 
-- The authors of Swiss Ephemeris encourage always requesting all
-- planets, ecliptic and nutation since they're stored in contiguous blocks anyway,
-- and the implementation calculates speeds /always/ so omitting speed
-- isn't worthwhile apart from data hygiene.
--
-- Make sure you set the @EP4_PATH@ environment variable, or call
-- 'setEphe4Path' before calling this function, otherwise the
-- underlying library will try to locate the files in @ /home/ephe/ @.
readEphemeris  :: NonEmpty PlanetListOption
  -> NonEmpty EpheCalcOption
  -> JulianTime
  -> IO (Either String Ephemeris)
readEphemeris planetOptions calcOptions time = do
  -- TODO(luis,) technically, we're /also/ able to exclude
  -- planets from calculations; don't currently have a use for that,
  -- but there should be a clean way to do that... or maybe we'll
  -- literally have to include all planets in 'PlanetListOption'.
  let plalist =
        Just
          . foldPlanetListOptions
          . map planetListOptionToFlag
          . toList
          $ planetOptions
      flag =
        Just
          . foldEpheCalcOptions
          . map epheOptionToFlag
          . toList
          $ calcOptions
  ephe <- readEphemerisRaw plalist flag time
  pure $ mkEphemeris planetOptions calcOptions <$> ephe

mkEphemeris
  :: NonEmpty PlanetListOption
  -> NonEmpty EpheCalcOption
  -> [Double]
  -> Ephemeris
mkEphemeris planetOptions calcOptions results =
  Ephemeris {
    epheEcliptic = ecl `givenPlanetOptions` [IncludeEcliptic, IncludeAll],
    epheNutation = nut `givenPlanetOptions` [IncludeNutation, IncludeAll],
    ephePositions = ps
  }
  where
    givenPlanetOptions val opts =
      if hasPlanetOptions opts then Just val else Nothing
    hasPlanetOptions = not . null . intersect (toList planetOptions)
    givenCalcOptions val opts =
      if hasCalcOptions opts then Just val else Nothing
    hasCalcOptions   = not . null . intersect (toList calcOptions)

    unConst = fromIntegral . unEpheConst
    singleFactors = unConst numberOfFactors
    (factors, speeds) = splitAt singleFactors results
    ecl = factors !! unConst eclipticIndex
    nut = factors !! unConst nutationIndex
    ps = zipWith3 mkEphePos factors speeds placalcOrdering
    mkEphePos planetPos planetSpeed planet' =
      EphemerisPosition{
        ephePlanet = planet'
      -- TODO(luis) /technically/, we should only include the longitude
      -- if the planet was supposed to be included; but I always ask
      -- for all planets; it's really only worth it for cases where one
      -- wants to allow the fallback and thus limit /that/ calculation.
      , epheLongitude = planetPos
      , epheSpeed = planetSpeed `givenCalcOptions` [IncludeSpeed]
      }

-- | A version of 'readEphemeris' that always gets all planets,
-- ecliptic and nutation, always includes speed, 
-- and allows falling back to non-stored
-- @swe_calc@ for dates/planets outside of the stored range.
readEphemerisSimple :: JulianTime -> IO (Either String Ephemeris)
readEphemerisSimple = 
  readEphemeris 
    (fromList [IncludeAll]) 
    (fromList [IncludeSpeed])

-- | A version of 'readEphemerisSimple' that does /not/ allow fallback.
readEphemerisSimpleNoFallback :: JulianTime -> IO (Either String Ephemeris)
readEphemerisSimpleNoFallback = 
  readEphemeris 
    (fromList [IncludeAll])
    (fromList [IncludeSpeed, MustUseStoredEphe])

-- | Lower-level version of 'readEphemeris':
-- 
-- * Expects options as either Nothing (for the library's defaults,)
--   or bit flags set in a 'PlanetListFlag'; idem for 'EpheCalcFlag'
--   options
-- * Returns a simple list of 'Double's, where the first 'numberOfFactors'
--   elements are the planets, ecliptic and nutation; and the rest are speeds.
--   (the underlying library /always/ returns the full array, but if planets
--    ecliptic, nutation or ommitted, they won't be included.)
-- * The underlying implementation uses a @static@ array, which means that
--   between invocations, quantities that are not calculated again linger (e.g.
--   you asked for all planets, all speeds, ecliptic and nutation in one pass,
--   and then only ask for certain planets, no speeds, no ecl/nut: 
--   **the previous values for these will be there!**) I've left it as-is here,
--   but made the behavior more predictable in 'readEphemeris'.
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
epheOptionToFlag = 
  \case
    IncludeSpeed -> includeSpeed
    MustUseStoredEphe -> mustUseStoredEphe

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

-- | An order-equivalent version of the 'PlacalcPlanet' enum
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
