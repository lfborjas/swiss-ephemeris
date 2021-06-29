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
-- 
-- @since 1.3.1.0

module SwissEphemeris.Precalculated (
  -- * Constituent types
  EphemerisPosition(..),
  Ephemeris(..),
  EphemerisBlockNumber,
  
  -- * Options 
  -- ** Options for additional computations
  EpheCalcOption(..),
  -- ** Options for which positions to include.
  PlanetListOption(..),
  -- * Convenience functions
  forPlanet,
  mkEphemerisBlockNumber,
  -- * Setup functions
  setEphe4Path,
  -- * High-level read functions
  readEphemeris,
  readEphemerisStrict,
  readEphemerisSimple,
  readEphemerisEasy,
  -- * Low-level read functions
  readEphemerisRaw,
  readEphemerisRawNoFallback,
  -- * Generating new ephemeris files
  writeEphemeris,
  -- * Low-level utils
  foldEpheCalcOptions,
  foldPlanetListOptions
)where

import Foreign ( peekArray, nullPtr, Bits((.|.)) )
import Foreign.C.String ( peekCAString, withCString )
import Foreign.SweEphe4
    ( includeAll,
      includeNutation,
      includeEcliptic,
      includeAllPlanets,
      mustUseStoredEphe,
      includeSpeed,
      c_dephread2,
      nutationIndex,
      eclipticIndex,
      numberOfFactors,
      c_ephe4_set_ephe_path,
      EpheConst(unEpheConst),
      EpheCalcFlag(..),
      PlanetListFlag(..),
      PlacalcPlanet(unPlacalcPlanet),
      pSun,
      pMoon,
      pMercury,
      pVenus,
      pMars,
      pJupiter,
      pSaturn,
      pUranus,
      pNeptune,
      pPluto,
      pMeanNode,
      pTrueNode,
      pChiron,
      pLilith, c_ephe4_write_file )
import SwissEphemeris.Internal
    ( JulianTime(unJulianTime),
      Planet(MeanApog, Sun, Moon, Mercury, Venus, Mars, Jupiter, Saturn,
             Uranus, Neptune, Pluto, MeanNode, TrueNode, Chiron),
      allocaErrorMessage )
import GHC.Generics (Generic)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.List (intersect)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

data EphemerisPosition a = EphemerisPosition
  { ephePlanet :: !Planet
  , epheLongitude :: !a
  , epheSpeed :: !a
  } deriving (Eq, Show, Generic)

data Ephemeris a = Ephemeris
  { epheDate :: !JulianTime 
  , epheEcliptic :: !a
  , epheNutation :: !a
  , ephePositions :: !(V.Vector (EphemerisPosition a))
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
  
newtype EphemerisBlockNumber = EphemerisBlockNumber Int 

-- | Construct a valid ephemeris block number. As per the
-- underlying library, all times between Julian day @-200000.0@
-- and @3000000.0@ are valid. Note that depending on which
-- ephemeris files you have, your effective range may be smaller.
mkEphemerisBlockNumber :: Int -> Maybe EphemerisBlockNumber
mkEphemerisBlockNumber n
  | n > -20 || n < 300 = Just . EphemerisBlockNumber $ n 
  | otherwise = Nothing

-- | For extreme data locality, we schlep around unboxed vectors.
-- Note that the higher level functions return regular 'Vector's,
-- to maintain some locality/performance, without awkward unboxing.
type EpheVector = VU.Vector Double

-- | Convenience "indexing" function to get a given 'Planet's
-- data from a given ephemeris.
forPlanet :: Planet -> Ephemeris a -> Maybe (EphemerisPosition a)
forPlanet pl ephe =
  case planetToPlanetOption pl of
    Nothing -> Nothing
    Just placalc -> ephePositions ephe V.!? (fromIntegral . unPlacalcPlanet $ placalc)

-- | Set path for base directory where @sep4_@ files are stored.
setEphe4Path :: FilePath -> IO ()
setEphe4Path path =
  withCString path $ \baseEphe4Path -> c_ephe4_set_ephe_path baseEphe4Path

-- | Given a function to convert a vector of doubles to a type for ephemeris,
-- a list of 'PlanetListOption', a list of 'EpheCalcOption' and a 'JulianTime',
-- try to get ephemeris data from a precalculated file on disk, or, if
-- allowed in the specified options, fall back to regular Swiss
-- Ephemeris calculations, using the current ephemeris mode.
-- 
-- The authors of Swiss Ephemeris encourage always requesting all
-- planets, ecliptic and nutation since they're stored in contiguous blocks anyway,
-- and the implementation calculates speeds /always/ so omitting speed
-- isn't worthwhile apart from data hygiene, or when choosing to allow
-- falling back to non-stored calculations.
--
-- Make sure you set the @EP4_PATH@ environment variable, or call
-- 'setEphe4Path' before calling this function, otherwise the
-- underlying library will try to locate the files in @ /home/ephe/ @.
readEphemeris
  :: (NonEmpty PlanetListOption
      -> NonEmpty EpheCalcOption 
      -> JulianTime
      -> EpheVector 
      -> a)
  -> NonEmpty PlanetListOption
  -> NonEmpty EpheCalcOption
  -> JulianTime
  -> IO (Either String a)
readEphemeris mkEphemeris planetOptions calcOptions time = do
  -- TODO(luis,) technically, we're /also/ able to exclude
  -- planets from calculations; don't currently have a use for that,
  -- but there should be a clean way to do that... or maybe we'll
  -- literally have to include all planets in 'PlanetListOption'.
  let plalist =
          foldPlanetListOptions
          . map planetListOptionToFlag
          . toList
          $ planetOptions
      flag =
          foldEpheCalcOptions
          . map epheOptionToFlag
          . toList
          $ calcOptions
  ephe <- readEphemerisRaw plalist flag time
  pure $ mkEphemeris planetOptions calcOptions time <$> ephe

-- | A version of 'readEphemeris' that wraps all longitudes, speeds,
-- ecliptic and nutation in 'Maybe's: if a given planet is configured
-- to be skipped, its longitude will be 'Nothing'; same for all speeds,
-- ecliptic and nutation if the respective options are set.
readEphemerisStrict :: NonEmpty PlanetListOption
  -> NonEmpty EpheCalcOption
  -> JulianTime
  -> IO (Either String (Ephemeris (Maybe Double)))
readEphemerisStrict = readEphemeris mkEphemerisStrict

-- | A version of 'readEphemeris' that always includes all planets and all
-- speeds, as well as ecliptic and nutation. 
readEphemerisSimple :: NonEmpty PlanetListOption
  -> NonEmpty EpheCalcOption
  -> JulianTime
  -> IO (Either String (Ephemeris Double))
readEphemerisSimple = readEphemeris mkEphemerisSimple

-- | A version of 'readEphemeris' that always gets all planets,
-- ecliptic and nutation, always includes speed, 
-- and allows falling back to non-stored
-- @swe_calc@ for dates/planets outside of the stored range (
-- via the @allowFallback@ 'Bool' parameter.)
--
-- I recommend using this one: as mentioned elsewhere, the underlying
-- library will /always/ calculate all ephemeris and all speeds, the only
-- material difference is that if it has to fall back to the underlying ephemeris,
-- it _will_ skip calculating any specified planets or speeds. I personally
-- use the "no fallback" version.
readEphemerisEasy :: Bool -> JulianTime -> IO (Either String (Ephemeris Double))
readEphemerisEasy allowFallback =
  readEphemerisSimple
    ( IncludeAll :| [])
    ( if allowFallback then withFallback else noFallback)
  where
    withFallback = IncludeSpeed :| []
    noFallback   = IncludeSpeed :| [MustUseStoredEphe]


-- | A 'law-abiding' ephemeris maker, producing the more complex
-- type with optional values, as determined by the underlying library's
-- flags. Implemented for thoroughness, but not as useful as it looks:
-- the underlying library will calculate everything even if we request
-- it not to, so turning off certain planets, speeds or nutation/ecliptic
-- is more of an exercise in pedantry than in optimization.
mkEphemerisStrict
  :: NonEmpty PlanetListOption
  -> NonEmpty EpheCalcOption
  -> JulianTime
  -> EpheVector
  -> Ephemeris (Maybe Double)
mkEphemerisStrict planetOptions calcOptions time results'=
  Ephemeris {
    epheDate = time,
    epheEcliptic = ecl `givenPlanetOptions` [IncludeEcliptic, IncludeAll],
    epheNutation = nut `givenPlanetOptions` [IncludeNutation, IncludeAll],
    ephePositions = ps
  }
  where
    givenPlanetOptions val opts =
      if hasPlanetOptions opts then val else Nothing
    hasPlanetOptions = not . null . intersect (toList planetOptions)
    givenCalcOptions val opts =
      if hasCalcOptions opts then Just val else Nothing
    hasCalcOptions   = not . null . intersect (toList calcOptions)

    singleFactors = unConst numberOfFactors
    results = V.convert results'
    (factors, speeds) = V.splitAt singleFactors results
    ecl = factors V.!? unConst eclipticIndex
    nut = factors V.!? unConst nutationIndex
    ps = V.zipWith3 mkEphePos factors speeds (V.fromList placalcOrdering)
    mkEphePos planetPos planetSpeed planet' =
      EphemerisPosition{
        ephePlanet = planet'
      -- TODO(luis) /technically/, we should only include the longitude
      -- if the planet was supposed to be included; but I always ask
      -- for all planets; it's really only worth it for cases where one
      -- wants to allow the fallback and thus limit /that/ calculation.
      , epheLongitude = Just planetPos
      , epheSpeed = planetSpeed `givenCalcOptions` [IncludeSpeed]
      }

mkEphemerisSimple
  :: NonEmpty PlanetListOption
  -> NonEmpty EpheCalcOption
  -> JulianTime
  -> EpheVector
  -> Ephemeris Double
mkEphemerisSimple _ _ time results'=
  Ephemeris {
    epheDate = time,
    epheEcliptic = ecl,
    epheNutation = nut,
    ephePositions = ps
  }
  where
    singleFactors = unConst numberOfFactors
    results = V.convert results'
    (factors, speeds) = V.splitAt singleFactors results
    -- ecl and nut are /always/ present, even when not
    -- requested; they just happen to be garbage if not requested.
    ecl = factors V.! unConst eclipticIndex
    nut = factors V.! unConst nutationIndex
    ps = V.zipWith3 mkEphePos factors speeds (V.fromList placalcOrdering)
    mkEphePos planetPos planetSpeed planet' =
      EphemerisPosition{
        ephePlanet = planet'
      -- TODO(luis) /technically/, we should only include the longitude
      -- if the planet was supposed to be included; but I always ask
      -- for all planets; it's really only worth it for cases where one
      -- wants to allow the fallback and thus limit /that/ calculation.
      , epheLongitude = planetPos
      , epheSpeed = planetSpeed
      }


-- | Lower-level version of 'readEphemeris':
-- 
-- * Expects options as bit flags set in a 'PlanetListFlag'; idem for 'EpheCalcFlag'
--   options. This is useful if one really wants to get into the setting bit flags
--   to select planets adventure.
-- * Returns a simple unboxed vector of 'Double's, where the first 'numberOfFactors'
--   elements are the planets, ecliptic and nutation; and the rest are speeds.
--   (the underlying library /always/ returns the full array, but if planets
--    ecliptic, nutation or ommitted, they won't be populated.)
-- * The underlying implementation uses a @static@ array, which means that
--   between invocations, quantities that are not calculated again linger (e.g.
--   you asked for all planets, all speeds, ecliptic and nutation in one pass,
--   and then only ask for certain planets, no speeds, no ecl/nut: 
--   /the previous values for these will be there!/) I've left it as-is here,
--   but made the behavior more predictable in 'readEphemeris'.
--
-- Due to the somewhat leaky/tricky nature of the underlying interface, this
-- function is provided merely for experimental usage -- you very likely want
-- 'readEphemeris'!
readEphemerisRaw
  :: PlanetListFlag
  -> EpheCalcFlag
  -> JulianTime
  -> IO (Either String EpheVector)
readEphemerisRaw plalist flag time =
  allocaErrorMessage $ \serr -> do
    ephe <-
      c_dephread2
        (realToFrac . unJulianTime $ time)
        plalist
        flag
        serr
    if ephe == nullPtr then do
      err <- peekCAString serr
      return $ Left err
    else do
      let noFactors = fromIntegral $ 2 * unEpheConst numberOfFactors
      -- the underlying library _always_ allocates `EP_NP` factors
      -- times two (to also include speeds.)
      factors <- peekArray noFactors ephe
      let vector = VU.fromList . map realToFrac $ factors
      pure $ Right vector

-- | For the most basic case: read ephemeris without falling back
-- to the non-stored variant, and always include speeds, all planets,
-- ecliptic and nutation.
readEphemerisRawNoFallback :: JulianTime -> IO (Either String EpheVector)
readEphemerisRawNoFallback =
  readEphemerisRaw calculateAll addSpeedNoFallback
  where
    -- an empty lists yields a flag with value @0@, which the underlying
    -- library considers equivalent to 'includeAll'.
    calculateAll = PlanetListFlag  0
    addSpeedNoFallback =
      foldEpheCalcOptions $ map epheOptionToFlag [IncludeSpeed, MustUseStoredEphe]

-- | Persist a 10,000 day block of ephemeris to disk, given a julian prefix.
-- 
-- This is a highly side-effectful function:
--
-- * It assumes you've set usable paths to both the @sep4@ files and
--   usable ephemeris. A @Left@ value will be returned otherwise.
-- * It is /not/ friendly to repeated invocations: if you happen to run
--   it in sequence with the same block number, undefined behavior may arise
--   where the file is still "open" as far as the underlying library is concerned,
--   because it keeps its own cursor, but the file descriptior will be closed. We
--   can fix that, but it introduces a divergence in the code that I'd rather just
--   warn about for the time being.
writeEphemeris :: EphemerisBlockNumber -> IO (Either String EphemerisBlockNumber)
writeEphemeris bn@(EphemerisBlockNumber n) = do
  allocaErrorMessage $ \serr -> do
    retval <-
      c_ephe4_write_file (fromIntegral n) serr
    
    if retval < 0 then do
      err <- peekCAString serr
      pure $ Left err
    else
      pure $ Right bn


unConst :: EpheConst -> Int
unConst = fromIntegral . unEpheConst

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
-- TODO:
-- We currently only allow the include* options, but the underlying
-- library _also_ allows setting bit flags for _each planet_ individually,
-- in fact, 'includeAllPlanets' is simply a value with all flags set!
-- I haven't thought of a clean way to expose that in the Haskell API,
-- but it should be doable!
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

planetToPlanetOption :: Planet -> Maybe PlacalcPlanet
planetToPlanetOption =
  \case
    Sun -> Just pSun
    Moon -> Just pMoon
    Mercury -> Just pMercury
    Venus -> Just pVenus
    Mars -> Just pMars
    Jupiter -> Just pJupiter
    Saturn -> Just pSaturn
    Uranus -> Just pUranus
    Neptune -> Just pNeptune
    Pluto -> Just pPluto
    MeanNode -> Just pMeanNode
    TrueNode -> Just pTrueNode
    Chiron -> Just pChiron
    MeanApog -> Just pLilith
    _ -> Nothing
-------------------------------------------------------------------------
