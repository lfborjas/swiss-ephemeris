{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module: SwissEphemeris.Precalculated
-- License: AGPL-3
-- Maintainer: swiss-ephemeris@lfborjas.com
-- Portability: POSIX
--
--
-- Functions for interacting with pre-calculated, file-persisted
-- ephemeris. You're responsible for providing a location for
-- @sep4_@ formatted files, as produced by the current version of
-- this library, in a compatible architecture. This can be done either
-- by setting the `EP4_PATH` environment variable to a valid directory path,
-- or via the 'setEph4Path' function.
--
-- @since 1.4.0.0
module SwissEphemeris.Precalculated
  ( -- * About Precalculated Ephemeris
    -- $precalc
    EphemerisPosition (..),
    Ephemeris (..),
    EphemerisBlockNumber,
    EpheVector,

    -- * Options

    -- ** Options for additional computations
    EpheCalcOption (..),

    -- ** Options for which positions to include.
    PlanetListOption (..),

    -- * Convenience functions
    forPlanet,
    planetEphe,
    mkEphemerisBlockNumber,
    extractEphemerisBlockNumber,

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
    foldPlanetListOptions,
  )
where

import Data.List (intersect)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Foreign (Bits ((.|.)), nullPtr, peekArray)
import Foreign.C.String (peekCAString, withCString)
import Foreign.SweEphe4
  ( EpheCalcFlag (..),
    EpheConst (unEpheConst),
    PlacalcPlanet (unPlacalcPlanet),
    PlanetListFlag (..),
    c_dephread2,
    c_ephe4_set_ephe_path,
    c_ephe4_write_file,
    eclipticIndex,
    includeAll,
    includeAllPlanets,
    includeEcliptic,
    includeNutation,
    includeSpeed,
    mustUseStoredEphe,
    numberOfFactors,
    nutationIndex,
    pChiron,
    pJupiter,
    pLilith,
    pMars,
    pMeanNode,
    pMercury,
    pMoon,
    pNeptune,
    pPluto,
    pSaturn,
    pSun,
    pTrueNode,
    pUranus,
    pVenus,
  )
import GHC.Generics (Generic)
import SwissEphemeris.Internal
  (
    Planet
      ( Chiron,
        Jupiter,
        Mars,
        MeanApog,
        MeanNode,
        Mercury,
        Moon,
        Neptune,
        Pluto,
        Saturn,
        Sun,
        TrueNode,
        Uranus,
        Venus
      ),
    allocaErrorMessage, HasEclipticLongitude(..)
  )
import SwissEphemeris.Time

{- $precalc
   Pre-calculated Ephemeris are disk-persisted binary blocks of 10,000 days
   of positions each. They store planetary data in a space-efficient format,
   to the slight detriment of precision. Positions for all planets in the sol
   system (including Pluto, but excluding the heliocentric position of the Earth,)
   as well as the mean lunar apogee ("Black Moon Lilith",) the Mean and True [North]
   lunar nodes, and the asteroid Chiron as well as Ecliptic and Nutation for the day
   are stored in contiguous blocks.

   Even though only midnight data
   is actually stored (e.g. @2440000.5@,) the underlying library will
   use a fast interpolation method to approximate any julian date requested of it,
   it also uses that interpolation method to approximate the speeds of planets,
   which are /not/ stored.
   
   The file format is designed for fast sequential access: a file pointer and
   cursor survive invocations, and a buffer of 20 days of positions is maintained
   in-memory, so requesting all days in a year will benefit vastly from memory
   residency and outpace random access or actual ephemeris calculations. Some
   care has been put into thread-safety, but the original implementation was /not/
   thread safe, so even though I've run a lot of @valgrind@ after adding some
   thread-local guardrails, here may be dragons if you do heavy threaded access
   or heavy non-sequential querying. The regular 'SwissEphemeris' is battle-tested
   for safety and speed, so use that for most use cases that don't need to quickly
   examine large intervals of contiguous time quickly, which is the narrow province
   of this module.
   
   Deferring to the original authors' for more details:

   == /Note from @sweephe4.h@/
   
   The design of ephemeris type ep4:
   In all ASYS and transit application of stored ephemerides
   except Progressed Declinations Type 56 we need only the
   longitudes of the planets or nodes.
   The old EP3 format contains also latitudes, and uses ephemeris time.
   Therefore we define a new ephemeris format, which is capable of
   replacing EP3, when some ASYS programs are changed.
   The ASYS programs requiring different data can receive them
   by asking the calcserv module.
   
   We therefore store now a daily ephemeris with only logitudes, ecl and nut.
   The ephemeris is computed and stored for midnight ephemeris time, i.e.
   for @jd = 2400000.5, 2400001.5@ etc.
   In the ephemeris record for this date, only floor(jd) is kept.
   
   In many cases universal time (UT) is desired, not ephemeris time.
   Because computation with our current computers is very cheap for
   everything except trigonometrci functions, we can afford to
   build a good interpolation into the ephemeris package.
   
   The user can request from ephread() ephemeris positions for
   any (double) jd, not only for midnight ephemeris time.
   Inside the package the same quick Everett 5th-order interpolator
   is used as in placalc.
   It delivers positions within 0.01" for all planets except moon, mercury
   and true node. Moon and Mercury suffer, because their positions are
   stored only with a precision of 0.1"; true node suffers because
   it oscillates quickly with the fastest moon terms.
   The maximum observed differences between placalc and ephread for 12.00 noon
   are 0.25" for moon and true node and 0.1" for Mercury; in 80% of the days
   the differences are less than 0.1". This is significantly better than
   the implemented precision of the placalc package itself.
   
   The Everett interpolator delivers without any additional cost also
   the speed of the planets. This speed is very much better than the
   speed derived for the inner planets from the mean orbits.
   
   The returned data from ephread are in an array of centisec,
   with ecl and nut behind the planets.
   The default, @pflag = 0@, returns all.
   The speeds are returned in the second half of the array;
   the speed is always there, even when the speed bit has not been set.
-}

-- | The ecliptic longitude data of a given 'Planet' 
data EphemerisPosition a = EphemerisPosition
  { ephePlanet :: !Planet,
    -- ^ the 'Planet' this position corresponds to
    epheLongitude :: !a,
    -- ^ longitude in @a@ units (by default, degrees)
    -- you can use your own @mkEphemeris@-style function;
    -- two are provided here, one to produce 'Double',
    -- another to produce 'Maybe Double'.
    epheSpeed :: !a
    -- ^ ecliptic speed in @a@ units.
  }
  deriving (Eq, Show, Generic)

instance (Real a, Eq a, Fractional a) => HasEclipticLongitude (EphemerisPosition a) where
  getEclipticLongitude = realToFrac . epheLongitude
  setEclipticLongitude p l' = p{epheLongitude = realToFrac l'}

-- | The positions of all planets for a given time,
-- plus ecliptic and nutation.
data Ephemeris a = Ephemeris
  { epheDate :: !JulianDayTT,
    -- ^ julian time for this ephemeris
    epheEcliptic :: !a,
    epheNutation :: !a,
    ephePositions :: !(V.Vector (EphemerisPosition a))
  }
  deriving (Eq, Show, Generic)

-- | Options for additional computations that are
-- allowed when reading a block of ephemeris.
data EpheCalcOption
  = IncludeSpeed
  | MustUseStoredEphe
  deriving (Eq, Show, Enum, Generic)

-- | Whether to include all planets, ecliptic, nutation,
-- or all of the above. The underlying library also allows
-- for bit flags to disable selecting some planets; I haven't
-- though of a way to model that ergonomically.
data PlanetListOption
  = IncludeAllPlanets
  | IncludeEcliptic
  | IncludeNutation
  | IncludeAll
  deriving (Eq, Show, Enum, Generic)

-- | Up to three-digit numbers assigned to each
-- 10,000 day block of ephemeris data; any given
-- number will be padded with zeroes internally
-- to make up a Julian date, at midnight.
-- e.g. @EphemerisBlocknumber 244@
-- corresponds to @jd = 2440000.5@, i.e.
-- @1968-May-23 12:00:00 UTC@
newtype EphemerisBlockNumber
  = EphemerisBlockNumber Int
  deriving (Eq, Show, Ord)

-- | The 'Bounded' instance for 'EphemerisBlockNumber' comes from
-- the underlying library's older limits; as reported in the manual:
-- [section 2.1.1 three ephemerides](https://www.astro.com/swisseph/swisseph.htm#_Toc58931065)
-- As of the time of writing, the range was from
-- @JD -3026604.5@ to @JD 7857139.5@ 
-- (i.e. 11 Aug 13000 BCE (-12999) Jul. to 7 Jan 16800 CE Greg.) 
-- However, the underlying C code expects to work in a much smaller range:
-- from @JD -200000.0@ to @JD 3000000.0@, which is "merely"
-- from @6 Jun 5261 BCE@ to @15 Aug 3501 CE@; indicating that pre-calculated
-- ephemeris are more suited for transits/astrology than more serious astronomical
-- studies. I haven't dug into /why/ that limit has been kept other than
-- the fact that it's old code, so it surely is possible to extend it.
instance Bounded EphemerisBlockNumber where
  minBound = EphemerisBlockNumber $ -20
  maxBound = EphemerisBlockNumber 300
  
-- | Implements a lawful 'Bounded' 'Enum'
instance Enum EphemerisBlockNumber where
  succ en@(EphemerisBlockNumber n) = 
    if en == maxBound then 
      error "max bound reached"
    else
      EphemerisBlockNumber (succ n)
  pred en@(EphemerisBlockNumber n) = 
    if en == minBound  then
      error "min bound reached"
    else
      EphemerisBlockNumber (pred n)
  toEnum = EphemerisBlockNumber
  fromEnum (EphemerisBlockNumber n) = n
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y maxBound
  enumFromTo (EphemerisBlockNumber n) (EphemerisBlockNumber m) =
    fmap EphemerisBlockNumber (enumFromTo n m)
  enumFromThenTo (EphemerisBlockNumber n) (EphemerisBlockNumber n') (EphemerisBlockNumber m) =
    fmap EphemerisBlockNumber (enumFromThenTo n n' m)
  

-- | Construct a valid ephemeris block number. As per the
-- underlying library, all times between Julian day @-200000.0@
-- and @3000000.0@ are valid. Note that depending on which
-- ephemeris files you have, your effective range may be smaller,
-- or bigger. This is provided as a common denominator, but to get
-- the /real/ range of your ephemeris, check out the @swe_get_current_file_data@
-- function. 
-- (cf. [section 2.6 of the manual](https://www.astro.com/swisseph/swephprg.htm#_Toc71121146))
mkEphemerisBlockNumber :: Int -> Maybe EphemerisBlockNumber
mkEphemerisBlockNumber n
  | n > extractEphemerisBlockNumber minBound && n < extractEphemerisBlockNumber maxBound = Just . EphemerisBlockNumber $ n
  | otherwise = Nothing

-- | Get the 'Int' inside an 'EphemerisBlockNumber'
extractEphemerisBlockNumber :: EphemerisBlockNumber -> Int
extractEphemerisBlockNumber (EphemerisBlockNumber n) = n

-- | For extreme data locality, we schlep around unboxed vectors.
-- Note that the higher level functions return regular 'Vector's,
-- to maintain some locality/performance, without awkward unboxing.
type EpheVector = VU.Vector Double

-- | Convenience "indexing" function to get a given 'Planet's
-- data from a given ephemeris.
forPlanet :: Ephemeris a -> Planet -> Maybe (EphemerisPosition a)
forPlanet ephe pl =
  case planetToPlanetOption pl of
    Nothing -> Nothing
    Just placalc -> ephePositions ephe V.!? (fromIntegral . unPlacalcPlanet $ placalc)
    
-- | Flipped version of `forPlanet`
planetEphe :: Planet -> Ephemeris a -> Maybe (EphemerisPosition a)
planetEphe = flip forPlanet

-- | Set path for base directory where @sep4_@ files are stored.
-- 
-- __WARNING__: this is provided for convenience, but in a multi-threaded
-- situation, it is relatively likely that a call to this function will
-- either be optimized away, or interleaved too late. Please consider
-- setting the @EP4_PATH@ environment variable instead: it will always
-- be found by the C code, vs. the /sometimes/ of Haskell's inscrutable
-- optimizations.
setEphe4Path :: FilePath -> IO ()
setEphe4Path path =
  withCString path $ \baseEphe4Path -> c_ephe4_set_ephe_path baseEphe4Path

-- | Given a function to convert a vector of doubles to a type for ephemeris,
-- a list of 'PlanetListOption', a list of 'EpheCalcOption' and a 'JulianDay',
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
-- underlying library will try to locate the files in @/home/ephe/@.
readEphemeris ::
  ( NonEmpty PlanetListOption ->
    NonEmpty EpheCalcOption ->
    JulianDayTT  ->
    EpheVector ->
    a
  ) ->
  NonEmpty PlanetListOption ->
  NonEmpty EpheCalcOption ->
  JulianDayTT  ->
  IO (Either String a)
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
readEphemerisStrict ::
  NonEmpty PlanetListOption ->
  NonEmpty EpheCalcOption ->
  JulianDayTT  ->
  IO (Either String (Ephemeris (Maybe Double)))
readEphemerisStrict = readEphemeris mkEphemerisStrict

-- | A version of 'readEphemeris' that always includes all planets and all
-- speeds, as well as ecliptic and nutation.
readEphemerisSimple ::
  NonEmpty PlanetListOption ->
  NonEmpty EpheCalcOption ->
  JulianDayTT  ->
  IO (Either String (Ephemeris Double))
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
readEphemerisEasy :: Bool -> JulianDayTT  -> IO (Either String (Ephemeris Double))
readEphemerisEasy allowFallback =
  readEphemerisSimple
    (IncludeAll :| [])
    (if allowFallback then withFallback else noFallback)
  where
    withFallback = IncludeSpeed :| []
    noFallback = IncludeSpeed :| [MustUseStoredEphe]

-- | A 'law-abiding' ephemeris maker, producing the more complex
-- type with optional values, as determined by the underlying library's
-- flags. Implemented for thoroughness, but not as useful as it looks:
-- the underlying library will calculate everything even if we request
-- it not to, so turning off certain planets, speeds or nutation/ecliptic
-- is more of an exercise in pedantry than in optimization.
mkEphemerisStrict ::
  NonEmpty PlanetListOption ->
  NonEmpty EpheCalcOption ->
  JulianDayTT  ->
  EpheVector ->
  Ephemeris (Maybe Double)
mkEphemerisStrict planetOptions calcOptions time results' =
  Ephemeris
    { epheDate = time,
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
    hasCalcOptions = not . null . intersect (toList calcOptions)

    singleFactors = unConst numberOfFactors
    results = V.convert results'
    (factors, speeds) = V.splitAt singleFactors results
    ecl = factors V.!? unConst eclipticIndex
    nut = factors V.!? unConst nutationIndex
    ps = V.zipWith3 mkEphePos factors speeds (V.fromList placalcOrdering)
    mkEphePos planetPos planetSpeed planet' =
      EphemerisPosition
        { ephePlanet = planet',
          -- TODO(luis) /technically/, we should only include the longitude
          -- if the planet was supposed to be included; but I always ask
          -- for all planets; it's really only worth it for cases where one
          -- wants to allow the fallback and thus limit /that/ calculation.
          epheLongitude = Just planetPos,
          epheSpeed = planetSpeed `givenCalcOptions` [IncludeSpeed]
        }

mkEphemerisSimple ::
  NonEmpty PlanetListOption ->
  NonEmpty EpheCalcOption ->
  JulianDayTT  ->
  EpheVector ->
  Ephemeris Double
mkEphemerisSimple _ _ time results' =
  Ephemeris
    { epheDate = time,
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
      EphemerisPosition
        { ephePlanet = planet',
          -- TODO(luis) /technically/, we should only include the longitude
          -- if the planet was supposed to be included; but I always ask
          -- for all planets; it's really only worth it for cases where one
          -- wants to allow the fallback and thus limit /that/ calculation.
          epheLongitude = planetPos,
          epheSpeed = planetSpeed
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
readEphemerisRaw ::
  PlanetListFlag ->
  EpheCalcFlag ->
  JulianDayTT  ->
  IO (Either String EpheVector)
readEphemerisRaw plalist flag timeTT =
  allocaErrorMessage $ \serr -> do
    ephe <-
      c_dephread2
        (realToFrac . getJulianDay $ timeTT)
        plalist
        flag
        serr
    if ephe == nullPtr
      then do
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
readEphemerisRawNoFallback :: JulianDayTT  -> IO (Either String EpheVector)
readEphemerisRawNoFallback =
  readEphemerisRaw calculateAll addSpeedNoFallback
  where
    -- an empty lists yields a flag with value @0@, which the underlying
    -- library considers equivalent to 'includeAll'.
    calculateAll = PlanetListFlag 0
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

    if retval < 0
      then do
        err <- peekCAString serr
        pure $ Left err
      else pure $ Right bn

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

-- | Fold any given planet flags into one datum with the combined flags set.
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
placalcOrdering =
  [ Sun,
    Moon,
    Mercury,
    Venus,
    Mars,
    Jupiter,
    Saturn,
    Uranus,
    Neptune,
    Pluto,
    MeanNode,
    TrueNode,
    Chiron,
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
