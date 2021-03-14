-- |
-- Module: SwissEphemeris
-- Description: Bindings to the swisseph C library.
-- License: GPL-2
-- Maintainer: swiss-ephemeris@lfborjas.com
-- Portability: POSIX
--
-- Exposes types and functions that mirror the rich functionality of <https://www.astro.com/swisseph/swephinfo_e.htm Swiss Ephemeris>.
-- Currently only certain bodies are exposed as data constructors, same for the major house systems. This is for the sake of simplicity
-- only, if you need more, please refer to the bundled header files in @csrc@.
--
-- You'll need to procure ephemeris files (see the official site, linked above) if you wish to obtain positions for planets outside of the main planetary
-- bodies in the solar system, or before 3000 B.C or after 3000 A.D. For example, the test suite uses a small ephemeris
-- that includes data for the asteroid Chiron, which is astrologically relevant in most modern practices.
--
-- Currently, only a few select functions that are useful for western horoscopy are exported.
-- There's a wealth of other calculations possible with the underlying library, however,
-- please refer to their documentation and the bundled sources for ideas!
module SwissEphemeris
  ( -- fundamental aliases/newtypes
    JulianTime (..),
    SiderealTime (..),
    HouseCusp,
    -- fundamental enumerations
    SplitDegreesOption (..),
    Planet (..),
    HouseSystem (..),
    ZodiacSignName (..),
    NakshatraName (..),
    -- coordinate/position systems
    EclipticPosition (..),
    EquatorialPosition (..),
    GeographicPosition (..),
    HousePosition (..),
    -- information about the ecliptic at a point in time.
    ObliquityInformation (..),
    Angles (..),
    CuspsCalculation (..),
    LongitudeComponents (..),
    -- management of data files
    setEphemeridesPath,
    setNoEphemeridesPath,
    closeEphemerides,
    withEphemerides,
    withoutEphemerides,
    -- core calculations
    calculateEclipticPosition,
    calculateEquatorialPosition,
    calculateObliquity,
    calculateCusps,
    calculateCuspsLenient,
    calculateCuspsStrict,
    -- utility: coordinate transformation
    equatorialToEcliptic,
    eclipticToEquatorial,
    -- utilities for sidereal information
    calculateSiderealTime,
    calculateSiderealTimeSimple,
    calculateHousePosition,
    calculateHousePositionSimple,
    -- utilities for time calculations:
    julianDay,
    gregorianDateTime,
    deltaTime,
    -- utilities for angles:
    defaultSplitDegreesOptions,
    splitDegrees,
    splitDegreesZodiac,
  )
where

import Control.Exception (bracket_)
import Data.Semigroup ((<>))
import Foreign
import Foreign.C.String
import Foreign.SwissEphemeris
import SwissEphemeris.Internal
import System.IO.Unsafe (unsafePerformIO)

-- | Given a path to a directory, point the underlying ephemerides library to it.
-- You only need to call this function to provide an explicit ephemerides path,
-- if the environment variable @SE_EPHE_PATH@ is set, it overrides this function.
setEphemeridesPath :: FilePath -> IO ()
setEphemeridesPath path =
  withCString path $ \ephePath -> c_swe_set_ephe_path ephePath

-- | Explicitly state that we don't want to set an ephemeris path,
-- which will default to the built-in ephemeris, or use the directory
-- in the @SE_EPHE_PATH@ environment variable, if set.
setNoEphemeridesPath :: IO ()
setNoEphemeridesPath = c_swe_set_ephe_path nullPtr

-- | Explicitly release all "cache" pointers and open files obtained by the C
-- library.
closeEphemerides :: IO ()
closeEphemerides = c_swe_close

-- | Run a computation with a given ephemerides path open, and then close it.
-- Note that the computation does /not/ receive the ephemerides,
-- in keeping with the underlying library's side-effectful conventions.
withEphemerides :: FilePath -> (IO a) -> IO a
withEphemerides ephemeridesPath =
  bracket_
    (setEphemeridesPath ephemeridesPath)
    (closeEphemerides)

-- | Run a computation with no explicit ephemerides set, if the @SE_EPHE_PATH@
-- environment variable is set, that will be used. If not, it'll fall back to
-- in-memory data.
withoutEphemerides :: (IO a) -> IO a
withoutEphemerides =
  bracket_
    (setNoEphemeridesPath)
    (closeEphemerides)

-- | Given year, month and day as `Int` and a time as `Double`, return
-- a single floating point number representing absolute `JulianTime`.
-- The input date is assumed to be in Gregorian time.
julianDay :: Int -> Int -> Int -> Double -> JulianTime
julianDay year month day hour = JulianTime $ realToFrac $ c_swe_julday y m d h gregorian
  where
    y = fromIntegral year
    m = fromIntegral month
    d = fromIntegral day
    h = realToFrac hour

-- | Given a `JulianTime`, return a tuple with a year, month, day
-- and hour (as a `Double`.) It is the reverse of `julianDay`.
gregorianDateTime :: JulianTime -> (Int, Int, Int, Double)
gregorianDateTime (JulianTime jd) =
  unsafePerformIO $ do
    alloca $ \jyear -> alloca $ \jmon -> alloca $ \jday -> alloca $ \jut -> do
      _ <-
        c_swe_revjul
          (realToFrac jd)
          gregorian
          jyear
          jmon
          jday
          jut
      year <- peek jyear
      month <- peek jmon
      day <- peek jday
      time <- peek jut
      return $ (fromIntegral year, fromIntegral month, fromIntegral day, realToFrac time)

-- | Given `JulianTime` (see `julianDay`),
-- and a `Planet`, returns either the position of that planet at the given time,
-- if available in the ephemeris, or an error. The underlying library may do IO
-- when reading ephemerides data.
calculateEclipticPosition :: JulianTime -> Planet -> IO (Either String EclipticPosition)
calculateEclipticPosition time planet = do
  let options = (mkCalculationOptions defaultCalculationOptions)
  rawCoords <- calculateCoordinates' options time (planetNumber planet)
  return $ fmap coordinatesFromList rawCoords

-- | Obtain equatorial position (includes declination) of a planet.
-- If you've called `calculateEclipticPosition` in your code, this is a very cheap call, as the data
-- is already available to the C code.
calculateEquatorialPosition :: JulianTime -> Planet -> IO (Either String EquatorialPosition)
calculateEquatorialPosition time planet = do
  let options = (mkCalculationOptions $ defaultCalculationOptions ++ [equatorialPositions])
  rawCoords <- calculateCoordinates' options time (planetNumber planet)
  return $ fmap equatorialFromList rawCoords

-- | Given a time, calculate ecliptic obliquity and nutation
calculateObliquity :: JulianTime -> IO (Either String ObliquityInformation)
calculateObliquity time = do
  let options = CalcFlag 0
  rawCoords <- calculateCoordinates' options time specialEclNut
  return $ fmap obliquityNutationFromList rawCoords

-- | Internal function for calculations: the contract is too permissive, use one of the specialized
-- ones!
calculateCoordinates' :: CalcFlag -> JulianTime -> PlanetNumber -> IO (Either String [Double])
calculateCoordinates' options time planet =
  allocaArray 6 $ \coords -> allocaArray 256 $ \serr -> do
    iflgret <-
      c_swe_calc_ut
        (realToFrac . unJulianTime $ time)
        planet
        options
        coords
        serr

    if unCalcFlag iflgret < 0
      then do
        msg <- peekCAString serr
        return $ Left msg
      else do
        result <- peekArray 6 coords
        return $ Right $ map realToFrac result

-- | Convert from an ecliptic position to an equatorial position. Requires
-- knowledge of obliquity (see `calculateObliquity`.)
eclipticToEquatorial :: ObliquityInformation -> EclipticPosition -> EquatorialPosition
eclipticToEquatorial oAndN ecliptic =
  let obliquityLn = eclipticObliquity oAndN
      eclipticPos = eclipticToList ecliptic
      transformed = coordinateTransform' (negate obliquityLn) eclipticPos
   in equatorialFromList transformed

-- | Convert from an equatorial position to an ecliptic position. Requires
-- knowledge of obliquity (see `calculateObliquity`.)
equatorialToEcliptic :: ObliquityInformation -> EquatorialPosition -> EclipticPosition
equatorialToEcliptic oAndN equatorial =
  let obliquityLn = eclipticObliquity oAndN
      equatorialPos = equatorialToList equatorial
      transformed = coordinateTransform' obliquityLn equatorialPos
   in eclipticFromList transformed

-- | Internal function for coordinate transformation.
coordinateTransform' :: Double -> [Double] -> [Double]
coordinateTransform' obliquity ins =
  unsafePerformIO $ do
    withArray (map realToFrac $ take 6 ins) $ \xpo -> allocaArray 6 $ \xpn -> do
      _ <- c_swe_cotrans_sp xpo xpn (realToFrac obliquity)
      result <- peekArray 6 xpn
      return $ map realToFrac result

-- | Alias for `calculateCuspsLenient`
calculateCusps :: HouseSystem -> JulianTime -> GeographicPosition -> IO CuspsCalculation
calculateCusps = calculateCuspsLenient

-- | Given a decimal representation of Julian Time (see `julianDay`),
-- a `GeographicPosition` and a `HouseSystem`
-- (most applications use `Placidus`,) return a `CuspsCalculation` with all
-- house cusps in that system, and other relevant `Angles`.
-- Notice that certain systems,
-- like `Placidus` and `Koch`, are very likely to fail close to the polar circles; in this
-- and other edge cases, the calculation returns cusps in the `Porphyrius` system.
-- The underlying library may do IO when consulting ephemerides data.
calculateCuspsLenient :: HouseSystem -> JulianTime -> GeographicPosition -> IO CuspsCalculation
calculateCuspsLenient sys time loc =
  allocaArray 13 $ \cusps -> allocaArray 10 $ \ascmc -> do
    rval <-
      c_swe_houses
        (realToFrac . unJulianTime $ time)
        (realToFrac $ geoLat loc)
        (realToFrac $ geoLng loc)
        (fromIntegral $ toHouseSystemFlag sys)
        cusps
        ascmc
    -- NOTE: the underlying library returns 13 cusps for most systems,
    -- but the first element is always zero, to enable saying:
    -- cusps[1] -> first house.
    -- we treat it as a normal zero-indexed list.
    -- TODO: the Gauquelin system may return 37 doubles,
    -- we can try to support that, though do keep in mind that it may fall
    -- back to porphyrius near the poles, which ony has 13 doubles returned.
    (_ : cuspsL) <- peekArray 13 cusps
    anglesL <- peekArray 10 ascmc
    return $
      CuspsCalculation
        (map realToFrac $ cuspsL)
        (anglesFromList $ map realToFrac $ anglesL)
        (if rval < 0 then Porphyrius else sys)

-- | Unlike `calculateCuspsLenient`, return a `Left` value if the required house system
-- couldn't be used to perform the calculations.
calculateCuspsStrict :: HouseSystem -> JulianTime -> GeographicPosition -> IO (Either String CuspsCalculation)
calculateCuspsStrict sys time loc = do
  calcs@(CuspsCalculation _ _ sys') <- calculateCuspsLenient sys time loc
  if sys' /= sys
    then pure $ Left $ "Unable to calculate cusps in the requested house system (used " ++ (show sys') ++ " instead.)"
    else pure $ Right calcs

-- | Calculates the house position of a body in a house in the given system.
-- requires the geographic coordinates and time of the birth/event, and the
-- ecliptic coordinates of the planet/body. You only want this function if
-- you're working in the polar circle, or with objects that are way off the ecliptic;
-- for most objects in usual astrological charts, simply seeing which cusps
-- a planet falls between is sufficient, no need for this more complicated method.
-- see <https://groups.io/g/swisseph/message/4052>
-- NOTES: for the Koch system, this is likely to fail, or return counterintuitive
-- results. Also, we're doing a bit of a funky conversion between sidereal time and
-- ARMC, if you `calculateCusps`, the correct `armc` will be present in the returned `Angles`
calculateHousePositionSimple :: HouseSystem -> JulianTime -> GeographicPosition -> EclipticPosition -> IO (Either String HousePosition)
calculateHousePositionSimple sys time loc pos = do
  obliquityAndNutation <- calculateObliquity time
  case obliquityAndNutation of
    Left e -> return $ Left e
    Right on -> do
      SiderealTime siderealTime <- calculateSiderealTime time on
      let armc' = siderealTime * 15 + geoLng loc
      calculateHousePosition sys armc' loc on pos

-- | If you happen to have the correct ARMC for a time and place (obtained from calculateCusps)
-- and obliquity and nutation,
-- you can use this method to calculate a planet's house position.
-- Usually, what you have is just the time and place of the event, and positions of a planet,
-- in those cases, see `calculateHousePositionSimple`.
calculateHousePosition :: HouseSystem -> Double -> GeographicPosition -> ObliquityInformation -> EclipticPosition -> IO (Either String HousePosition)
calculateHousePosition sys armc' geoCoords obliq eclipticCoords =
  withArray [realToFrac $ lng eclipticCoords, realToFrac $ lat eclipticCoords] $ \xpin -> allocaArray 256 $ \serr -> do
    housePos <-
      c_swe_house_pos
        (realToFrac armc')
        (realToFrac $ geoLat geoCoords)
        (realToFrac $ eclipticObliquity obliq)
        (fromIntegral $ toHouseSystemFlag sys)
        xpin
        serr
    if housePos <= 0
      then do
        msg <- peekCAString serr
        return $ Left msg
      else do
        let houseN = truncate housePos
            cuspD = housePos - (fromIntegral houseN)
        return $ Right $ HousePosition houseN (realToFrac cuspD)

-- | Given `JulianTime`, get `SiderealTime`. May consult ephemerides data, hence it being in IO,
-- will have to calculate obliquity at the given julian time, so it'll be slightly slower than
-- `calculateSiderealTime`.
calculateSiderealTimeSimple :: JulianTime -> IO SiderealTime
calculateSiderealTimeSimple jt = do
  sidTime <- c_swe_sidtime (realToFrac . unJulianTime $ jt)
  return $ SiderealTime $ realToFrac sidTime

-- | Given a `JulianTime` and `ObliquityInformation`, calculate the equivalent `SiderealTime`.
-- prefer it over `calculateSiderealTimeSimple` if you already obtained `ObliquityInformation`
-- for another calculation.
calculateSiderealTime :: JulianTime -> ObliquityInformation -> IO SiderealTime
calculateSiderealTime jt on = do
  let obliq = realToFrac $ eclipticObliquity on
      nut = realToFrac $ nutationLongitude on
  sidTime <- c_swe_sidtime0 (realToFrac . unJulianTime $ jt) obliq nut
  return $ SiderealTime $ realToFrac sidTime

-- | Given a `JulianTime` (based on a UniversalTime), calculate the delta
-- between it and "true time":
-- See <https://www.astro.com/swisseph/swisseph.htm#_Toc46391727 7. Delta T>
-- It relies on ephemeris data being open, and as such belongs in IO.
-- /NOTE:/ this could be used to create a JulianTime -> EphemerisTime
-- function to send down to @swe_calc@, if we choose to port that one.
deltaTime :: JulianTime -> IO Double
deltaTime jt = do
  deltaT <- c_swe_deltat . realToFrac . unJulianTime $ jt
  return $ realToFrac deltaT

-- | Given a longitude, return the degrees it's from its nearest sign,
-- minutes, and seconds; with seconds rounded. Convenience alias for `splitDegrees`,
-- when wanting to display e.g. a table in a horoscope.
splitDegreesZodiac :: Double -> LongitudeComponents
splitDegreesZodiac = splitDegrees $ defaultSplitDegreesOptions <> [SplitZodiacal, RoundSeconds]

-- | Given a `Double` representing an ecliptic longitude, split it according to any
-- options from `SplitDegreesOption`:
-- if `SplitZodiacal` or `SplitNakshatra` are specified, they're returned
-- in `longitudeZodiacSign` and `longitudeNakshatra`, respectively.
-- If neither of those is specified, the raw `signum` is then populated, in
-- `longitudeSignum` (-1 for negative, 1, for positive.)
-- /NOTE:/ this function can also be used for latitudes, speeds or quantities
-- from other positional systems (like declinations,) but the zodiacal or
-- nakshatra components would of course be nonsensical.
splitDegrees :: [SplitDegreesOption] -> Double -> LongitudeComponents
splitDegrees options d =
  LongitudeComponents sign deg m s sf signum' nak
  where
    (z, deg, m, s, sf) = splitDegrees' flags d
    flags = foldSplitDegOptions $ map splitOptionToFlag options
    isZodiacSplit = SplitZodiacal `elem` options
    isNakshatraSplit = SplitNakshatra `elem` options
    sign = if isZodiacSplit then (Just . toEnum $ z) else Nothing
    nak = if isNakshatraSplit then (Just . toEnum $ z) else Nothing
    signum' = if (not isZodiacSplit && not isNakshatraSplit) then Just z else Nothing

-- | Internal implementation to split a given longitude into components.
splitDegrees' :: SplitDegFlag -> Double -> (Int, Integer, Integer, Integer, Double)
splitDegrees' options deg =
  unsafePerformIO $ do
    alloca $ \ideg -> alloca $ \imin -> alloca $ \isec -> alloca $ \dsecfr -> alloca $ \isign -> do
      -- initialize with 0, since it may never be touched.
      poke dsecfr 0
      _ <-
        c_swe_split_deg
          (realToFrac deg)
          options
          ideg
          imin
          isec
          dsecfr
          isign
      sign' <- peek isign
      deg' <- peek ideg
      min' <- peek imin
      sec' <- peek isec
      secfr <- peek dsecfr
      return $ ((fromIntegral sign'), (fromIntegral deg'), (fromIntegral min'), (fromIntegral sec'), (realToFrac secfr))
