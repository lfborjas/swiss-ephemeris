
{-|
Module: SwissEphemeris
Description: Bindings to the swisseph C library.
License: GPL-2
Maintainer: swiss-ephemeris@lfborjas.com
Portability: POSIX

Exposes types and functions that mirror the rich functionality of <https://www.astro.com/swisseph/swephinfo_e.htm Swiss Ephemeris>.
Currently only certain bodies are exposed as data constructors, same for the major house systems. This is for the sake of simplicity
only, if you need more, please refer to the bundled header files in @csrc@.

You'll need to procure ephemeris files (see the official site, linked above) if you wish to obtain positions for planets outside of the main planetary
bodies in the solar system, or before 3000 B.C or after 3000 A.D. For example, the test suite uses a small ephemeris
that includes data for the asteroid Chiron, which is astrologically relevant in most modern practices.

Currently, only `calculateCoordinates` (to calculate the geocentric position of a given celestial body at a given Julian time,)
and `calculateCusps` (to calculate house cusps and relevant angles in various house systems/traditions) are provided; plus a
small `julianDay` function to translate between gregorian and julian times. There's a wealth of other calculations possible with
the underlying library, please refer to their documentation and the bundled sources for ideas!
-}

module SwissEphemeris (
    Planet(..)
,   HouseSystem(..)
,   JulianTime
,   HouseCusp
,   Coordinates
,   EclipticPosition(..)
,   EquatorialPosition(..)
,   ObliquityAndNutation(..)
,   Angles(..)
,   CuspsCalculation(..)
-- constructors
,   mkCoordinates
,   julianDay
-- management of data files
,   setEphemeridesPath
,   setNoEphemeridesPath
,   closeEphemerides
,   withEphemerides
,   withoutEphemerides
-- core calculations
,   calculateCoordinates
,   calculateEquatorialPosition
,   calculateObliquityAndNutation
,   calculateCusps
,   calculateCuspsLenient
,   calculateCuspsStrict
,   calculateHousePositionSimple
) where

import           Foreign.SwissEphemeris

import           Foreign
import           Foreign.C.String
import Control.Exception (bracket_)

import SwissEphemeris.Internal

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
  bracket_ (setEphemeridesPath ephemeridesPath)
           (closeEphemerides)

-- | Run a computation with no explicit ephemerides set, if the @SE_EPHE_PATH@
-- environment variable is set, that will be used. If not, it'll fall back to
-- in-memory data.
withoutEphemerides :: (IO a) -> IO a
withoutEphemerides =
  bracket_ (setNoEphemeridesPath)
           (closeEphemerides)

-- | Given year, month and day as `Int` and a time as `Double`, return
-- a single floating point number representing absolute `JulianTime`.
-- The input date is assumed to be in Gregorian time.
julianDay :: Int -> Int -> Int -> Double -> JulianTime
julianDay year month day hour = realToFrac $ c_swe_julday y m d h gregorian
  where
    y = fromIntegral year
    m = fromIntegral month
    d = fromIntegral day
    h = realToFrac hour

-- | Alias for `calculateEclipticPosition`, since it's the most common
-- position calculation.
calculateCoordinates :: JulianTime -> Planet -> IO (Either String Coordinates)
calculateCoordinates = calculateEclipticPosition

-- | Given `JulianTime` (see `julianDay`),
-- and a `Planet`, returns either the position of that planet at the given time,
-- if available in the ephemeris, or an error. The underlying library may do IO
-- when reading ephemerides data.
calculateEclipticPosition :: JulianTime -> Planet -> IO (Either String Coordinates)
calculateEclipticPosition time planet = do
  let options = (mkCalculationOptions defaultCalculationOptions)
  rawCoords <- calculateCoordinates' options time (planetNumber planet)
  return $ fmap coordinatesFromList rawCoords

-- | Obtain equatorial position (includes declination) of a planet.
-- If you've called `calculateCoordinates` in your code, this is a very cheap call, as the data
-- is already available to the C code.
calculateEquatorialPosition :: JulianTime -> Planet -> IO (Either String EquatorialPosition)
calculateEquatorialPosition time planet = do
  let options = (mkCalculationOptions $ defaultCalculationOptions ++ [equatorialPositions])
  rawCoords <- calculateCoordinates' options time (planetNumber planet)
  return $ fmap equatorialFromList rawCoords

-- | Given a time, calculate 
calculateObliquityAndNutation :: JulianTime -> IO (Either String ObliquityAndNutation)
calculateObliquityAndNutation time = do
  let options = CalcFlag 0
  rawCoords <- calculateCoordinates' options time specialEclNut
  return $ fmap obliquityNutationFromList rawCoords

-- | Call the internal function for calculations
calculateCoordinates' :: CalcFlag -> JulianTime -> PlanetNumber -> IO (Either String [Double])
calculateCoordinates' options time planet =
    allocaArray 6 $ \coords -> allocaArray 256 $ \serr -> do
        iflgret <- c_swe_calc_ut (realToFrac time)
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


-- | Alias for `calculateCuspsLenient`
calculateCusps :: HouseSystem -> JulianTime -> Coordinates -> IO CuspsCalculation
calculateCusps = calculateCuspsLenient

-- | Given a decimal representation of Julian Time (see `julianDay`),
-- a set of `Coordinates` (see `mkCoordinates`,) and a `HouseSystem`
-- (most applications use `Placidus`,) return a `CuspsCalculation` with all
-- house cusps in that system, and other relevant `Angles`. 
-- Notice that certain systems,
-- like `Placidus` and `Koch`, are very likely to fail close to the polar circles; in this
-- and other edge cases, the calculation returns cusps in the `Porphyrius` system.
-- The underlying library may do IO when consulting ephemerides data.
calculateCuspsLenient :: HouseSystem -> JulianTime -> Coordinates -> IO CuspsCalculation
calculateCuspsLenient sys time loc = 
    allocaArray 13 $ \cusps -> allocaArray 10 $ \ascmc -> do
        rval <- c_swe_houses (realToFrac time)
                             (realToFrac $ lat loc)
                             (realToFrac $ lng loc)
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
        (_:cuspsL)  <- peekArray 13 cusps
        anglesL     <- peekArray 10 ascmc
        return $ CuspsCalculation
                  (map realToFrac $ cuspsL) 
                  (anglesFromList $ map realToFrac $ anglesL)
                  (if rval < 0 then Porphyrius else sys)

-- | Unlike `calculateCuspsLenient`, return a `Left` value if the required house system
-- couldn't be used to perform the calculations.
calculateCuspsStrict :: HouseSystem -> JulianTime -> Coordinates -> IO (Either String CuspsCalculation)
calculateCuspsStrict sys time loc = do
  calcs@(CuspsCalculation _ _ sys') <- calculateCuspsLenient sys time loc
  if sys' /= sys then
    pure $ Left $ "Unable to calculate cusps in the requested house system (used " ++ (show sys') ++ " instead.)"
  else
    pure $ Right calcs

-- | Calculates the house position of a planet in a house in the given system.
-- requires the geographic coordinates and time of the birth/event, and the
-- ecliptic coordinates of the planet/body.
-- NOTE: for the Koch system, this is likely to fail, or return counterintuitive
-- results.
calculateHousePositionSimple :: HouseSystem -> JulianTime -> Coordinates -> EclipticPosition -> IO (Either String HousePosition)
calculateHousePositionSimple sys time loc pos = do
  obliquityAndNutation <- calculateObliquityAndNutation time
  case obliquityAndNutation of
    Left e -> return $ Left e
    Right on -> do
      siderealTime <- calculateSiderealTime time on
      let armc' = sidToArmc siderealTime
      calculateHousePosition sys armc' loc on pos

---
--- NOT EXPORTED (YET)
--- 

-- | If you happen to have the ARMC (obtained from calculateCusps, or by converting
-- sidereal time, which is in hours, to degrees,) and obliquity and nutation,
-- you can use this method to calculate a planet's house position.
-- Usually, what you have is just the time and place of the event, and positions of a planet,
-- in those cases, see `calculateHousePositionSimple`.
calculateHousePosition :: HouseSystem -> ARMC -> Coordinates -> ObliquityAndNutation -> EclipticPosition -> IO (Either String HousePosition)
calculateHousePosition sys armc' geoCoords obliq eclipticCoords =
  withArray [realToFrac $ lng eclipticCoords, realToFrac $ lat eclipticCoords] $ \xpin -> allocaArray 256 $ \serr -> do
    housePos <- c_swe_house_pos (realToFrac   $ unArmc armc')
                                (realToFrac   $ lat geoCoords)
                                (realToFrac   $ eclipticObliquity obliq)
                                (fromIntegral $ toHouseSystemFlag sys)
                                xpin
                                serr
    if housePos <= 0 then do
      msg <- peekCAString serr
      return $ Left msg
    else do
      let houseN = truncate housePos
          cuspD  = housePos - (fromIntegral houseN)
      return $ Right $ HousePosition houseN (realToFrac cuspD)

-- TODO: consider CDouble vs. realtoFrac, for loss of information issues.
calculateSiderealTimeSimple :: JulianTime -> IO SiderealTime
calculateSiderealTimeSimple jt = do
  sidTime <- c_swe_sidtime (realToFrac jt)
  return $ SiderealTime $ realToFrac sidTime

calculateSiderealTime :: JulianTime -> ObliquityAndNutation -> IO SiderealTime
calculateSiderealTime jt on = do
  let obliq = realToFrac $ eclipticObliquity on
      nut   = realToFrac $ nutationLongitude on
  sidTime <- c_swe_sidtime0 (realToFrac jt) obliq nut
  return $ SiderealTime $ realToFrac sidTime
