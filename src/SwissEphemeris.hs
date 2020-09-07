{-# LANGUAGE DeriveGeneric  #-}

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
,   Coordinates(..)
,   Angles(..)
,   CuspsCalculation(..)
-- constructors
,   defaultCoordinates
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
,   calculateCusps
,   calculateCuspsLenient
,   calculateCuspsStrict
) where

import           Foreign.SwissEphemeris

import           Foreign
import           GHC.Generics
import           Foreign.C.Types
import           Foreign.C.String
import           Data.Char (ord)
import Control.Exception (bracket_)

-- | All bodies for which a position can be calculated. Covers planets
-- in the solar system, points between the Earth and the Moon, and
-- astrologically significant asteroids (currently, only Chiron, but
-- ephemerides data is available for others.)
-- More at <https://www.astro.com/swisseph/swisseph.htm#_Toc46391648 2.1 Planetary and lunar ephemerides>
-- and <https://www.astro.com/swisseph/swephprg.htm#_Toc49847827 3.2 bodies>
data Planet = Sun
            | Moon
            | Mercury
            | Venus
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
            | Pluto
            | MeanNode
            | TrueNode
            | MeanApog
            | OscuApog
            | Earth
            | Chiron
            deriving (Show, Eq, Ord, Enum, Generic)

-- | The major house systems. The underlying library supports many more, including the
-- 36-cusp outlier Gauquelin.
-- More info at <https://www.astro.com/swisseph/swisseph.htm#_Toc46391705 6.2 Astrological house systems>
-- and <https://www.astro.com/swisseph/swephprg.htm#_Toc49847888 14. House cusp calculation>
data HouseSystem = Placidus
                 | Koch
                 | Porphyrius
                 | Regiomontanus
                 | Campanus
                 | Equal
                 | WholeSign
                 deriving (Show, Eq, Ord, Enum, Generic)

-- | Represents an instant in Julian time.
-- see:
-- <https://www.astro.com/swisseph/swephprg.htm#_Toc49847871 8. Date and time conversion functions>
-- also cf. `julianDay`
type JulianTime = Double

-- | The cusp of a given "house" or "sector"
-- see:
-- <https://www.astro.com/swisseph/swephprg.htm#_Toc49847888 14.1 House cusp calculation>
-- and <https://www.astro.com/swisseph/swisseph.htm#_Toc46391705 6.2 Astrological house systems>
type HouseCusp = Double

-- | Position data for a celestial body, includes rotational speeds.
-- see:
-- <https://www.astro.com/swisseph/swephprg.htm#_Toc49847837 3.4 Position and speed>
data Coordinates = Coordinates
  {
    lng :: Double
  , lat :: Double
  , distance :: Double
  , lngSpeed :: Double
  , latSpeed :: Double
  , distSpeed :: Double
  } deriving (Show, Eq, Generic)

-- | Default coordinates with all zeros -- when you don't care about/know the velocities,
-- which would be the case for most inputs (though most outputs /will/ include them.)
-- Usually you'll set only lat and lng (e.g. @defaultCoordinates{lat = 1.4, lng = 4.1}@)
-- when using it as an input for another function.
defaultCoordinates :: Coordinates
defaultCoordinates = Coordinates 0 0 0 0 0 0

-- | Constructor alias of `defaultCoordinates`, since it's used a lot in that role.
mkCoordinates :: Coordinates
mkCoordinates = defaultCoordinates

-- | Relevant angles: ascendant and MC, plus other "exotic" ones:
-- <https://www.astro.com/swisseph/swephprg.htm#_Toc49847890 14. House cusp calculation>
data Angles = Angles
  {
    ascendant :: Double
  , mc :: Double
  , armc :: Double
  , vertex :: Double
  , equatorialAscendant :: Double
  , coAscendantKoch :: Double
  , coAscendantMunkasey :: Double
  , polarAscendant :: Double
  } deriving (Show, Eq, Generic)

-- | Result of calculating the cusps for a given event; will include a list of
-- cusps (most systems use 12 cusps, Gauquelin uses 36.)
data CuspsCalculation = CuspsCalculation
  {
    houseCusps :: [HouseCusp]
  , angles :: Angles
  , systemUsed :: HouseSystem
  } deriving (Show, Eq, Generic)

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

-- | Given `JulianTime` (see `julianDay`),
-- and a `Planet`, returns either the position of that planet at the given time,
-- if available in the ephemeris, or an error. The underlying library may do IO
-- when reading ephemerides data.
calculateCoordinates :: JulianTime -> Planet -> IO (Either String Coordinates)
calculateCoordinates time planet =
    allocaArray 6 $ \coords -> allocaArray 256 $ \serr -> do
        iflgret <- c_swe_calc (realToFrac time)
                              (planetNumber planet)
                              speed
                              coords
                              serr

        if unCalcFlag iflgret < 0
            then do
                msg <- peekCAString serr
                return $ Left msg
            else do
                result <- peekArray 6 coords
                return $ Right $ coordinatesFromList $ map realToFrac result

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


-- Helpers

-- in the C lib, house systems are expected as ASCII
-- codes for specific characters (!)
-- documentation at: https://www.astro.com/swisseph/swephprg.htm#_Toc19111265
toHouseSystemFlag :: HouseSystem -> Int
toHouseSystemFlag Placidus      = ord 'P'
toHouseSystemFlag Koch          = ord 'K'
toHouseSystemFlag Porphyrius    = ord 'O'
toHouseSystemFlag Regiomontanus = ord 'R'
toHouseSystemFlag Campanus      = ord 'C'
toHouseSystemFlag Equal         = ord 'A'
toHouseSystemFlag WholeSign     = ord 'W'

coordinatesFromList :: [Double] -> Coordinates
-- N.B. note that for some reason the SWE guys really like lng,lat coordinates
-- though only for this one function: https://www.astro.com/swisseph/swephprg.htm#_Toc19111235
coordinatesFromList (sLng : sLat : c : d : e : f : _) = Coordinates sLng sLat c d e f
-- the underlying library goes to great lengths to not return fewer than 6 data,
-- it instead uses zeroes for unavailable entries.
coordinatesFromList _                           = Coordinates 0 0 0 0 0 0

anglesFromList :: [Double] -> Angles
anglesFromList (a : _mc : _armc : vtx : ea : cak : cam : pa : _ : _) =
    Angles a _mc _armc vtx ea cak cam pa
-- the underlying library always returns _something_, defaulting to zero
-- if the angle calculation doesn't apply.
anglesFromList _ = Angles 0 0 0 0 0 0 0 0

planetNumber :: Planet -> PlanetNumber
planetNumber p = PlanetNumber $ CInt y
  where
    y = fromIntegral $ fromEnum p :: Int32
