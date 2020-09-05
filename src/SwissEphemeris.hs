{-# LANGUAGE DeriveGeneric     #-}

module SwissEphemeris (
    Planet(..)
,   HouseSystem(..)
,   JulianTime
,   Coordinates(..)
,   HouseCusps(..)
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
)where

import           Foreign.SwissEphemeris

import           Foreign
import           GHC.Generics
import           Foreign.C.Types
import           Foreign.C.String
import           Data.Char                      ( ord )
import Control.Exception (bracket_)
import Debug.Trace (trace)

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

data HouseSystem = Placidus
                 | Koch
                 | Porphyrius
                 | Regiomontanus
                 | Campanus
                 | Equal
                 | WholeSign
                 deriving (Show, Eq, Ord, Enum, Generic)

type JulianTime = Double

data Coordinates = Coordinates
  {
    lng :: Double
  , lat :: Double
  , distance :: Double
  , lngSpeed :: Double
  , latSpeed :: Double
  , distSpeed :: Double
  } deriving (Show, Eq, Ord, Generic)

-- | Default coordinates with all zeros -- when you don't care about/know the velocities,
-- which would be the case for most inputs (though most outputs _will_ include them.)
-- Usually you'll set only lat and lng (e.g. `defaultCoordinates{lat = 1.4, lng = 4.1}`)
-- when using it as an input for another function.
defaultCoordinates :: Coordinates
defaultCoordinates = Coordinates 0 0 0 0 0 0

-- | Constructor alias of `defaultCoordinates`, since it's used a lot in that role.
mkCoordinates :: Coordinates
mkCoordinates = defaultCoordinates

data HouseCusps = HouseCusps
  {
      i :: Double
    , ii :: Double
    , iii :: Double
    , iv :: Double
    , v :: Double
    , vi :: Double
    , vii :: Double
    , viii :: Double
    , ix :: Double
    , x :: Double
    , xi :: Double
    , xii :: Double
  } deriving (Show, Eq, Generic)

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

data CuspsCalculation = CuspsCalculation
  {
    houseCusps :: HouseCusps
  , angles :: Angles
  -- the underlying library may switch to Porphyrius
  -- if it's unable to determine a cusp.
  , systemUsed :: HouseSystem
  } deriving (Show, Eq, Generic)

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


-- TODO: these fromList fns could be captured in a typeclass...
fromList :: [Double] -> Coordinates
-- N.B. note that for some reason the SWE guys really like lng,lat coordinates
-- though only for this one function: https://www.astro.com/swisseph/swephprg.htm#_Toc19111235
fromList (sLng : sLat : c : d : e : f : _) = Coordinates sLng sLat c d e f
fromList _                           = error "Invalid coordinate array"

fromCuspsList :: [Double] -> HouseCusps
fromCuspsList (_ : _i : _ii : _iii : _iv : _v : _vi : _vii : _viii : _ix : _x : _xi : _xii : _)
    = HouseCusps _i _ii _iii _iv _v _vi _vii _viii _ix _x _xi _xii
fromCuspsList _ = error "Invalid cusps list"

fromAnglesList :: [Double] -> Angles
fromAnglesList (a : _mc : _armc : vtx : ea : cak : cam : pa : _ : _) =
    Angles a _mc _armc vtx ea cak cam pa
fromAnglesList _ = error "Invalid angles list"

planetNumber :: Planet -> PlanetNumber
planetNumber p = PlanetNumber $ CInt y
  where
    y = fromIntegral $ fromEnum p :: Int32

-- | Given a path to a directory, point the underlying ephemerides library to it.
-- You only need to call this function to provide an explicit ephemerides path,
-- if the environment variable SE_EPHE_PATH is set, it overrides this function.
setEphemeridesPath :: FilePath -> IO ()
setEphemeridesPath path =
    withCString path $ \ephePath -> c_swe_set_ephe_path ephePath

-- | Explicitly state that we don't want to set an ephemeris path,
-- which will default to the built-in ephemeris, or use the directory
-- in the SE_EPHE_PATH environment variable, if set.
setNoEphemeridesPath :: IO ()
setNoEphemeridesPath = c_swe_set_ephe_path nullPtr

-- | Explicitly release all "cache" pointers and open files obtained by the C
-- library.
closeEphemerides :: IO ()
closeEphemerides = c_swe_close

-- | Run a computation with a given ephemerides path open, and then close it. 
-- Note that the computation does _not_ receive the ephemerides, 
-- in keeping with the underlying library's side-effectful conventions.
withEphemerides :: FilePath -> (IO a) -> IO a
withEphemerides ephemeridesPath =
  bracket_ (setEphemeridesPath ephemeridesPath)
           (closeEphemerides)


-- | Run a computation with no explicit ephemerides set, if the SE_EPHE_PATH
-- environment variable is set, that will be used. If not, it'll fall back to
-- in-memory data.
withoutEphemerides :: (IO a) -> IO a
withoutEphemerides =
  bracket_ (setNoEphemeridesPath)
           (closeEphemerides)

-- | Given year, month and day as @Int@ and a time as @Double@, return
-- a single floating point number representing absolute Julian Time.
-- The input date is assumed to be in Gregorian time.
-- More info on this:
-- https://www.astro.com/swisseph/swephprg.htm#_Toc46406824
julianDay :: Int -> Int -> Int -> Double -> JulianTime
julianDay year month day hour = realToFrac $ c_swe_julday y m d h gregorian
  where
    y = fromIntegral year
    m = fromIntegral month
    d = fromIntegral day
    h = realToFrac hour

-- | Given a decimal representation of Julian Time (see @julianDay@),
-- and a @Planet@, returns either the position of that planet at the given time,
-- if available in the ephemeris, or an error.
-- This function is in IO because it _may_ allocate memory/read data beyond
-- its scope, when using ephemeris data. 
-- Call it with `withEphemerides` or `withoutEphemerides`.
-- Failing to call `closeEphemerides` at some point after calling this function
-- will likely result in a segmentation fault down the line!!
calculateCoordinates :: JulianTime -> Planet -> IO (Either String Coordinates)
calculateCoordinates time planet =
    allocaArray 6 $ \coords -> alloca $ \errorP -> do
        iflgret <- c_swe_calc (realToFrac time)
                              (planetNumber planet)
                              speed
                              coords
                              errorP

        if unCalcFlag iflgret < 0
            then do
                msg <- if errorP == nullPtr then 
                          trace ("No error here") $ pure $ "Unable to calculate position; NULL error from swiss ephemeris."
                        else
                          peekCAString errorP
                 
                trace ("The error was:" ++ msg ) $return $ Left msg
            else do
                result <- peekArray 6 coords
                return $ Right $ fromList $ map realToFrac result

-- | Alias for `calculateCuspsLenient`
calculateCusps :: JulianTime -> Coordinates -> HouseSystem -> IO CuspsCalculation
calculateCusps = calculateCuspsLenient

-- | Given a decimal representation of Julian Time (see `julianDay`),
-- a set of `Coordinates` (see `mkCoordinates`,) and a `HouseSystem`
-- (most applications use `Placidus`,) return a `CuspsCalculation` with all 12
-- house cusps in that system, and other relevant `Angles`. Notice that certain systems,
-- like `Placidus` and `Koch`, are very likely to fail close to the polar circles; in this
-- and other edge cases, the calculation returns cusps in the `Porphyrius` system.
-- This function is in IO because it _may_ allocate memory/read data beyond
-- its scope, when using ephemeris data. 
-- Call it with `withEphemerides` or `withoutEphemerides`.
-- Failing to call `closeEphemerides` at some point after calling this function
-- will likely result in a segmentation fault!!
calculateCuspsLenient :: JulianTime -> Coordinates -> HouseSystem -> IO CuspsCalculation
calculateCuspsLenient time loc sys = allocaArray 13 $ \cusps ->
    allocaArray 10 $ \ascmc -> do
        rval <- c_swe_houses (realToFrac time)
                             (realToFrac $ lat loc)
                             (realToFrac $ lng loc)
                             (fromIntegral $ toHouseSystemFlag sys)
                             cusps
                             ascmc
        cuspsL  <- peekArray 13 cusps
        anglesL <- peekArray 10 ascmc
        return $ CuspsCalculation
                  (fromCuspsList $ map realToFrac $ cuspsL) 
                  (fromAnglesList $ map realToFrac $ anglesL)
                  (if rval < 0 then Porphyrius else sys)

-- | Unlike `calculateCuspsLenient`, return a `Left` value if the required house system
-- couldn't be used to perform the calculations.
calculateCuspsStrict :: JulianTime -> Coordinates -> HouseSystem -> IO (Either String CuspsCalculation)
calculateCuspsStrict time loc sys = do
  calcs@(CuspsCalculation _ _ sys') <- calculateCuspsLenient time loc sys
  if sys' /= sys then
    pure $ Left $ "Unable to calculate cusps in the requested house system (used " ++ (show sys') ++ "instead.)"
  else
    pure $ Right calcs
