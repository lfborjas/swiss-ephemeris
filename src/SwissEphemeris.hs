{-# LANGUAGE DeriveGeneric     #-}

module SwissEphemeris (
    Planet(..)
,   HouseSystem(..)
,   JulianTime
,   Coordinates(..)
,   HouseCusps(..)
,   Angles(..)
,   CuspsCalculation(..)
,   defaultCoordinates
,   setEphemeridesPath
,   closeEphemerides
,   withEphemerides
,   julianDay
,   calculateCoordinates
,   calculateCusps
-- MonadFail versions of calculations.
,   calculateCoordinatesM
,   calculateCuspsM
)where

import           Foreign.SwissEphemeris

import           Foreign
import           GHC.Generics
import           System.IO.Unsafe
import           Foreign.C.Types
import           Foreign.C.String
import           Data.Char                      ( ord )
import Control.Exception (bracket_)
import Control.Monad.Fail (MonadFail, fail)
import Prelude hiding (fail)

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
                 deriving (Show, Eq, Ord, Generic)

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

-- | Given an *absolute* path, point the underlying ephemerides library to it.
--  Takes a `String` for easy use with the `directory` package.
-- You only need to call this function to provide an explicit ephemerides path,
-- if the environment variable `SE_EPHE_PATH` is set, it overrides this function.
setEphemeridesPath :: String -> IO ()
setEphemeridesPath path =
    withCString path $ \ephePath -> c_swe_set_ephe_path ephePath

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
calculateCoordinates :: JulianTime -> Planet -> Either String Coordinates
calculateCoordinates time planet =
    unsafePerformIO $ allocaArray 6 $ \coords -> alloca $ \errorP -> do
        let iflgret = c_swe_calc (realToFrac time)
                                 (planetNumber planet)
                                 speed
                                 coords
                                 errorP

        if unCalcFlag iflgret < 0
            then do
                msg <- peekCAString errorP
                return $ Left msg
            else do
                result <- peekArray 6 coords
                return $ Right $ fromList $ map realToFrac result

-- | MonadFail version of `calculateCoordinates`, in case you don't particularly care
-- about the error message (since it's likely to be due to misconfigured ephe files)
-- and want it to play nice with other `MonadFail` computations.
calculateCoordinatesM :: MonadFail m => JulianTime -> Planet -> m Coordinates
calculateCoordinatesM time planet = do
  let coords = calculateCoordinates time planet
  case coords of
    Left e -> fail e
    Right c -> return c

-- | Given a decimal representation of Julian Time (see @julianDay@),
-- and a set of @Coordinates@ (see @calculateCoordinates@,) and a @HouseSystem@
-- (most applications use @Placidus@,) return either @CuspsCalculation@ with all 12
-- house cusps in that system, and other relevant @Angles@, or an error.
calculateCusps :: JulianTime -> Coordinates -> HouseSystem -> Either String CuspsCalculation
calculateCusps time loc sys = unsafePerformIO $ allocaArray 13 $ \cusps ->
    allocaArray 10 $ \ascmc -> do
        let rval = c_swe_houses (realToFrac time)
                                (realToFrac $ lat loc)
                                (realToFrac $ lng loc)
                                (fromIntegral $ toHouseSystemFlag sys)
                                cusps
                                ascmc
        if rval < 0 then do
          return $ Left "Unable to calculate cusps for the given point and house system."
        else do
          cuspsL  <- peekArray 13 cusps
          anglesL <- peekArray 10 ascmc
          return $ Right $ CuspsCalculation
                             (fromCuspsList $ map realToFrac $ cuspsL) 
                             (fromAnglesList $ map realToFrac $ anglesL)

-- | MonadFail version of `calculateCusps`, in case you don't particularly care about
-- the error message (there's only one error scenario currently: inability to 
-- determine cusps, in coordinates not contemplated by the given house system.
calculateCuspsM :: MonadFail m => JulianTime -> Coordinates -> HouseSystem -> m CuspsCalculation
calculateCuspsM time loc sys = do
  let calcs = calculateCusps time loc sys
  case calcs of
    Left e -> fail e
    Right c -> return c
