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
,   julianDay
,   calculateCoordinates
,   calculateCusps
)where

import           Foreign.SwissEphemeris

import           Foreign
import           GHC.Generics
import           System.IO.Unsafe
import           Foreign.C.Types
import           Foreign.C.String
import           Data.Char                      ( ord )

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
-- Ideally you'll set lat and lng: defaultCoordinates{lat = 1.4, lng = 4.1}
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

-- | Combine @CalcFlag@ as bitwise options. 
-- For example, can use it to obtain the equatorial,
-- not eliptical (default,) positions of planetary bodies.
calculationOptions :: [CalcFlag] -> CalcFlag
calculationOptions = CalcFlag . foldr ((.|.) . unCalcFlag) 0

-- | Given an *absolute* path, point the underlying ephemerides library to it.
-- takes a `String` for easy use with the `directory` package.
setEphemeridesPath :: String -> IO ()
setEphemeridesPath path =
    -- note, using the *CA* variants of String functions, since the swe
    -- code seems to be ignorant of UTF8:
    -- http://hackage.haskell.org/package/base-4.14.0.0/docs/Foreign-C-String.html#g:3
    withCAString path $ \ephePath -> c_swe_set_ephe_path ephePath

-- | Given year, month and day as @Int@ and a time as @Double@, return
-- a single floating point number representing absolute Julian Time.
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
                                 --(calculationOptions [swissEph, speed, equatorialPositions ])
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

-- | Given a decimal representation of Julian Time (see @julianDay@),
-- and a set of @Coordinates@ (see @calculateCoordinates@,) and a @HouseSystem@
-- (most applications use @Placidus@,) return a @CuspsCalculation@ with all 12
-- house cusps in that system, and other relevant @Angles@.
calculateCusps :: JulianTime -> Coordinates -> HouseSystem -> CuspsCalculation
calculateCusps time loc sys = unsafePerformIO $ allocaArray 13 $ \cusps ->
    allocaArray 10 $ \ascmc -> do
        c_swe_houses (realToFrac time)
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
