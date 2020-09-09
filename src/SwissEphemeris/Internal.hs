{-# LANGUAGE DeriveGeneric  #-}

{-|
Module: SwissEphemeris.Internal
Description: helper functions and types, not for public consumption.

The types defined here are exported publicly for convenience, but at least in versions <= 2.x, they're likely to evolve.
-}

module SwissEphemeris.Internal where

import Foreign.SwissEphemeris
import Data.Bits
import           GHC.Generics
import Foreign.C.Types
import Foreign (Int32)
import Data.Char (ord)

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
newtype SiderealTime = SiderealTime {unSidereal :: Double}
    deriving (Show, Eq)

-- | Convert tropical sidereal time to an ARMC relative to a given longitude
-- this is not explained in the SWE docs, I pulled it from:
--   armc = swe_degnorm(swe_sidtime0(tjd_ut, eps + nutlo[1], nutlo[0]) * 15 + geolon);
-- (line 144 of swehouse.c, part of swe_houses)
sidToArmc :: SiderealTime -> Double -> ARMC
sidToArmc s l = ARMC $ (unSidereal s) * 15 + l

newtype ARMC = ARMC {unArmc :: Double}
    deriving (Show, Eq)

-- | The cusp of a given "house" or "sector"
-- see:
-- <https://www.astro.com/swisseph/swephprg.htm#_Toc49847888 14.1 House cusp calculation>
-- and <https://www.astro.com/swisseph/swisseph.htm#_Toc46391705 6.2 Astrological house systems>
type HouseCusp = Double


-- | Position data for a celestial body, includes rotational speeds.
-- see:
-- <https://www.astro.com/swisseph/swephprg.htm#_Toc49847837 3.4 Position and speed>
data EclipticPosition = EclipticPosition
  {
    lng :: Double
  , lat :: Double
  , distance :: Double -- in AU
  , lngSpeed :: Double -- deg/day
  , latSpeed :: Double -- deg/day
  , distSpeed :: Double -- deg/day
  } deriving (Show, Eq, Generic)

type Coordinates = EclipticPosition

data EquatorialPosition = EquatorialPosition
  {
    rightAscension :: Double
  , declination :: Double
  , eqDistance  :: Double -- same as distance in `EclipticPosition`, uses AU
  , ascensionSpeed :: Double -- deg/day
  , declinationSpeed :: Double -- deg/day
  , eqDistanceSpeed :: Double -- deg/day
  } deriving (Show, Eq, Generic)

data ObliquityAndNutation = ObliquityAndNutation
  {
    eclipticObliquity :: Double
  , eclipticMeanObliquity :: Double
  , nutationLongitude :: Double
  , nutationObliquity :: Double
  } deriving (Show, Eq, Generic)

-- | Default coordinates with all zeros -- when you don't care about/know the velocities,
-- which would be the case for most inputs (though most outputs /will/ include them.)
-- Usually you'll set only lat and lng (e.g. @defaultEclipticPosition{lat = 1.4, lng = 4.1}@)
-- when using it as an input for another function.
defaultEclipticPosition :: EclipticPosition
defaultEclipticPosition = EclipticPosition 0 0 0 0 0 0

-- | Constructor alias of `defaultEclipticPosition`, since it's used a lot in that role.
mkEclipticPosition :: EclipticPosition
mkEclipticPosition = defaultEclipticPosition

-- TODO: need a new type: GeographicPosition, with only lat and lng.
mkCoordinates :: EclipticPosition
mkCoordinates = mkEclipticPosition

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

data HousePosition = HousePosition
    {
        houseNumber :: Int
    ,   houseCuspDistance :: Double
    } deriving (Show, Eq, Generic)

-- folders for bitwise flags, and some opinionated defaults.

mkCalculationOptions :: [CalcFlag] -> CalcFlag
mkCalculationOptions = CalcFlag . foldr ((.|.) . unCalcFlag) 0

defaultCalculationOptions :: [CalcFlag]
defaultCalculationOptions =  [speed, swissEph]

mkRoundingOptions :: [SplitDegFlag] -> SplitDegFlag
mkRoundingOptions = SplitDegFlag . foldr ((.|.) . unSplitDegFlag) 0

defaultRoundingOptions :: [SplitDegFlag]
defaultRoundingOptions = 
    [ splitKeepDeg  -- don't round up to the next degree
    , splitKeepSign -- don't round up to the next sign
    , splitZodiacal -- reference zodiacal signs
    , splitRoundSec -- don't need second fractions.
    ]

-- helpers

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

coordinatesFromList :: [Double] -> EclipticPosition
-- N.B. note that for some reason the SWE guys really like lng,lat coordinates
-- though only for this one function: https://www.astro.com/swisseph/swephprg.htm#_Toc19111235
coordinatesFromList (sLng : sLat : c : d : e : f : _) = EclipticPosition sLng sLat c d e f
-- the underlying library goes to great lengths to not return fewer than 6 data,
-- it instead uses zeroes for unavailable entries.
coordinatesFromList _                           = EclipticPosition 0 0 0 0 0 0

eclipticFromList :: [Double] -> EclipticPosition
eclipticFromList = coordinatesFromList

eclipticToList :: EclipticPosition -> [Double]
eclipticToList (EclipticPosition sLng sLat c d e f) =  (sLng : sLat : c : d : e : f : [])

equatorialFromList :: [Double] -> EquatorialPosition
equatorialFromList (a:b:c:d:e:f:_) = EquatorialPosition a b c d e f
equatorialFromList _               = EquatorialPosition 0 0 0 0 0 0

equatorialToList :: EquatorialPosition -> [Double]
equatorialToList (EquatorialPosition a b c d e f) =  (a:b:c:d:e:f:[])

obliquityNutationFromList :: [Double] -> ObliquityAndNutation
obliquityNutationFromList (a:b:c:d:_:_:_) = ObliquityAndNutation a b c d
obliquityNutationFromList _               = ObliquityAndNutation 0 0 0 0

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
