{-# LANGUAGE OverloadedStrings #-}

module PrecalculatedSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.Vector (fromList)
import Foreign.SweEphe4 (includeAll, includeSpeed, mustUseStoredEphe, EpheCalcFlag)
import SwissEphemeris
import SwissEphemeris.Precalculated
import System.Directory
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic

ephe4Path :: FilePath
ephe4Path = "./swedist/precalc"

ephePath :: FilePath
ephePath = "./swedist/sweph_18"

withFallback :: IO () -> IO ()
withFallback act = do
  fullPath <- makeAbsolute ephe4Path
  withEphemerides ephePath $ do
    setEphe4Path fullPath
    act

speedButNoFallback :: EpheCalcFlag
speedButNoFallback = foldEpheCalcOptions [includeSpeed, mustUseStoredEphe]

spec :: Spec
spec = around_ withFallback $ do
  describe "readEphemerisRaw" $ do
    context "with stored ephemeris, but no fallback ephemeris" $ do
        modifyMaxSuccess (const 10) $
          prop "it is unable to read ephemeris for out-of-range days" $
            forAll genOutOfRangeJulian $
              \time -> monadicIO $ do
                ephe <- run $ readEphemerisRaw includeAll speedButNoFallback $ JulianTime time
                assert $ isLeft ephe

        prop "it is able to read ephemeris for in-range days" $
          forAll genInRangeJulian $
            \time -> monadicIO $ do
              ephe <- run $ readEphemerisRaw includeAll speedButNoFallback $ JulianTime time
              assert $ isRight ephe

        
    context "with stored ephemeris, and fallback ephemeris" $ do
        prop "it is able to read ephemeris for in-range days" $
          forAll genInRangeJulian $
            \time -> monadicIO $ do
              ephe <- run $ readEphemerisRaw includeAll includeSpeed $ JulianTime time
              assert $ isRight ephe

        it "works for a weird time that failed another time" $ do
          ephe <- readEphemerisRaw includeAll includeSpeed (JulianTime 2450614.646993673)
          ephe `shouldSatisfy` isRight

        prop "it is also able to read ephemeris for out-of-range days" $
          forAll genOutOfRangeJulian $
            \time -> monadicIO $ do
              ephe <- run $ readEphemerisRaw includeAll includeSpeed $ JulianTime time
              assert $ isRight ephe

  describe "readEphemerisEasy" $ do
      it "fails to read when the Julian date is out of range, and no fallback is allowed" $ do
        ephe <- readEphemerisEasy False (julianDay 2021 6 6 0.0)
        fullPath <- makeAbsolute ephe4Path
        let errorMessage = Left $ "eph4_posit: file " ++ fullPath ++ "/sep4_245 does not exist\n"
        ephe `shouldBe` errorMessage
        
      it "reads ephemeris for a Julian date out of range, with fallback" $ do
        ephe <- readEphemerisEasy True (julianDay 2021 6 6 0.0)
        ephe `shouldSatisfy` isRight
  
      it "reads all the ephemeris for a Julian date in range, no fallback" $ do
        ephe <- readEphemerisEasy False (julianDay 1989 1 6 0.0)
        let expectedPositions =
              fromList
                [ EphemerisPosition {ephePlanet = Sun, epheLongitude = 285.64657777777774, epheSpeed = 1.019651435185189},
                  EphemerisPosition {ephePlanet = Moon, epheLongitude = 262.4723416666667, epheSpeed = 13.539089351851851},
                  EphemerisPosition {ephePlanet = Mercury, epheLongitude = 304.3135666666667, epheSpeed = 1.2740435185185066},
                  EphemerisPosition {ephePlanet = Venus, epheLongitude = 264.0478777777778, epheSpeed = 1.2512999537036904},
                  EphemerisPosition {ephePlanet = Mars, epheLongitude = 22.784491666666668, epheSpeed = 0.5238418981481495},
                  EphemerisPosition {ephePlanet = Jupiter, epheLongitude = 56.44158888888889, epheSpeed = -4.888703703703475e-2},
                  EphemerisPosition {ephePlanet = Saturn, epheLongitude = 276.18193333333335, epheSpeed = 0.11736805555557339},
                  EphemerisPosition {ephePlanet = Uranus, epheLongitude = 272.0516777777778, epheSpeed = 5.919861111109033e-2},
                  EphemerisPosition {ephePlanet = Neptune, epheLongitude = 280.1110194444444, epheSpeed = 3.782453703704884e-2},
                  EphemerisPosition {ephePlanet = Pluto, epheLongitude = 224.6817138888889, epheSpeed = 2.390787037037493e-2},
                  EphemerisPosition {ephePlanet = MeanNode, epheLongitude = 337.52351666666664, epheSpeed = -5.290370370371041e-2},
                  EphemerisPosition {ephePlanet = TrueNode, epheLongitude = 336.0941305555556, epheSpeed = -0.15458472222221362},
                  EphemerisPosition {ephePlanet = Chiron, epheLongitude = 93.53731666666667, epheSpeed = -6.391388888888609e-2},
                  EphemerisPosition {ephePlanet = MeanApog, epheLongitude = 176.27789722222224, epheSpeed = 0.11092824074075718}
                ]
            expectedEphe =
              Right $
                Ephemeris
                  { epheDate = JulianTime 2447532.5,
                    epheEcliptic = 23.44288611111111,
                    epheNutation = 1.9472222222222224e-3,
                    ephePositions = expectedPositions
                  }
        ephe `shouldBe` expectedEphe

genOutOfRangeJulian :: Gen Double
genOutOfRangeJulian = oneof [choose (2420000.5, 2430000.5), choose (2450000.5, 2470000.5)]

genInRangeJulian :: Gen Double
-- NOTE(luis) technically we could go up to 2449999.9,
-- however, due to interpolation, for some dates the underlying
-- library _may_ try to peek into the next block, which we don't
-- bundle intentionally.
genInRangeJulian = choose (2440005.0, 2449990.0)
