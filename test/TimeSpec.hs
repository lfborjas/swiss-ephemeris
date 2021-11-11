{-# LANGUAGE OverloadedStrings #-}

module TimeSpec (spec) where

import SwissEphemeris (setEphemeridesPath)
import SwissEphemeris.Time
import System.Directory
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Time
import Data.Maybe (isJust)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, POSIXTime)
import Arbitrary ()
import Utils ( ephePath, mkUTC, civilTime )

withEphemeris :: IO ()
withEphemeris = do
  fullPath <- makeAbsolute ephePath
  setEphemeridesPath fullPath

leapSeconds :: Gen UTCTime
leapSeconds =
  -- a sampling of:
  -- https://www.nist.gov/pml/time-and-frequency-division/time-realization/leap-seconds
  oneof [
    pure (mkUTC "2016-12-31T23:59:60Z"),
    pure (mkUTC "1985-06-30T23:59:60Z"),
    pure (mkUTC "1987-12-31T23:59:60Z")
    ]

validTime :: Gen UTCTime
validTime = oneof [civilTime, leapSeconds]

spec :: Spec
spec = beforeAll_ withEphemeris $ do
  describe "pure conversion functions" $ do
    describe "dayToJulianDay/dayFromJulianDay" $ do
      it "can produce a fake TT julian from a Day" $ do
        let day = fromGregorian 2021 7 1
            jd = dayToJulianDay day :: JulianDayTT
            rt = dayFromJulianDay jd
        getJulianDay jd `shouldBe` 2459397.0
        rt `shouldBe` day

      prop "can roundtrip a fake TT julian from any Day" $
        forAll validTime $
          \(UTCTime day _) ->
            let jd = dayToJulianDay day :: JulianDayTT
            in dayFromJulianDay jd `shouldBe` day

    describe "utcToJulian" $ do
      it "can be constructed from a UTC" $ do
        let jd = utcToJulian (mkUTC "2021-07-03T23:05:54.696005Z")
        getJulianDay jd `shouldBe` 2459399.4624386113
        getJulianDay (julianMidnight jd) `shouldBe` 2459398.5
        getJulianDay (julianNoon jd) `shouldBe` 2459399.0

      it "can produce a UT Julian even from a fake leap second" $ do
        let time = mkUTC "-5779-06-16T23:59:60Z"
            -- NOTE: notice how the roundtripped version correctly
            -- lands on the next day. This same value would
            -- result in an error when using the more strict UT1/TT
            -- functions.
            time' = mkUTC "-5779-06-17T00:00:00Z"
        Just jd <- toJulianDay time :: (IO (Maybe JulianDayUT))
        roundTripped <- fromJulianDay jd
        time' `shouldBe` roundTripped

    describe "julianDay" $ do
      it "permits easy conversion from date components to JD (no validation)" $ do
        let jd = gregorianToJulianDayUT 2021 7 3 0
        getJulianDay jd `shouldBe` 2459398.5

    describe "coerceUT" $ do
      it "shows that a UT JD is within a second of a UT1 JD" $ do
        let time = mkUTC "2021-07-03T23:05:54.696005Z"
            jdut = utcToJulian time
        Just jdut1 <- toJulianDay time :: (IO (Maybe JulianDayUT1))
        let fakeUT1 = coerceUT jdut
            difference = abs $ subtract (getJulianDay jdut1) (getJulianDay fakeUT1)
        difference `shouldSatisfy` (< 0.00001)

  describe "conversion functions" $ do
    describe "toJulianDay/fromJulianDay" $ do
      it "can produce a UT Julian from UTC" $ do
        let time = mkUTC  "2021-07-03T23:05:54.696005Z"
            time' = mkUTC "2021-07-03T23:05:54.696017503744Z"
        Just jd <- toJulianDay time :: (IO (Maybe JulianDayUT))
        roundTripped <- fromJulianDay jd :: IO UTCTime
        getJulianDay jd `shouldBe` 2459399.4624386113
        time' `shouldBe` roundTripped
        utcTimeToPOSIXSeconds time `shouldBeApprox` utcTimeToPOSIXSeconds roundTripped

      it "works for that weird timestamp that broke ubuntu once" $ do
        let time = mkUTC "7514-11-10T22:13:08.35750808104Z"
        jd <- toJulianDay time :: (IO (Maybe JulianDayUT1))
        jd `shouldSatisfy` isJust
        (getJulianDay <$> jd) `shouldBe` Just 4465805.425791175

      prop "can roundtrip a UT Julian from any UTC" $
        forAll validTime $
          \time -> monadicIO $ do
            Just jd <- run (toJulianDay time :: (IO (Maybe JulianDayUT)))
            roundTripped <- run $ fromJulianDay jd
            let jdSeconds = utcTimeToPOSIXSeconds time
                rtSeconds = utcTimeToPOSIXSeconds roundTripped
                difference = abs $ subtract jdSeconds rtSeconds
            assert $ difference < 1e-04

      it "can produce a UT1 Julian from UTC" $ do
        let time = mkUTC  "2021-07-03T23:05:54.696005Z"
            time' = mkUTC "2021-07-03T23:05:54.696017503738Z"
        Just jd <- toJulianDay time :: (IO (Maybe JulianDayUT1))
        roundTripped <- fromJulianDay jd :: IO UTCTime
        getJulianDay jd `shouldBe` 2459399.46243737
        time' `shouldBe` roundTripped
        utcTimeToPOSIXSeconds time `shouldBeApprox` utcTimeToPOSIXSeconds roundTripped


      prop "can roundtrip a UT1 Julian from any UTC" $
        forAll validTime $
          \time -> monadicIO $ do
            Just jd <- run (toJulianDay time :: (IO (Maybe JulianDayUT1)))
            roundTripped <- run $ fromJulianDay jd
            let jdSeconds = utcTimeToPOSIXSeconds time
                rtSeconds = utcTimeToPOSIXSeconds roundTripped
                difference = abs $ subtract jdSeconds rtSeconds

            assert $ difference < timeEpsilon

      it "can produce a TT Julian from UTC" $ do
        let time = mkUTC "2021-07-03T23:05:54.696005Z"
            time' = mkUTC "2021-07-03T23:05:54.696017503738Z"
        Just jd <- toJulianDay time :: (IO (Maybe JulianDayTT))
        roundTripped <- fromJulianDay jd :: IO UTCTime
        getJulianDay jd `shouldBe` 2459399.463239352
        time' `shouldBe` roundTripped
        utcTimeToPOSIXSeconds time `shouldBeApprox` utcTimeToPOSIXSeconds roundTripped

      prop "can roundtrip a TT Julian from any UTC" $
        forAll validTime $
          \time -> monadicIO $ do
            Just jd <- run (toJulianDay time :: (IO (Maybe JulianDayTT)))
            roundTripped <- run $ fromJulianDay jd
            let jdSeconds = utcTimeToPOSIXSeconds time
                rtSeconds = utcTimeToPOSIXSeconds roundTripped
                difference = abs $ subtract jdSeconds rtSeconds
  
            assert $ difference < timeEpsilon

  describe "delta time" $ do
    describe "deltaTime (simple)" $ do
      it "can be used to find a TT from a UT1" $ do
        let time = mkUTC "2021-07-03T23:05:54.696005Z"
        Just jdut <- toJulianDay time :: (IO (Maybe JulianDayUT1))
        Just jdtt <- toJulianDay time :: (IO (Maybe JulianDayTT))
        deltaT    <- deltaTime jdut
        -- JD(TT) = JD(UT1) + dT@JD(UT1)
        let derivedTT = addDeltaTime jdut deltaT
        derivedTT `shouldBe` jdtt

      prop "can be used to find a TT from any UT1" $
        forAll validTime $
          \time -> monadicIO $ do
            Just jdut <- run (toJulianDay time :: (IO (Maybe JulianDayUT1)))
            Just jdtt <- run (toJulianDay time :: (IO (Maybe JulianDayTT)))
            deltaT    <- run $ deltaTime jdut
            -- JD(TT) = JD(UT1) + dT@JD(UT1)
            let derivedTT = addDeltaTime jdut deltaT
            assert $ derivedTT == jdtt


-- TODO: move to a helpers module
shouldBeApprox :: (Fractional a, Ord a, Show a) => a -> a -> Expectation
shouldBeApprox expected actual =
  if abs (actual - expected) < abs margin * max 1 (abs expected)
    then pure ()
    else expectationFailure msg
  where
    margin = 1e-3
    msg =
      mconcat
        [ "Failure:\n expected: ",
          show actual,
          " to be approximately equal to ",
          show expected
        ]

infix 1 `shouldBeApprox`

-- | Acceptable difference between two roundtripped timestamps
timeEpsilon :: POSIXTime
timeEpsilon = 1e-03
