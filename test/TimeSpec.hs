{-# LANGUAGE OverloadedStrings #-}

module TimeSpec (spec) where

import SwissEphemeris (withEphemerides)
import SwissEphemeris.Time
import System.Directory
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Time.Format.ISO8601
import Data.Time
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

ephePath :: FilePath
ephePath = "./swedist/sweph_18"

withEphemeris :: IO () -> IO ()
withEphemeris act = do
  fullPath <- makeAbsolute ephePath
  withEphemerides fullPath $ do
    act

mkUTC :: String -> UTCTime
mkUTC  = fromJust . iso8601ParseM

spec :: Spec
spec = around_ withEphemeris $ do
  describe "pure conversion functions" $ do
    describe "utcToJulian" $ do
      it "can be constructed from a UTC" $ do
        let jd = utcToJulian (mkUTC "2021-07-03T23:05:54.696005Z")
        getJulianDay jd `shouldBe` 2459399.4624386113
        
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
        let time = mkUTC "2021-07-03T23:05:54.696005Z"
            time' = mkUTC "2021-07-03T23:05:54.696017503744Z"
        Just jd <- toJulianDay time :: (IO (Maybe JulianDayUT))
        roundTripped <- fromJulianDay jd :: IO UTCTime
        getJulianDay jd `shouldBe` 2459399.4624386113 
        time' `shouldBe` roundTripped
        utcTimeToPOSIXSeconds time `shouldBeApprox` utcTimeToPOSIXSeconds roundTripped

        
      it "can produce a UT1 Julian from UTC" $ do
        let time = mkUTC "2021-07-03T23:05:54.696005Z"
            time' = mkUTC "2021-07-03T23:05:54.696017503738Z"
        Just jd <- toJulianDay time :: (IO (Maybe JulianDayUT1))
        roundTripped <- fromJulianDay jd :: IO UTCTime
        getJulianDay jd `shouldBe` 2459399.46243737
        time' `shouldBe` roundTripped
        utcTimeToPOSIXSeconds time `shouldBeApprox` utcTimeToPOSIXSeconds roundTripped
        
      it "can produce a TT Julian from UTC" $ do
        let time = mkUTC "2021-07-03T23:05:54.696005Z"
            time' = mkUTC "2021-07-03T23:05:54.696017503738Z"
        Just jd <- toJulianDay time :: (IO (Maybe JulianDayTT))
        roundTripped <- fromJulianDay jd :: IO UTCTime
        getJulianDay jd `shouldBe` 2459399.463239352
        time' `shouldBe` roundTripped
        utcTimeToPOSIXSeconds time `shouldBeApprox` utcTimeToPOSIXSeconds roundTripped

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
