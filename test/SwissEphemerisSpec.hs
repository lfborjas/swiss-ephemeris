{-# LANGUAGE OverloadedStrings #-}

module SwissEphemerisSpec (spec) where

import Control.Monad (forM_)
import Data.Either (isLeft, isRight)
import SwissEphemeris
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Utils
import Data.Time (UTCTime)

-- to verify that we're calling things correctly, refer to the swiss ephemeris test page:
-- https://www.astro.com/swisseph/swetest.htm
-- to see the results for this specific test suite, see:
-- https://www.astro.com/cgi/swetest.cgi?b=6.1.1989&n=1&s=1&p=p&e=-eswe&f=PlbRS&arg=
-- note the `PlbRS` format, which outputs latitude and longitude as decimals, not degrees
-- for easier comparison.

withoutEphemerides' :: IO ()
withoutEphemerides' = setNoEphemeridesPath 

withEphemerides' :: IO ()
withEphemerides' = setEphemeridesPath ephePath

spec :: Spec
spec = do
  describe "eclipticToEquatorial" $
    it "converts between ecliptic and equatorial" $ do
      let e = eclipticToEquatorial (ObliquityInformation 23.2 0 0 0) $ EclipticPosition 285.6465775 (-0.0000826) 1 0 0 0
          equatorial = EquatorialPosition {rightAscension = 286.9471857576873, declination = -22.29312747773143, eqDistance = 1.0, ascensionSpeed = 0.0, declinationSpeed = 0.0, eqDistanceSpeed = 0.0}
      rightAscension e `shouldBeApprox` rightAscension equatorial
      declination e `shouldBeApprox` declination equatorial

  describe "equatorialToEcliptic" $
    it "converts between equatorial and ecliptic" $ do
      let e = equatorialToEcliptic (ObliquityInformation 23.2 0 0 0) $ EquatorialPosition {rightAscension = 286.9471857576873, declination = -22.29312747773143, eqDistance = 1.0, ascensionSpeed = 0.0, declinationSpeed = 0.0, eqDistanceSpeed = 0.0}
          ecliptic = EclipticPosition 285.6465775 (-0.0000826) 1 0 0 0
      lng e `shouldBeApprox` lng ecliptic
      lat e `shouldBeApprox` lat ecliptic

  describe "splitDegrees" $
    it "splits a given longitude into its components, not relative to the zodiac" $ do
      let longitude = 285.64723120365153
          split = LongitudeComponents {longitudeZodiacSign = Nothing, longitudeDegrees = 285, longitudeMinutes = 38, longitudeSeconds = 50, longitudeSecondsFraction = 3.233314550481481e-2, longitudeSignum = Just 1, longitudeNakshatra = Nothing}
      splitDegrees defaultSplitDegreesOptions longitude `shouldBe` split

  describe "splitDegreesZodiac" $
    it "splits a given longitude into its components, relative to the nearest zodiac sign; rounds seconds, keeps degrees and sign." $ do
      let longitude = 285.64723120365153
          split = LongitudeComponents {longitudeZodiacSign = Just Capricorn, longitudeDegrees = 15, longitudeMinutes = 38, longitudeSeconds = 50, longitudeSecondsFraction = 0.0, longitudeSignum = Nothing, longitudeNakshatra = Nothing}
      splitDegreesZodiac longitude `shouldBe` split

  beforeAll_ withoutEphemerides' $ do
    describe "calculateEclipticPosition" $ do
      it "calculates ecliptic coordinates for the Sun for a specific day" $ do
        let time = mkJulian 1989 1 6 0.0
            expectedCoords =
              Right $
                EclipticPosition
                  { lng = 285.64723120365153,
                    lat = -8.664716530514133e-5,
                    distance = 0.9833448914987339,
                    lngSpeed = 1.0196505771457982,
                    latSpeed = 1.4550248863443192e-5,
                    distSpeed = 1.7364210462433863e-5
                  }
        coords <- calculateEclipticPosition time Sun
        coords `compareCoords` expectedCoords

      it "fails to calculate coordinates for Chiron if no ephemeris file is set" $ do
        let time = mkJulian 1989 1 6 0.0
            expectedCoords = Left "SwissEph file 'seas_18.se1' not found in PATH '.:/users/ephe2/:/users/ephe/'"
        coords <- calculateEclipticPosition time Chiron
        coords `shouldBe` expectedCoords

    describe "calculateCuspsStrict" $ do
      it "calculates cusps and angles for a given place and time, keeping the same house system (not near the poles)" $ do
        let time = mkJulian 1989 1 6 0.0
        let place = GeographicPosition {geoLat = 14.0839053, geoLng = -87.2750137}
        let expectedCalculations =
              CuspsCalculation
                [ 112.20189657163523,
                  138.4658382335878,
                  167.69682489058204,
                  199.79861981778183,
                  232.2797046698429,
                  263.0249102802477,
                  292.20189657163525,
                  318.46583823358776,
                  347.69682489058204,
                  19.798619817781823,
                  52.27970466984291,
                  83.02491028024768
                ]
                Angles
                  { ascendant = 112.20189657163523,
                    mc = 19.798619817781823,
                    armc = 18.277351820745423,
                    vertex = 216.1872418365295,
                    equatorialAscendant = 106.85773516967282,
                    coAscendantKoch = 101.19442735316477,
                    coAscendantMunkasey = 153.1221838791593,
                    polarAscendant = 281.19442735316477
                  }
                Placidus

        calcs <- calculateCuspsStrict Placidus time place
        calcs `compareCalculations` Right expectedCalculations

      it "fails when using a house system that is unable to calculate cusps near the poles" $ do
        let time = mkJulian 1989 1 6 0.0
            -- Longyearbyen:
            place = GeographicPosition {geoLat = 78.2232, geoLng = 15.6267}
        calcs <- calculateCuspsStrict Placidus time place
        calcs `shouldSatisfy` isLeft

    describe "calculateCuspsLenient" $ do
      it "falls back to Porphyry when calculating cusps for a place near the poles" $ do
        let time = mkJulian 1989 1 6 0.0
            -- Longyearbyen:
            place = GeographicPosition {geoLat = 78.2232, geoLng = 15.6267}
            expected = CuspsCalculation {houseCusps = [190.88156009524067, 226.9336677703179, 262.9857754453951, 299.0378831204723, 322.9857754453951, 346.9336677703179, 10.881560095240673, 46.933667770317925, 82.98577544539512, 119.03788312047234, 142.98577544539512, 166.9336677703179], angles = Angles {ascendant = 190.88156009524067, mc = 119.03788312047234, armc = 121.17906552074543, vertex = 36.408617337292114, equatorialAscendant = 213.4074315205484, coAscendantKoch = 335.2547300150891, coAscendantMunkasey = 210.81731854391526, polarAscendant = 155.2547300150891}, systemUsed = Porphyrius}
        calcs <- calculateCuspsLenient Placidus time place
        systemUsed calcs `shouldBe` Porphyrius
        Right calcs `compareCalculations` Right expected

      prop "calculates cusps and angles for a wide range of points in space and time, in all supported house systems. Note that it may fall back to Porphyrius for some exotic (or polar) points." $
        -- see: `House cusps beyond the polar circle` in https://www.astro.com/swisseph/swisseph.htm#_Toc46391722
        -- and:
        -- > Placidus and Koch house cusps as well as Gauquelin sectors cannot be computed beyond the polar circle.
        -- > In such cases, swe_houses() switches to Porphyry houses (each quadrant is divided into three equal parts) and returns the error code ERR.
        -- > In addition, Sunshine houses may fail, e.g. when required for a date which is outside the time range of our solar ephemeris. Here, also, Porphyry houses will be provided.
        -- from: https://www.astro.com/swisseph/swephprg.htm
        forAll genCuspsQuery $
          \((la, lo), time, houseSystem) -> monadicIO $ do
            calcs <- run $ calculateCusps houseSystem time (GeographicPosition {geoLat = la, geoLng = lo})
            assert $ systemUsed calcs `elem` [houseSystem, Porphyrius]
            assert $ length (houseCusps calcs) == 12

      prop "calculates cusps and angles for points outside of the polar circles in the requested house system, no fallback." $
        forAll genCuspsNonPolarQuery $
          \((la, lo), time, houseSystem) -> monadicIO $ do
            calcs <- run $ calculateCusps houseSystem time (GeographicPosition {geoLat = la, geoLng = lo})
            assert $ systemUsed calcs == houseSystem
            assert $ length (houseCusps calcs) == 12

    describe "calculateHousePositionSimple" $
      it "calculates accurate house positions for some known planets" $ do
        let time = mkJulian 1989 1 6 0.0
            place = GeographicPosition {geoLat = 14.06, geoLng = -87.13}
            housePos = calculateHousePositionSimple Placidus time place
            houseN = fmap houseNumber
            mkEcliptic = EclipticPosition 0 0 0 0 0 0

        sunH <- housePos $ mkEcliptic {lng = 285.6465775, lat = -0.0000826}
        moonH <- housePos $ mkEcliptic {lng = 262.4723493, lat = -4.9055744}
        mercuryH <- housePos $ mkEcliptic {lng = 304.3135759, lat = -1.3441786}
        venusH <- housePos $ mkEcliptic {lng = 264.0478768, lat = 0.6114330}
        marsH <- housePos $ mkEcliptic {lng = 22.7844912, lat = 0.6472527}
        jupiterH <- housePos $ mkEcliptic {lng = 56.4415899, lat = -0.8785552}
        saturnH <- housePos $ mkEcliptic {lng = 276.1819323, lat = 0.7124667}
        uranusH <- housePos $ mkEcliptic {lng = 272.0516769, lat = -0.2200407}
        neptuneH <- housePos $ mkEcliptic {lng = 280.1110192, lat = 0.9024311}
        plutoH <- housePos $ mkEcliptic {lng = 224.6817137, lat = 15.6296117}
        meanNH <- housePos $ mkEcliptic {lng = 337.5235158, lat = 0.0}
        chironH <- housePos $ mkEcliptic {lng = 93.5373174, lat = -6.8493261}

        houseN sunH `shouldBe` Right 6
        houseN moonH `shouldBe` Right 5
        houseN mercuryH `shouldBe` Right 7
        houseN venusH `shouldBe` Right 6
        houseN marsH `shouldBe` Right 10
        houseN jupiterH `shouldBe` Right 11
        houseN saturnH `shouldBe` Right 6
        houseN uranusH `shouldBe` Right 6
        houseN neptuneH `shouldBe` Right 6
        houseN plutoH `shouldBe` Right 4
        houseN meanNH `shouldBe` Right 8
        houseN chironH `shouldBe` Right 12

    describe "calculateEquatorialPosition" $
      it "calculates equatorial coordinates for the Sun for a specific date" $ do
        let time = mkJulian 1989 1 6 0.0
            expectedPosition = Right (EquatorialPosition {rightAscension = 286.9771081985312, declination = -22.52537550693229, eqDistance = 0.9833448914987338, ascensionSpeed = 1.0963895541503408, declinationSpeed = 0.1184607330811988, eqDistanceSpeed = 1.736421046243553e-5})
        position <- calculateEquatorialPosition time Sun
        position `shouldBe` expectedPosition

    describe "calculateObliquity" $
      it "calculates obliquity of the ecliptic, and nutation, for a specific date" $ do
        let time = mkJulian 1989 1 6 0.0
            expectedObliquity = Right (ObliquityInformation {eclipticObliquity = 23.44288555112768, eclipticMeanObliquity = 23.44070869609064, nutationLongitude = 1.9483531068634399e-3, nutationObliquity = 2.1768550370416455e-3})
        obliquity <- calculateObliquity time
        obliquity `shouldBe` expectedObliquity

    describe "deltaTime" $
      it "calculates the Delta T for a specific date" $ do
        let time = mkJulian 1989 1 6 0.0
            expectedDeltaT = 6.517108007976064e-4
        deltaT <- deltaTime time
        deltaT `shouldBeApprox` expectedDeltaT
        
    describe "solar eclipses" $
      it "calculates the date and location of a solar eclipse" $ do
        let startTime = mkJulian 2021 8 9 0.0
            expectedEclipseDate = mkJulian 2021 12 4 7.0
        Right (nextEclipseType, nextEclipseJD) <- nextSolarEclipseWhen [] SearchForward startTime
        Right nextEclipseLoc <- nextSolarEclipseWhere nextEclipseJD
        nextEclipseType `shouldBe` TotalSolarEclipse
        julianNoon nextEclipseJD `shouldBe` julianNoon expectedEclipseDate
        -- somewhere in Antarctica
        geoLat nextEclipseLoc `shouldBeApprox` -76.75422256653523
        geoLng nextEclipseLoc `shouldBeApprox` -46.06809018915021

    describe "lunar eclipses" $
      it "calculates the date of a lunar eclipse" $ do
        let startTime = mkJulian 2021 8 9 0.0
            expectedEclipseDate = mkJulian 2021 11 19 9.0
        Right (nextEclipseType, nextEclipseJD) <- nextLunarEclipseWhen [] SearchForward startTime
        nextEclipseType `shouldBe` PartialLunarEclipse
        julianNoon nextEclipseJD `shouldBe` julianNoon expectedEclipseDate 
 

  beforeAll_ withEphemerides' $
    context "with bundled ephemeris" $ do
      prop "calculates ecliptic coordinates for any of the planets in a wide range of time." $
        forAll genCoordinatesQuery $
          \(time, planet) -> monadicIO $ do
            coords <- run $ calculateEclipticPosition time planet
            assert $ isRight coords
      prop "is unable to calculate coordinates for times before or after the bundled ephemerides" $
        forAll genBadCoordinatesQuery $
          \(time, planet) -> monadicIO $ do
            coords <- run $ calculateEclipticPosition time planet
            assert $ isLeft coords
      describe "crossings" $ do
        describe "sunCrossing" $  
          it "calculates the geocentric solar crossing over a given longitude" $ do
            let libraSeasonStart = mkJulian 2021 9 23 0
                startTime = mkJulian 2021 8 9 0.0
            Right crossingJD <- sunCrossing 180.0 startTime
            julianNoon crossingJD `shouldBe` julianNoon libraSeasonStart

        describe "sunCrossingBetween" $ do 
          it "calculates the geocentric solar crossing over a given longitude in an interval" $ do
            let libraSeasonStart = mkJulian 2021 9 23 0
                startTime = mkJulian 2021 8 9 0.0
                endTime   = mkJulian 2021 9 24 0.0
            Right crossingJD <- sunCrossingBetween 180.0 startTime endTime
            julianNoon crossingJD `shouldBe` julianNoon libraSeasonStart
   
          it "knows when to give up calculating the geocentric solar crossing over a given longitude in an interval" $ do
            let startTime = mkJulian 2021 8 9 0.0
                endTime   = mkJulian 2021 9 22 0.0
            Left e <- sunCrossingBetween 180.0 startTime endTime
            e `shouldBe` "No crossing found in the specified interval."

        describe "moonCrossing" $
          it "calculates the geocentric lunar crossing over a given longitude" $ do
            let startTime = mkJulian 2021 8 9 0.0
                -- 2021-Aug-09 06:56:14.57 UT
                expectedCrossing = 2459435.7890575626
                venusTrine = 265.5868455517535 - 120.0
            Right crossingJD <- moonCrossing venusTrine startTime
            getJulianDay crossingJD `shouldBe` expectedCrossing

        describe "moonCrossingBetween" $ do
          it "calculates the geocentric lunar crossing over a given longitude in an interval" $ do
            let startTime = mkJulian 2021 8 9 0.0
                endTime   = mkJulian 2021 8 10 0.0
                -- 2021-Aug-09 06:56:14.57 UT
                expectedCrossing = 2459435.7890575626
                venusTrine = 265.5868455517535 - 120.0
            Right crossingJD <- moonCrossingBetween venusTrine startTime endTime
            getJulianDay crossingJD `shouldBe` expectedCrossing

          it "knows when to give up when calculating the geocentric lunar crossing over a given longitude in an interval" $ do
            let startTime = mkJulian 2021 8 9 0.0
                endTime   = mkJulian 2021 8 9 2.0
                venusTrine = 265.5868455517535 - 120.0
            Left e <- moonCrossingBetween venusTrine startTime endTime
            e `shouldBe` "No crossing found in the specified interval."

        describe "heliocentricCrossing" $
          it "calculates the heliocentric crossing of a planet over a given longitude" $ do
            let startTime = mkJulian 2021 8 9 0.0
                expectedCrossing = mkJulian 2021 9 4 13.0
                libraLongitude = 180.0
            Right crossingJD <- heliocentricCrossing SearchForward Mars libraLongitude startTime
            -- note that Mars enters libra /earlier/ from a heliocentric
            -- perspective than from a geocentric perspective.
            julianNoon crossingJD `shouldBe` julianNoon expectedCrossing

      describe "changes of direction" $ do
        describe "nextDirectionChange" $
          it "calculates the moment of direction change of a planet after a date" $ do
            let startTime = mkJulian 2021 7 14 0.0
                -- 2021-Jul-15 16:41:02.9 UTC
                expectedCrossing = 2459411.1951724007
                expectedMotion = RetrogradeMotion
            Right (crossingJD, motion) <- nextDirectionChange Chiron startTime
            getJulianDay crossingJD `shouldBe` expectedCrossing
            motion `shouldBe` expectedMotion
        describe "directionChangeBetween" $ do
          it "calculates the moment of direction change if it happens in the interval" $ do
            let startTime = mkJulian 2021 7 15 0.0
                endTime   = mkJulian 2021 7 16 0.0
                -- 2021-Jul-15 16:41:02.9 UTC
                expectedCrossing = 2459411.1951724007
                expectedMotion = RetrogradeMotion
            Right (crossingJD, motion) <- directionChangeBetween Chiron startTime endTime
            getJulianDay crossingJD `shouldBe` expectedCrossing
            motion `shouldBe` expectedMotion

          it "fails to calculate direction change if outside of the interval" $ do
            let startTime = mkJulian 2021 7 14 0.0
                endTime   = mkJulian 2021 7 15 0.0
            Left msg <- directionChangeBetween Chiron startTime endTime
            msg `shouldBe` "swe_next_direction_change: no change within 1.000000 days"
      
      describe "bracketed geocentric crossings" $ do
        describe "crossingBetween" $ do
          it "finds the moment the moon crosses over a given longitude" $ do
            let jd1 = mkJulianDay SUT1 2459449.5
                jd2 = mkJulianDay SUT1 2459450.5
                cross = 341.89809835262577
                -- 2021-Aug-23 09:52:07.39 UTC 
                expectedTime = 2459449.911196674
            Right crossesAt <- crossingBetween Moon cross jd1 jd2
            getJulianDay crossesAt `shouldBe` expectedTime

          it "fails if the given times don't bracket the crossing" $ do
            let jd1 = succ $ mkJulianDay SUT1 2459449.5
                jd2 = succ $ mkJulianDay SUT1 2459450.5
                cross = 341.89809835262577
            Left msg <- crossingBetween Moon cross jd1 jd2
            msg `shouldBe` "swe_interpolate: not bracketed: 2459450.500802 - 2459451.500802"

        describe "bracketed moon phase exactitude" $ do
          it "returns an error if a phase doesn't happen during an interval" $ do
            Left msg <- moonPhaseExactAt NewMoon (mkJulian 2021 9 8 0) (mkJulian 2021 9 10 0)
            msg `shouldBe` "swe_interpolate: not bracketed: 2459465.500802 - 2459467.500802"
            
          it "finds exactitude with tight lower bounds (edge cases for root finding)" $ do
            let a1 = mkJulianDay STT 2459489.7091340744
                a2 = succ a1
                b1 = mkJulianDay STT 2459515.7091340744
                b2 = succ b1
            Right exactA <- moonPhaseExactAt WaningCrescent a1 a2
            Right exactB <- moonPhaseExactAt LastQuarter b1 b2
            getJulianDay exactA `shouldBe` 2459490.4836063166 
            getJulianDay exactB `shouldBe` 2459516.3377326634

          it "finds all moments of exactitude for September 2021 (UTC)" $ do
            let _newMoon@(nmA, nmB) = (mkJulian 2021 9 7 0, mkJulian 2021 9 8 0)
                _waxingCrescent@(wcA, wcB) = (mkJulian 2021 9 10 0, mkJulian 2021 9 11 0)
                _firstQuarter@(fqA, fqB) = (mkJulian 2021 9 13 0, mkJulian 2021 9 14 0)
                _waxingGibbous@(wgA, wgB) = (mkJulian 2021 9 17 0, mkJulian 2021 9 18 0)
                _fullMoon@(fmA, fmB) = (mkJulian 2021 9 20 0, mkJulian 2021 9 21 0)
                _waningGibbous@(gA, gB) = (mkJulian 2021 9 24 0, mkJulian 2021 9 25 0)
                _lastQuarter@(lqA, lqB) = (mkJulian 2021 9 29 0, mkJulian 2021 9 30 0)
                _waningCrescent@(cA, cB) = (mkJulian 2021 10 2 0, mkJulian 2021 10 3 0)
            Right exactNewMoon <- moonPhaseExactAt NewMoon nmA nmB
            Right exactWaxingCrescent <- moonPhaseExactAt WaxingCrescent wcA wcB
            Right exactFirstQuarter <- moonPhaseExactAt FirstQuarter fqA fqB
            Right exactWaxingGibbous <- moonPhaseExactAt WaxingGibbous wgA wgB
            Right exactFullMoon <- moonPhaseExactAt FullMoon fmA fmB
            Right exactWaningGibbous <- moonPhaseExactAt WaningGibbous gA gB
            Right exactLastQuarter <- moonPhaseExactAt LastQuarter lqA lqB
            Right exactWaningCrescent <- moonPhaseExactAt WaningCrescent cA cB
            -- cf: https://ssd.jpl.nasa.gov/tools/jdc/#/jd
            -- 2021-09-07 00:51:46 UTC
            getJulianDay exactNewMoon `shouldBe` 2459464.5359518672
            -- 2021-09-10 11:03:31 UTC
            getJulianDay exactWaxingCrescent `shouldBe` 2459467.9607786587
            -- 2021-09-13 20:39:22 UTC
            getJulianDay exactFirstQuarter `shouldBe` 2459471.3606688627
            -- 2021-09-17 08:18:20 UTC
            getJulianDay exactWaxingGibbous `shouldBe` 2459474.8460611873
            -- 2021-09-20 23:54:42 UTC
            getJulianDay exactFullMoon `shouldBe` 2459478.496323667
            -- 2021-09-24 22:32:33 UTC
            getJulianDay exactWaningGibbous `shouldBe` 2459482.4392759944
            -- 2021-09-29 01:57:09 UTC
            getJulianDay exactLastQuarter `shouldBe` 2459486.5813509836
            -- 2021-10-02 23:35:14 UTC
            getJulianDay exactWaningCrescent `shouldBe` 2459490.482804646




{- For reference, here's an official test output from swetest.c as retrieved from the swetest page:
https://www.astro.com/cgi/swetest.cgi?b=6.1.1989&n=1&s=1&p=p&e=-eswe&f=PlbRS&arg=

/ulb/swetest -b6.1.1989 -n1 -s1 -fPlbRS -pp -eswe
date (dmy) 6.1.1989 greg.   0:00:00 TT		version 2.09.02
UT:  2447532.499348289     delta t: 56.307812 sec
TT:  2447532.500000000
Epsilon (t/m)     23°26'34.3880   23°26'26.5513
Nutation           0° 0' 7.0140    0° 0' 7.8367
Sun              285.6465775  -0.0000826    0.983344873    1° 1'10.7494
Moon             262.4723493  -4.9055744    0.002541081   13°32'20.5748
Mercury          304.3135759  -1.3441786    1.063192008    1°16'26.5499
Venus            264.0478768   0.6114330    1.541580119    1°15' 4.6876
Mars             22.7844912   0.6472527    1.022405583    0°31'25.8280
Jupiter          56.4415899  -0.8785552    4.335636145   -0° 2'55.9909
Saturn           276.1819323   0.7124667   11.011941178    0° 7' 2.5299
Uranus           272.0516769  -0.2200407   20.269984778    0° 3'33.1193
Neptune          280.1110192   0.9024311   31.197895466    0° 2'16.1622
Pluto            224.6817137  15.6296117   30.105043006    0° 1'26.0648
mean Node        337.5235158   0.0000000    0.002569555   -0° 3'10.4503
true Node        336.0940780   0.0000000    0.002430997   -0° 9'16.5193
mean Apogee      176.2778962  -1.6583044    0.002710625    0° 6'39.3460
osc. Apogee      160.7688271  -0.4177243    0.002728008   -3°13'10.0208
intp. Apogee     179.0030402  -2.0103177    0.002703937    0°13'36.2585
intp. Perigee    332.6193255  -0.2979700    0.002455943   -0° 6' 9.7439
Chiron           93.5373174  -6.8493261   11.045970633   -0° 3'50.0890
Pholus           68.5935494 -20.2059152    8.932136793   -0° 3'11.0749
Ceres            356.8293239  -9.1162694    3.076827933    0°16'39.3152
Pallas           320.7627882  12.0659795    4.068428816    0°18'24.9563
Juno             160.5478653  -9.1175408    1.718473253   -0° 1'22.3470
Vesta            238.4983081   5.1734845    2.723062869    0°29'44.2131
-}

-- helpers for approximate equality:

-- from: https://github.com/codewars/hspec-codewars/blob/476f6c0e85b8f0c060a12c8d573ee4b805589fe0/src/Test/Hspec/Codewars.hs

shouldBeApprox :: (Fractional a, Ord a, Show a) => a -> a -> Expectation
shouldBeApprox expected actual =
  if abs (actual - expected) < abs margin * max 1 (abs expected)
    then pure ()
    else expectationFailure msg
  where
    margin = 1e-5
    msg =
      mconcat
        [ "Failure:\n expected: ",
          show actual,
          " to be approximately equal to ",
          show expected
        ]

infix 1 `shouldBeApprox`

compareCoords :: Either String EclipticPosition -> Either String EclipticPosition -> Expectation
compareCoords (Right a) (Right b) = do
  lng a `shouldBeApprox` lng b
  lat a `shouldBeApprox` lat b
  distance a `shouldBeApprox` distance b
  lngSpeed a `shouldBeApprox` lngSpeed b
  latSpeed a `shouldBeApprox` latSpeed b
  distSpeed a `shouldBeApprox` distSpeed b
compareCoords (Left e) _ = expectationFailure $ "Expected coordinates, got: " ++ e
compareCoords _ (Left e) = expectationFailure $ "Expected coordinates, got: " ++ e

compareCalculations :: Either String CuspsCalculation -> Either String CuspsCalculation -> Expectation
compareCalculations (Right (CuspsCalculation housesA anglesA sysA)) (Right (CuspsCalculation housesB anglesB sysB)) = do
  -- compare systems: note that for polar coordinates, it may be switched to Porphyry
  sysA `shouldBe` sysB

  forM_ (zip housesA housesB) $ uncurry shouldBeApprox

  -- angles:
  ascendant anglesA `shouldBeApprox` ascendant anglesB
  mc anglesA `shouldBeApprox` mc anglesB
  armc anglesA `shouldBeApprox` armc anglesB
  vertex anglesA `shouldBeApprox` vertex anglesB
  equatorialAscendant anglesA `shouldBeApprox` equatorialAscendant anglesB
  coAscendantKoch anglesA `shouldBeApprox` coAscendantKoch anglesB
  coAscendantMunkasey anglesA `shouldBeApprox` coAscendantMunkasey anglesB
  polarAscendant anglesA `shouldBeApprox` polarAscendant anglesB
compareCalculations _ _ = expectationFailure "Unable to calculate"

-- | As noted in the readme, the test ephemeris only covers from
-- 1800-Jan-01 AD to 2399-Dec-31
-- the Moshier ephemeris should cover a wider range of years, but
-- they cannot compute Chiron in the general case. 
-- We're choosing a range for which we have continuous Ephemeris for chiron.
-- These numbers were calculated with:
-- https://ssd.jpl.nasa.gov/tc.cgi
-- read more in the manual:
-- https://www.astro.com/swisseph/swephprg.htm
minT :: UTCTime
minT = minTestEpheT 

maxT :: UTCTime
maxT = maxTestEpheT

-- see:
-- https://github.com/aloistr/swisseph/blob/7a9a56f858f8db5128c1e5c0bf1c3bde760a0cb3/sweph.h#L207-L208
-- and:
-- https://github.com/aloistr/swisseph/blob/40a0baa743a7c654f0ae3331c5d9170ca1da6a6a/sweph.c#L1079
-- It seems that even though we don't have /data/ for Chiron beyond
-- `minT` and `maxT` above, some far out dates are still interpolated
-- just fine; so we work with the absolute limits of the library
-- for the negative case, instead.
minChironEphe, maxChironEphe :: UTCTime 
minChironEphe = mkUTC "0675-01-01T00:00:00Z"
maxChironEphe = mkUTC "4650-01-01T00:00:00Z"

genJulian :: Gen JulianDayUT1
genJulian = genJulianInRange minT maxT

genBadJulian :: Gen JulianDayUT1
genBadJulian = oneof [genJulianBefore minChironEphe, genJulianAfter maxChironEphe]

genHouseSystem :: Gen HouseSystem
genHouseSystem = elements [Placidus, Koch, Porphyrius, Regiomontanus, Campanus, Equal, WholeSign]

genCoordinatesQuery :: Gen (JulianDayUT1, Planet)
genCoordinatesQuery = do
  time <- genJulian
  planet <- elements [Sun .. Chiron]
  return (time, planet)

-- only Chiron is reliably outside of our calculations,
-- our ephemerides data does have some other bodies missing though.
genBadCoordinatesQuery :: Gen (JulianDayUT1, Planet)
genBadCoordinatesQuery = do
  time <- genBadJulian
  -- TODO: does the library _really_ misbehave for all bodies, or just Chiron?
  let planet = Chiron
  return (time, planet)

genAnyCoords :: Gen (Double, Double)
genAnyCoords = do
  -- see swehouse.c: for many systems, being _on_ the pole will fail,
  -- even if the system works in the polar circle, nominally.
  anyLat <- choose (-90.0, 90.0)
  anyLong <- choose (-180.0, 180.0)
  return (anyLat, anyLong)

genCuspsQuery :: Gen ((Double, Double), JulianDayUT1, HouseSystem)
genCuspsQuery = do
  coords <- genAnyCoords
  time <- genJulian
  -- Placidus and Koch _sometimes_ succeed, for certain locations, but are more likely to fail.
  -- Regiomontanus and Campanus also struggle to calculate some angles.
  house <- genHouseSystem
  return (coords, time, house)

genCuspsNonPolarQuery :: Gen ((Double, Double), JulianDayUT1, HouseSystem)
genCuspsNonPolarQuery = do
  nonPolarLat <- choose (-40.0, 40.0)
  anyLong <- choose (-180.0, 180.0)
  time <- genJulian
  house <- genHouseSystem
  return ((nonPolarLat, anyLong), time, house)
