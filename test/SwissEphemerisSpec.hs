{-# LANGUAGE OverloadedStrings #-}

module SwissEphemerisSpec (spec) where

import SwissEphemeris
import Test.Hspec
import Control.Monad (forM_)

-- to verify that we're calling things correctly, refer to the swiss ephemeris test page:
-- https://www.astro.com/swisseph/swetest.htm
-- to see the results for this specific test suite, see:
-- https://www.astro.com/cgi/swetest.cgi?b=6.1.1989&n=1&s=1&p=p&e=-eswe&f=PlbRS&arg=
-- note the `PlbRS` format, which outputs latitude and longitude as decimals, not degrees
-- for easier comparison.

spec :: Spec
spec = do
  describe "calculateCoordinates" $ do
    it "calculates coordinates for the Sun for a specific day" $ do
      let time = julianDay 1989 1 6 0.0
      let coords = calculateCoordinates time Sun
      let expectedCoords =
            Right $
              Coordinates
                { lng = 285.64723120365153,
                  lat = -8.664716530514133e-5,
                  distance = 0.9833448914987339,
                  lngSpeed = 1.0196505771457982,
                  latSpeed = 1.4550248863443192e-5,
                  distSpeed = 1.7364210462433863e-5
                }
      compareCoords coords expectedCoords

    it "calculates coordinates for all basic bodies" $ do
      let time = julianDay 1989 1 6 0.0
      let allCoords = map (\p -> (p, calculateCoordinates time p)) [Sun .. Earth]
      let expectedCoords =
            [ (Sun, Right (Coordinates {lat = -8.664716530514133e-5, lng = 285.64723120365153, distance = 0.9833448914987339, lngSpeed = 1.0196505771457982, latSpeed = 1.4550248863443192e-5, distSpeed = 1.7364210462433863e-5})),
              (Moon, Right (Coordinates {lat = -4.905337023892561, lng = 262.4812494369113, distance = 2.541068202676841e-3, lngSpeed = 13.539144530705265, latSpeed = 0.3393248335360281, distSpeed = -3.33641945987241e-5})),
              (Mercury, Right (Coordinates {lat = -1.3440872161262003, lng = 304.3143965950206, distance = 1.063175964159272, lngSpeed = 1.274004420536768, latSpeed = 0.15124323418411553, distSpeed = -2.454427438491084e-2})),
              (Venus, Right (Coordinates {lat = 0.6114024394824853, lng = 264.0486985274232, distance = 1.541582977470953, lngSpeed = 1.2512995742102928, latSpeed = -4.242663256363747e-2, distSpeed = 3.7838654439487605e-3})),
              (Mars, Right (Coordinates {lat = 0.647245858225883, lng = 22.784889069947795, distance = 1.0224117036976277, lngSpeed = 0.523839164808033, latSpeed = 1.9425688922062898e-2, distSpeed = 9.240020614382198e-3})),
              (Jupiter, Right (Coordinates {lat = -0.8785283244410401, lng = 56.44155335587722, distance = 4.335648980677844, lngSpeed = -4.88837863269876e-2, latSpeed = 4.367436333624747e-3, distSpeed = 1.2352162533995464e-2})),
              (Saturn, Right (Coordinates {lat = 0.7124491550388845, lng = 276.1820278995121, distance = 11.01193611882914, lngSpeed = 0.11736895453441915, latSpeed = -9.509664300187428e-4, distSpeed = -2.773473744332959e-3})),
              (Uranus, Right (Coordinates {lat = -0.22004659044807043, lng = 272.0517516959161, distance = 20.269981273926522, lngSpeed = 5.9199668456516044e-2, latSpeed = -1.8350976677191972e-4, distSpeed = -3.83562617273294e-3})),
              (Neptune, Right (Coordinates {lat = 0.9024382125033713, lng = 280.11104006417463, distance = 31.19789619108639, lngSpeed = 3.782283564079936e-2, latSpeed = -1.0376763991092633e-4, distSpeed = -1.6316563374118558e-3})),
              (Pluto, Right (Coordinates {lat = 15.629571509658952, lng = 224.68177283445652, distance = 30.105102148398323, lngSpeed = 2.3906545299026256e-2, latSpeed = 7.006247172893374e-3, distSpeed = -1.4619180699047109e-2})),
              (MeanNode, Right (Coordinates {lat = 0.0, lng = 337.5234813280144, distance = 2.5695552897999894e-3, lngSpeed = -5.29028695601468e-2, latSpeed = 0.0, distSpeed = 0.0})),
              (TrueNode, Right (Coordinates {lat = 0.0, lng = 336.088555837235, distance = 2.4309717049541494e-3, lngSpeed = -0.15531674320876074, latSpeed = 0.0, distSpeed = 9.45488601879409e-6})),
              (MeanApog, Right (Coordinates {lat = -1.658313456511187, lng = 176.27796853267614, distance = 2.7106251317225464e-3, lngSpeed = 0.11092946668995039, latSpeed = -1.3957593707984951e-2, distSpeed = 0.0})),
              (OscuApog, Right (Coordinates {lat = -0.41908446181930853, lng = 160.77844211408353, distance = 2.7280173404211033e-3, lngSpeed = -3.209901508794726, latSpeed = 0.27200216400957794, distSpeed = 4.013939873418364e-6})),
              (Earth, Right (Coordinates {lat = 0.0, lng = 0.0, distance = 0.0, lngSpeed = 0.0, latSpeed = 0.0, distSpeed = 0.0}))
            ]
      
      forM_ (zip allCoords expectedCoords) $ \((planetA, actual), (planetB, expected)) -> do
        planetA `shouldBe` planetB
        actual `compareCoords` expected
   
    it "fails to calculate coordinates for Chiron if no ephemeris file is set" $ do
      let time = julianDay 1989 1 6 0.0
      let coords = calculateCoordinates time Chiron
      let expectedCoords = Left "SwissEph file 'seas_18.se1' not found in PATH '.:/users/ephe2/:/users/ephe/'"
      coords `shouldBe` expectedCoords

  around_ (withEphemerides "./swedist/sweph_18") $ do
    describe "setEphemeridesPath" $ do
      it "calculates more precise coordinates for the Sun if an ephemeris file is set" $ do
        let time = julianDay 1989 1 6 0.0
        let coords = calculateCoordinates time Sun
        let expectedCoords =
              Right $
                Coordinates
                  { lng = 285.64724200024165,
                    lat = -8.254238068673002e-5,
                    distance = 0.983344884137739,
                    lngSpeed = 1.0196526213625938,
                    latSpeed = 1.4968387810319695e-5,
                    distSpeed = 1.734078975098347e-5
                  }
        coords `compareCoords` expectedCoords

      it "calculates coordinates for Chiron if an ephemeris file is set" $ do
        let time = julianDay 1989 1 6 0.0
        let coords = calculateCoordinates time Chiron
        let expectedCoords = Right (Coordinates {lng = 93.53727572747667, lat = -6.849325566420532, distance = 11.045971701732345, lngSpeed = -6.391339610156536e-2, latSpeed = 8.213606290819226e-4, distSpeed = 1.6210560093203594e-3})
        coords `compareCoords` expectedCoords

  describe "calculateCusps" $ do
    it "calculates cusps and angles for a given place and time" $ do
      let time = julianDay 1989 1 6 0.0
      let place = defaultCoordinates {lat = 14.0839053, lng = -87.2750137}
      let expectedCalculations =
            CuspsCalculation
              HouseCusps
                { i = 112.20189657163523,
                  ii = 138.4658382335878,
                  iii = 167.69682489058204,
                  iv = 199.79861981778183,
                  v = 232.2797046698429,
                  vi = 263.0249102802477,
                  vii = 292.20189657163525,
                  viii = 318.46583823358776,
                  ix = 347.69682489058204,
                  x = 19.798619817781823,
                  xi = 52.27970466984291,
                  xii = 83.02491028024768
                }
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

      let calcs = calculateCusps time place Placidus
      calcs `compareCalculations` expectedCalculations

{- For reference, here's the official test output from swetest.c as retrieved from the swetest page:
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
shouldBeApprox actual expected =
  if abs (actual - expected) < abs margin * max 1 (abs expected) then
    pure ()
  else
    expectationFailure msg
  where
    margin = 1e-5
    msg = mconcat [
      "Failure:\n expected: ", show expected,
      "to be approximately equal to", (show actual)]

infix 1 `shouldBeApprox`

compareCoords :: Either String Coordinates -> Either String Coordinates -> Expectation
compareCoords (Right a) (Right b) = do
   lng a `shouldBeApprox` lng b
   lat a `shouldBeApprox` lat b
   distance a `shouldBeApprox` distance b
   lngSpeed a `shouldBeApprox` lngSpeed b
   latSpeed a `shouldBeApprox` latSpeed b
   distSpeed a `shouldBeApprox` distSpeed b
compareCoords (Left e) _ = expectationFailure $ "Expected coordinates, got: " ++ e
compareCoords _ (Left e) = expectationFailure $ "Expected coordinates, got: " ++ e

compareCalculations :: CuspsCalculation -> CuspsCalculation -> Expectation
compareCalculations (CuspsCalculation housesA anglesA) (CuspsCalculation housesB anglesB) = do
  i housesA `shouldBeApprox` i housesB
  ii housesA `shouldBeApprox` ii housesB
  iii housesA `shouldBeApprox` iii housesB
  iv housesA `shouldBeApprox` iv housesB
  v housesA `shouldBeApprox` v housesB
  vi housesA `shouldBeApprox` vi housesB
  vii housesA `shouldBeApprox` vii housesB
  viii housesA `shouldBeApprox` viii housesB
  ix housesA `shouldBeApprox` ix housesB
  x housesA `shouldBeApprox` x housesB
  xi housesA `shouldBeApprox` xi housesB
  xii housesA `shouldBeApprox` xii housesB

  -- angles:
  ascendant anglesA `shouldBeApprox` ascendant anglesB
  mc anglesA `shouldBeApprox` mc anglesB
  armc anglesA `shouldBeApprox` armc anglesB
  vertex anglesA `shouldBeApprox` vertex anglesB
  equatorialAscendant anglesA `shouldBeApprox` equatorialAscendant anglesB
  coAscendantKoch anglesA `shouldBeApprox` coAscendantKoch anglesB
  coAscendantMunkasey anglesA `shouldBeApprox` coAscendantMunkasey anglesB
  polarAscendant anglesA `shouldBeApprox` polarAscendant anglesB
