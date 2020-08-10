{-# LANGUAGE OverloadedStrings #-}

module SwissEphemerisSpec (spec) where

import SwissEphemeris
import System.Directory (makeAbsolute)
import Test.Hspec

-- to verify that we're calling things correctly, refer to the swiss ephemeris test page:
-- https://www.astro.com/swisseph/swetest.htm
-- to see the results for this specific test suite, see:
-- https://www.astro.com/cgi/swetest.cgi?b=6.1.1989&n=1&s=1&p=p&e=-eswe&f=PlbRS&arg=
-- note the `PlbRS` format, which outputs latitude and longitude as decimals, not degrees
-- for easier comparison.

setRelativeEphePath :: FilePath -> IO ()
setRelativeEphePath relativePath = do
  absolutePath <- makeAbsolute relativePath
  setEphemeridesPath absolutePath

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
      coords `shouldBe` expectedCoords

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
      allCoords `shouldBe` expectedCoords
    it "fails to calculate coordinates for Chiron if no ephemeris file is set" $ do
      let time = julianDay 1989 1 6 0.0
      let coords = calculateCoordinates time Chiron
      let expectedCoords = Left "SwissEph file 'seas_18.se1' not found in PATH '.:/users/ephe2/:/users/ephe/'"
      coords `shouldBe` expectedCoords

  beforeAll_ (setRelativeEphePath "swedist/sweph_18") $ do
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
        coords `shouldBe` expectedCoords

      it "calculates coordinates for Chiron if an ephemeris file is set" $ do
        let time = julianDay 1989 1 6 0.0
        let coords = calculateCoordinates time Chiron
        let expectedCoords = Right (Coordinates {lng = 93.53727572747667, lat = -6.849325566420532, distance = 11.045971701732345, lngSpeed = -6.391339610156536e-2, latSpeed = 8.213606290819226e-4, distSpeed = 1.6210560093203594e-3})
        coords `shouldBe` expectedCoords

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
      calcs `shouldBe` expectedCalculations
