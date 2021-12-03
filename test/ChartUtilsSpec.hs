module ChartUtilsSpec (spec) where

import Data.Either (fromRight)
import Data.List (sortBy)
import SwissEphemeris
import SwissEphemeris.ChartUtils
import Test.Hspec

newtype Lng = Lng Double
  deriving (Eq)

instance HasEclipticLongitude Lng where
  getEclipticLongitude (Lng d) = d
  setEclipticLongitude (Lng _) d' = Lng d'

examplePositions :: [(Planet, Lng)]
examplePositions =
  [ (Sun, Lng 285.64),
    (Moon, Lng 262.47),
    (Mercury, Lng 304.31),
    (Venus, Lng 264.04),
    (Mars, Lng 22.78),
    (Jupiter, Lng 56.44),
    (Saturn, Lng 276.18),
    (Uranus, Lng 272.05),
    (Neptune, Lng 280.11),
    (Pluto, Lng 224.68),
    (MeanNode, Lng 337.52),
    (MeanApog, Lng 176.41),
    (Chiron, Lng 93.53)
  ]

exampleCusps :: [HouseCusp]
exampleCusps =
  [ 191.89048504832346, -- 7
    221.91413165410415, -- 8
    251.683593357439, -- 9
    281.1401159871351, -- 10
    311.16006908081926, -- 11
    341.89809835262577, -- 12
    11.890485048323455, -- 1
    41.91413165410415, -- 2
    71.68359335743901, -- 3
    101.14011598713512, -- 4
    131.16006908081926, -- 5
    161.89809835262577 -- 6
  ]

sectorCompare :: GlyphInfo Planet -> GlyphInfo Planet -> Ordering
sectorCompare a b = compare (sectorNumber a) (sectorNumber b)

spec :: Spec
spec = do
  describe "gravGroupEasy" $ do
    it "rejects empty sectors" $ do
      let positions = [(Mars, Lng 274.5), (Venus, Lng 275.0)]
          sectors = []
          grouped = gravGroupEasy 5.0 positions sectors
      grouped `shouldBe` Left "grav_group: 0 sectors."

    it "downscales glyphs in narrow sectors" $ do
      -- [{pos: 274.5, lsize:2.5, rsize: 2.5, dp: "Mars"}, {pos: 275.0, lsize: 2.5, rsize: 2.5, dp: "Venus"}];
      let positions = [(Mars, Lng 274.5), (Venus, Lng 275.0)]
          sectors = [270.0, 274.0, 280.0]
          grouped = fromRight [] $ gravGroupEasy 5.0 positions sectors
      grouped
        `shouldBe` [ GlyphInfo {originalPosition = 274.5, glyphSize = (2.5, 2.5), placedPosition = 275.5, sectorNumber = 1, sequenceNumber = 0, levelNumber = 0, glyphScale = 0.6, extraData = Mars},
                     GlyphInfo {originalPosition = 275.0, glyphSize = (2.5, 2.5), placedPosition = 278.5, sectorNumber = 1, sequenceNumber = 1, levelNumber = 0, glyphScale = 0.6, extraData = Venus}
                   ]

    it "returns planets in corrected positions, when applicable" $ do
      let grouped = fromRight [] $ gravGroupEasy 5.0 examplePositions exampleCusps
          redux = sortBy sectorCompare grouped
      redux
        `shouldBe` [
                    GlyphInfo {originalPosition = 224.68, glyphSize = (2.5, 2.5), placedPosition = 224.68, sectorNumber = 1, sequenceNumber = 9, levelNumber = 0, glyphScale = 1.0, extraData = Pluto},
                    GlyphInfo {originalPosition = 262.47, glyphSize = (2.5, 2.5), placedPosition = 258.6401159871351, sectorNumber = 2, sequenceNumber = 1, levelNumber = 0, glyphScale = 1.0, extraData = Moon},
                    GlyphInfo {originalPosition = 264.04, glyphSize = (2.5, 2.5), placedPosition = 263.6401159871351, sectorNumber = 2, sequenceNumber = 3, levelNumber = 0, glyphScale = 1.0, extraData = Venus},
                    GlyphInfo {originalPosition = 272.05, glyphSize = (2.5, 2.5), placedPosition = 268.6401159871351, sectorNumber = 2, sequenceNumber = 7, levelNumber = 0, glyphScale = 1.0, extraData = Uranus},
                    GlyphInfo {originalPosition = 276.18, glyphSize = (2.5, 2.5), placedPosition = 273.6401159871351, sectorNumber = 2, sequenceNumber = 6, levelNumber = 0, glyphScale = 1.0, extraData = Saturn},
                    GlyphInfo {originalPosition = 280.11, glyphSize = (2.5, 2.5), placedPosition = 278.6401159871351, sectorNumber = 2, sequenceNumber = 8, levelNumber = 0, glyphScale = 1.0, extraData = Neptune},
                    GlyphInfo {originalPosition = 285.64, glyphSize = (2.5, 2.5), placedPosition = 285.64, sectorNumber = 3, sequenceNumber = 0, levelNumber = 0, glyphScale = 1.0, extraData = Sun},
                    GlyphInfo {originalPosition = 304.31, glyphSize = (2.5, 2.5), placedPosition = 304.31, sectorNumber = 3, sequenceNumber = 2, levelNumber = 0, glyphScale = 1.0, extraData = Mercury},
                    GlyphInfo {originalPosition = 337.52, glyphSize = (2.5, 2.5), placedPosition = 337.52, sectorNumber = 4, sequenceNumber = 10, levelNumber = 0, glyphScale = 1.0, extraData = MeanNode},
                    GlyphInfo {originalPosition = 22.779999999999973, glyphSize = (2.5, 2.5), placedPosition = 22.779999999999973, sectorNumber = 6, sequenceNumber = 4, levelNumber = 0, glyphScale = 1.0, extraData = Mars},
                    GlyphInfo {originalPosition = 56.44, glyphSize = (2.5, 2.5), placedPosition = 56.44, sectorNumber = 7, sequenceNumber = 5, levelNumber = 0, glyphScale = 1.0, extraData = Jupiter},
                    GlyphInfo {originalPosition = 93.53000000000003, glyphSize = (2.5, 2.5), placedPosition = 93.53000000000003, sectorNumber = 8, sequenceNumber = 12, levelNumber = 0, glyphScale = 1.0, extraData = Chiron},
                    GlyphInfo {originalPosition = 176.41000000000008, glyphSize = (2.5, 2.5), placedPosition = 176.41000000000008, sectorNumber = 11, sequenceNumber = 11, levelNumber = 0, glyphScale = 1.0, extraData = MeanApog}
                   ]

    it "can deal with sectors that jump 360" $ do
      let planets = [(Uranus, Lng 41.685460865149885), (Chiron, Lng 8.560852515243027)]
          sectors = [355.2817671250407, 26.407082565767553, 57.62582859633026]
          grouped = fromRight [] $ gravGroupEasy 6 planets sectors
      grouped `shouldBe` [
                          GlyphInfo {originalPosition = 8.560852515243027, glyphSize = (3.0,3.0), placedPosition = 8.560852515243027, sectorNumber = 0, sequenceNumber = 1, levelNumber = 0, glyphScale = 1.0, extraData = Chiron},
                          GlyphInfo {originalPosition = 41.68546086514988, glyphSize = (3.0,3.0), placedPosition = 41.68546086514988, sectorNumber = 1, sequenceNumber = 0, levelNumber = 0, glyphScale = 1.0, extraData = Uranus}
                         ]


  describe "gravGroup2Easy" $ do
    it "accepts empty sectors" $ do
      let positions = [(Mars, Lng 274.5), (Venus, Lng 275.0)]
          sectors = []
          grouped = fromRight [] $ gravGroup2Easy 5.0 positions sectors True
      grouped
        `shouldBe` [
                     GlyphInfo {originalPosition = 274.5, glyphSize = (2.5, 2.5), placedPosition = 272.25, sectorNumber = 0, sequenceNumber = 0, levelNumber = 0, glyphScale = 1.0, extraData = Mars},
                     GlyphInfo {originalPosition = 275.0, glyphSize = (2.5, 2.5), placedPosition = 277.25, sectorNumber = 0, sequenceNumber = 1, levelNumber = 0, glyphScale = 1.0, extraData = Venus}
                   ]

    it "shifts glyphs in narrow sectors to different levels, keeps the scale" $ do
      -- [{pos: 274.5, lsize:2.5, rsize: 2.5, dp: "Mars"}, {pos: 275.0, lsize: 2.5, rsize: 2.5, dp: "Venus"}];
      let positions = [(Mars, Lng 274.5), (Venus, Lng 275.0)]
          sectors = [270.0, 274.0, 280.0]
          grouped = fromRight [] $ gravGroup2Easy 5.0 positions sectors True
      grouped
        `shouldBe` [ 
                     GlyphInfo {originalPosition = 274.5, glyphSize = (2.5, 2.5), placedPosition = 276.5, sectorNumber = 1, sequenceNumber = 0, levelNumber = 0, glyphScale = 1.0, extraData = Mars},
                     GlyphInfo {originalPosition = 275.0, glyphSize = (2.5, 2.5), placedPosition = 276.5, sectorNumber = 1, sequenceNumber = 1, levelNumber = 1, glyphScale = 1.0, extraData = Venus}
                   ]

    it "returns planets in corrected positions, when applicable" $ do
      let grouped = fromRight [] $ gravGroup2Easy 5.0 examplePositions exampleCusps True
          redux = sortBy sectorCompare grouped

      redux
        `shouldBe` [ GlyphInfo {originalPosition = 22.78, glyphSize = (2.5, 2.5), placedPosition = 22.78, sectorNumber = 0, sequenceNumber = 4, levelNumber = 0, glyphScale = 1.0, extraData = Mars},
                     GlyphInfo {originalPosition = 56.44, glyphSize = (2.5, 2.5), placedPosition = 56.44, sectorNumber = 1, sequenceNumber = 5, levelNumber = 0, glyphScale = 1.0, extraData = Jupiter},
                     GlyphInfo {originalPosition = 93.53, glyphSize = (2.5, 2.5), placedPosition = 93.53, sectorNumber = 2, sequenceNumber = 12, levelNumber = 0, glyphScale = 1.0, extraData = Chiron},
                     GlyphInfo {originalPosition = 176.41, glyphSize = (2.5, 2.5), placedPosition = 176.41, sectorNumber = 5, sequenceNumber = 11, levelNumber = 0, glyphScale = 1.0, extraData = MeanApog},
                     GlyphInfo {originalPosition = 224.68, glyphSize = (2.5, 2.5), placedPosition = 224.68, sectorNumber = 7, sequenceNumber = 9, levelNumber = 0, glyphScale = 1.0, extraData = Pluto},
                     GlyphInfo {originalPosition = 262.47, glyphSize = (2.5, 2.5), placedPosition = 258.6401159871351, sectorNumber = 8, sequenceNumber = 1, levelNumber = 0, glyphScale = 1.0, extraData = Moon},
                     GlyphInfo {originalPosition = 264.04, glyphSize = (2.5, 2.5), placedPosition = 263.6401159871351, sectorNumber = 8, sequenceNumber = 3, levelNumber = 0, glyphScale = 1.0, extraData = Venus},
                     GlyphInfo {originalPosition = 272.05, glyphSize = (2.5, 2.5), placedPosition = 268.6401159871351, sectorNumber = 8, sequenceNumber = 7, levelNumber = 0, glyphScale = 1.0, extraData = Uranus},
                     GlyphInfo {originalPosition = 276.18, glyphSize = (2.5, 2.5), placedPosition = 273.6401159871351, sectorNumber = 8, sequenceNumber = 6, levelNumber = 0, glyphScale = 1.0, extraData = Saturn},
                     GlyphInfo {originalPosition = 280.11, glyphSize = (2.5, 2.5), placedPosition = 278.6401159871351, sectorNumber = 8, sequenceNumber = 8, levelNumber = 0, glyphScale = 1.0, extraData = Neptune},
                     GlyphInfo {originalPosition = 285.64, glyphSize = (2.5, 2.5), placedPosition = 285.64, sectorNumber = 9, sequenceNumber = 0, levelNumber = 0, glyphScale = 1.0, extraData = Sun},
                     GlyphInfo {originalPosition = 304.31, glyphSize = (2.5, 2.5), placedPosition = 304.31, sectorNumber = 9, sequenceNumber = 2, levelNumber = 0, glyphScale = 1.0, extraData = Mercury},
                     GlyphInfo {originalPosition = 337.52, glyphSize = (2.5, 2.5), placedPosition = 337.52, sectorNumber = 10, sequenceNumber = 10, levelNumber = 0, glyphScale = 1.0, extraData = MeanNode}
                   ]

{-
IDEAS FOR PROPS
* preserves original sequence number
* preserves original sector
* doesn't situate beyond 360
-}
