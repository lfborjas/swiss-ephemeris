module ChartUtilsSpec (spec) where

import Data.Bifunctor
import SwissEphemeris
import SwissEphemeris.ChartUtils
import Test.Hspec
import Data.Either (fromRight)

defaultPosition :: EclipticPosition
defaultPosition = EclipticPosition 0.0 0.0 0.0 0.0 0.0 0.0

examplePositions :: [(Planet, EclipticPosition)]
examplePositions =
  [ (Sun, defaultPosition {lng = 285.64}),
    (Moon, defaultPosition {lng = 262.47}),
    (Mercury, defaultPosition {lng = 304.31}),
    (Venus, defaultPosition {lng = 264.04}),
    (Mars, defaultPosition {lng = 22.78}),
    (Jupiter, defaultPosition {lng = 56.44}),
    (Saturn, defaultPosition {lng = 276.18}),
    (Uranus, defaultPosition {lng = 272.05}),
    (Neptune, defaultPosition {lng = 280.11}),
    (Pluto, defaultPosition {lng = 224.68}),
    (MeanNode, defaultPosition {lng = 337.52}),
    (MeanApog, defaultPosition {lng = 176.41}),
    (Chiron, defaultPosition {lng = 93.53})
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

spec :: Spec
spec = do
  describe "gravGroupEasy" $ do
    it "returns planets in corrected positions, when applicable" $ do
      let grouped = fromRight [] $ gravGroupEasy 5.0 examplePositions exampleCusps
          redux = map (first lng) grouped

      redux
        `shouldBe` [ (285.64, GlyphInfo {placedPosition = 285.64, sectorNumber = 9, sequenceNumber = 0, levelNumber = 0, glyphScale = 1.0, extraData = Sun}),
                     (262.47, GlyphInfo {placedPosition = 258.64011666666664, sectorNumber = 8, sequenceNumber = 1, levelNumber = 0, glyphScale = 1.0, extraData = Moon}),
                     (304.31, GlyphInfo {placedPosition = 304.31, sectorNumber = 9, sequenceNumber = 2, levelNumber = 0, glyphScale = 1.0, extraData = Mercury}),
                     (264.04, GlyphInfo {placedPosition = 263.64011666666664, sectorNumber = 8, sequenceNumber = 3, levelNumber = 0, glyphScale = 1.0, extraData = Venus}),
                     (22.78, GlyphInfo {placedPosition = 22.78, sectorNumber = 0, sequenceNumber = 4, levelNumber = 0, glyphScale = 1.0, extraData = Mars}),
                     (56.44, GlyphInfo {placedPosition = 56.44, sectorNumber = 1, sequenceNumber = 5, levelNumber = 0, glyphScale = 1.0, extraData = Jupiter}),
                     (276.18, GlyphInfo {placedPosition = 273.64011666666664, sectorNumber = 8, sequenceNumber = 6, levelNumber = 0, glyphScale = 1.0, extraData = Saturn}),
                     (272.05, GlyphInfo {placedPosition = 268.64011666666664, sectorNumber = 8, sequenceNumber = 7, levelNumber = 0, glyphScale = 1.0, extraData = Uranus}),
                     (280.11, GlyphInfo {placedPosition = 278.64011666666664, sectorNumber = 8, sequenceNumber = 8, levelNumber = 0, glyphScale = 1.0, extraData = Neptune}),
                     (224.68, GlyphInfo {placedPosition = 224.68, sectorNumber = 7, sequenceNumber = 9, levelNumber = 0, glyphScale = 1.0, extraData = Pluto}),
                     (337.52, GlyphInfo {placedPosition = 337.52, sectorNumber = 10, sequenceNumber = 10, levelNumber = 0, glyphScale = 1.0, extraData = MeanNode}),
                     (176.41, GlyphInfo {placedPosition = 176.41, sectorNumber = 5, sequenceNumber = 11, levelNumber = 0, glyphScale = 1.0, extraData = MeanApog}),
                     (93.53, GlyphInfo {placedPosition = 93.53, sectorNumber = 2, sequenceNumber = 12, levelNumber = 0, glyphScale = 1.0, extraData = Chiron})
                   ]
  describe "gravGroup2Easy" $ do
    it "returns planets in corrected positions, when applicable" $ do
      let grouped = fromRight [] $ gravGroup2Easy 5.0 examplePositions exampleCusps True
          redux = map (first lng) grouped

      redux
        `shouldBe` [ (285.64, GlyphInfo {placedPosition = 285.64, sectorNumber = 9, sequenceNumber = 0, levelNumber = 0, glyphScale = 1.0, extraData = Sun}),
                     (262.47, GlyphInfo {placedPosition = 262.47, sectorNumber = 8, sequenceNumber = 1, levelNumber = 0, glyphScale = 1.0, extraData = Moon}),
                     (304.31, GlyphInfo {placedPosition = 304.31, sectorNumber = 9, sequenceNumber = 2, levelNumber = 0, glyphScale = 1.0, extraData = Mercury}),
                     (264.04, GlyphInfo {placedPosition = 264.04, sectorNumber = 8, sequenceNumber = 3, levelNumber = 1, glyphScale = 1.0, extraData = Venus}),
                     (22.78, GlyphInfo {placedPosition = 22.78, sectorNumber = 0, sequenceNumber = 4, levelNumber = 0, glyphScale = 1.0, extraData = Mars}),
                     (56.44, GlyphInfo {placedPosition = 56.44, sectorNumber = 1, sequenceNumber = 5, levelNumber = 0, glyphScale = 1.0, extraData = Jupiter}),
                     (276.18, GlyphInfo {placedPosition = 276.18, sectorNumber = 8, sequenceNumber = 6, levelNumber = 1, glyphScale = 1.0, extraData = Saturn}),
                     (272.05, GlyphInfo {placedPosition = 272.05, sectorNumber = 8, sequenceNumber = 7, levelNumber = 0, glyphScale = 1.0, extraData = Uranus}),
                     (280.11, GlyphInfo {placedPosition = 278.64011666666664, sectorNumber = 8, sequenceNumber = 8, levelNumber = 0, glyphScale = 1.0, extraData = Neptune}),
                     (224.68, GlyphInfo {placedPosition = 224.68, sectorNumber = 7, sequenceNumber = 9, levelNumber = 0, glyphScale = 1.0, extraData = Pluto}),
                     (337.52, GlyphInfo {placedPosition = 337.52, sectorNumber = 10, sequenceNumber = 10, levelNumber = 0, glyphScale = 1.0, extraData = MeanNode}),
                     (176.41, GlyphInfo {placedPosition = 176.41, sectorNumber = 5, sequenceNumber = 11, levelNumber = 0, glyphScale = 1.0, extraData = MeanApog}),
                     (93.53, GlyphInfo {placedPosition = 93.53, sectorNumber = 2, sequenceNumber = 12, levelNumber = 0, glyphScale = 1.0, extraData = Chiron})
                   ]

{-
IDEAS FOR PROPS
* preserves original sequence number
* preserves original sector
* doesn't situate beyond 360
-}
