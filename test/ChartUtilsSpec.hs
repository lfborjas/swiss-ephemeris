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
    it "returns planets in corrected positions, when applicable" $ do
      let grouped = fromRight [] $ gravGroupEasy 5.0 examplePositions exampleCusps
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

  describe "gravGroup2Easy" $ do
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
