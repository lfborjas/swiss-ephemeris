{-# LANGUAGE NamedFieldPuns #-}
{- | 
Module: SwissEphemeris.ChartUtils
Description: Utility functions for chart drawing.
functionality.
License: AGPL-3
Maintainer: swiss-ephemeris@lfborjas.com
Portability: POSIX
-}

module SwissEphemeris.ChartUtils where

import Foreign
import Foreign.C.String
import Foreign.SwissEphemeris
import Foreign.SwissEphemerisExtras
import SwissEphemeris.Internal
import Control.Category ((>>>))
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)
import Data.List

type PlanetGlyph = GravityObject Planet

data GlyphInfo a = GlyphInfo
  { placedPosition :: Double
  , sectorNumber :: Int
  , sequenceNumber :: Int
  , levelNumber :: Int
  , glyphScale :: Double
  , extraData  :: a
  } deriving Show
  
type PlanetGlyphInfo = GlyphInfo Planet

glyphPlanet :: PlanetGlyphInfo -> Planet
glyphPlanet = extraData

deg2cs :: Double
deg2cs = 360000.0

degreeToCentiseconds :: Double -> Int
degreeToCentiseconds = (* deg2cs) >>> round

centisecondsToDegree :: Int -> Double
centisecondsToDegree = realToFrac >>> (/ deg2cs)

planetCmp :: Planet -> Planet -> Ordering
planetCmp a b =
  compare (fromEnum a) (fromEnum b)

gravGroup 
  :: (Double, Double) 
  -- ^ lwidth, rwidth
  -> [(Planet, EclipticPosition)]
  -- ^ list of pre-calculated positions
  -> [Double]
  -- ^ list of "sectors" (e.g. house cusps)
  -> Either String [(EclipticPosition, PlanetGlyphInfo)]
gravGroup sz positions sectors =
  unsafePerformIO $ do
    withArray (map (planetPositionToGlyph sz) positions) $ \grobs ->
      withArray (map (fromIntegral . degreeToCentiseconds) sectors) $ \sbdy ->
        allocaArray 256 $ \serr -> do
          let nob = fromIntegral $ length positions
              nsectors = fromIntegral $ (length sectors) - 1
          retval <-
            c_grav_group grobs nob sbdy nsectors serr

          if retval < 0 then do
            msg <- peekCAString serr
            pure $ Left msg
          else do
            repositioned <- peekArray (fromIntegral  nob) grobs
            let repositionedInfo = map glyphInfo repositioned
                sortedInfo = sortBy (\a b -> planetCmp (glyphPlanet a) (glyphPlanet b)) repositionedInfo
                sortedOriginal = sortBy (\a b -> planetCmp (fst a) (fst b)) positions
            pure $ Right $ zipWith (\(_p, pos) glyph -> (pos, glyph)) sortedOriginal sortedInfo


-- | Given dimensions, and a @Planet@ and @EclipticPosition@ pair,
-- produce a "glyph" object suitable for the @grav_group@ functions.
planetPositionToGlyph :: (Double, Double) -> (Planet, EclipticPosition) -> PlanetGlyph
planetPositionToGlyph (lwidth, rwidth) (planet, EclipticPosition {lng}) = unsafePerformIO $ do
  alloca $ \planetPtr -> do
    poke planetPtr planet
    pure $
      GravityObject {
        pos = fromIntegral . degreeToCentiseconds $ lng
      , lsize = fromIntegral . degreeToCentiseconds $ lwidth
      , rsize = fromIntegral . degreeToCentiseconds $ rwidth
      -- fields that will be initialized by the functions
      , ppos = 0
      , sector_no = 0
      , sequence_no = 0
      , level_no = 0
      , scale = 0.0
      , dp = planetPtr
      }
      
-- | Convenience version of @planetPositionToGlyph@ for glyphs that are symmetrical
squarePlanetPositionToGlyph :: Double -> (Planet, EclipticPosition) -> PlanetGlyph
squarePlanetPositionToGlyph width = planetPositionToGlyph (width/2, width/2) 
      
glyphInfo :: PlanetGlyph -> PlanetGlyphInfo
glyphInfo GravityObject{ppos,sector_no,sequence_no, level_no, scale, dp} = unsafePerformIO $ do
  planet' <- peek dp
  pure $ 
    GlyphInfo {
      placedPosition = centisecondsToDegree $ fromIntegral ppos
    , sectorNumber = fromIntegral  sector_no
    , sequenceNumber = fromIntegral sequence_no
    , levelNumber = fromIntegral level_no
    , glyphScale = realToFrac scale
    , extraData = planet'
    }

{-
let sz = (2,2)
let ps = [(Mars, defaultPos{lng=56.6}), (Venus, defaultPos{lng=56.0})]

-}
