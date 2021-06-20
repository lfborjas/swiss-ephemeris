{-# LANGUAGE NamedFieldPuns #-}
{- | 
Module: SwissEphemeris.ChartUtils
Description: Utility functions for chart drawing.
functionality. Uses the C code shared by the swiss ephemeris authors in the official
mailing list: https://groups.io/g/swisseph/message/5568
License: AGPL-3
Maintainer: swiss-ephemeris@lfborjas.com
Portability: POSIX
-}

module SwissEphemeris.ChartUtils (
  GlyphInfo(..),
  PlanetGlyphInfo,
  glyphPlanet,
  cuspsToSectors,
  gravGroup,
  gravGroupEasy,
  gravGroup2,
  gravGroup2Easy
)
where

import Foreign
import Foreign.C.String
import Foreign.SwissEphemerisExtras
import SwissEphemeris.Internal
import Control.Category ((>>>))
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

-- | This function does a little bit of insider trading:
-- given N cusps, returns N+1 sectors; where the last
-- sector is an "impossible" position beyond 360, that
-- sets the end of the last sector as the first sector's beginning,
-- beyond one turn. That way, any body occurring in
-- the last sector will exist between sectors[N-1] and
-- sectors[N]. I've been using this as the "linearization"
-- approach for the sectors required by @gravGroup@,
-- but one may choose something different.
cuspsToSectors :: [HouseCusp] -> [Double]
cuspsToSectors cusps =
  sortedCusps ++ [head sortedCusps + 360.0]
  where
    sortedCusps = sort cusps

-- | Given dimensions, planet positions and "sectors" within which
-- the planets are meant to be drawn as glyphs, return a list
-- pairing each position with a @PlanetGlyphInfo@ that not only
-- remembers the position's planet, it's guaranteed to place it
-- in the same sector and sequence it started in, but without colliding
-- with other nearby planets or sector boundaries.
--
-- Note that "sectors" are usually cusps, but one must take that they're
-- sorted or "linearized": no sector should jump over 0/360, and the
-- last sector should mark the "end" of the circle. I use @cuspsToSectors@
-- on cusps obtained from the main module's cusp calculation functionality
-- and that seems to ensure that sectors are adequately monotonic and not
-- truncated, but one would be wise to take heed to the swiss ephemeris author's
-- notes, too:
-- https://groups.io/g/swisseph/message/5568
gravGroup
  :: (Double, Double)
  -- ^ lwidth, rwidth
  -> [(Planet, EclipticPosition)]
  -- ^ list of pre-calculated positions
  -> [Double]
  -- ^ list of "sectors" (e.g. house cusps + end of last cusp)
  -> Either String [(EclipticPosition, PlanetGlyphInfo)]
gravGroup sz positions sectors =
  unsafePerformIO $ do
    withArray (map (planetPositionToGlyph sz) positions) $ \grobs ->
      withArray (map (fromIntegral . degreeToCentiseconds) sectors) $ \sbdy ->
        allocaArray 256 $ \serr -> do
          let nob = fromIntegral $ length positions
              nsectors = fromIntegral $ length sectors - 1
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

-- "Easy" version of @gravGroup@ that assumes:
-- * Glyphs are square/symmetrical, so the left and right widths
-- are just half of the provided width, each.
-- * The provided cusps can be "linearized" by the naïve approach of @cuspsToSectors@
gravGroupEasy :: Double
  -> [(Planet, EclipticPosition)]
  -> [HouseCusp]
  -> Either String [(EclipticPosition, PlanetGlyphInfo)]
gravGroupEasy w ps s = gravGroup (w/2,w/2) ps (cuspsToSectors s)

-- | Same semantics and warnings as @gravGroup@, but allows a couple of things for
-- more advanced (or crowded) applications:
-- * Can send an empty list of sectors, to indicate that there's no subdivisions
-- in the circle.
-- * Can specify if planets can be pushed to an "inner" level if they're too
-- crowded in their assigned sector. Useful when drawing several objects in a
-- chart with many/tight sectors.
-- With a non-empty list of sectors, and not allowing shifting, this is essentially
-- a slightly slower version of @gravGroup@.
gravGroup2
  :: (Double, Double)
  -- ^ lwidth, rwidth
  -> [(Planet, EclipticPosition)]
  -- ^ list of pre-calculated positions
  -> [Double]
  -- ^ list of "sectors" (e.g. house cusps + end of last cusp)
  -- (can be empty, indicating that we're working in a non-subdivided circle.)
  -> Bool
  -- ^ allow planets to be moved up or down a level?
  -> Either String [(EclipticPosition, PlanetGlyphInfo)]
gravGroup2 sz positions sectors allowShift =
  unsafePerformIO $ do
    withArray (map (planetPositionToGlyph sz) positions) $ \grobs ->
      withArray (map (fromIntegral . degreeToCentiseconds) sectors) $ \sbdy ->
        allocaArray 256 $ \serr -> do
          let nob = fromIntegral $ length positions
              nsectors = fromIntegral $ length sectors - 1
              mayShift = fromBool allowShift
          retval <-
            c_grav_group2 grobs nob sbdy nsectors mayShift serr

          if retval < 0 then do
            msg <- peekCAString serr
            pure $ Left msg
          else do
            repositioned <- peekArray (fromIntegral  nob) grobs
            let repositionedInfo = map glyphInfo repositioned
                sortedInfo = sortBy (\a b -> planetCmp (glyphPlanet a) (glyphPlanet b)) repositionedInfo
                sortedOriginal = sortBy (\a b -> planetCmp (fst a) (fst b)) positions
            pure $ Right $ zipWith (\(_p, pos) glyph -> (pos, glyph)) sortedOriginal sortedInfo


-- | "Easy" version of @gravGroup2@, same provisions as @gravGroupEasy@
gravGroup2Easy :: Double
  -> [(Planet, EclipticPosition)]
  -> [HouseCusp]
  -> Bool
  -> Either String [(EclipticPosition, PlanetGlyphInfo)]
gravGroup2Easy w ps s = gravGroup2 (w/2, w/2) ps (cuspsToSectors s)

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
      -- store a pointer to the planet enum (stored as an int)
      -- as the "extra data" -- this allows us to remember which
      -- planet this is, without having to schlep around the entire
      -- @EclipticPosition@
      , dp = planetPtr
      }

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

deg2cs :: Double
deg2cs = 360000.0

degreeToCentiseconds :: Double -> Int
degreeToCentiseconds = (* deg2cs) >>> round

centisecondsToDegree :: Int -> Double
centisecondsToDegree = realToFrac >>> (/ deg2cs)

planetCmp :: Planet -> Planet -> Ordering
planetCmp a b =
  compare (fromEnum a) (fromEnum b)


{-
let sz = (2,2)
let ps = [(Mars, defaultPos{lng=56.6}), (Venus, defaultPos{lng=56.0})]

-}
