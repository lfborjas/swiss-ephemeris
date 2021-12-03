{-# LANGUAGE NamedFieldPuns #-}
-- | 
-- Module: SwissEphemeris.ChartUtils
-- License: AGPL-3
-- Maintainer: swiss-ephemeris@lfborjas.com
-- Portability: POSIX
--
-- Utility functions for chart drawing functionality. 
-- Uses the C code shared by the swiss ephemeris authors in the official
-- mailing list: <https://groups.io/g/swisseph/message/5568>

module SwissEphemeris.ChartUtils (
  GlyphInfo(..),
  PlanetGlyphInfo,
  glyphPlanet,
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
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (forM)
import Control.Exception (bracket)
import Data.Bifunctor (second)
import Data.Maybe (listToMaybe)

type PlanetGlyph = GravityObject Planet

-- | Information about a @glyph@ (planet or some other object
-- one intends to render within a circular chart) indicating
-- suggested position and scale as decided by 'gravGroup'
-- or 'gravGroup2' to minimize collisions without affecting
-- the sequence of a list of objects, or the sectors
-- within which they may be grouped.
data GlyphInfo a = GlyphInfo
  { originalPosition :: Double
  -- ^ the original position, before correction
  , glyphSize :: (Double, Double)
  -- ^ lsize,rsize: original size, in degrees, from the center extending
  -- to the left and the right, respectively.
  ,  placedPosition :: Double
  -- ^ position decided by the algorithm, in degrees
  , sectorNumber :: Int
  -- ^ sector assigned; should be the same as the original
  , sequenceNumber :: Int
  -- ^ position in sequence, should also be preserved
  , levelNumber :: Int
  -- ^ if allowing for multiple concentric levels, which
  -- level is this supposed to be on.
  , glyphScale :: Double
  -- ^ percentage of actual size it should be resized to
  -- fit, as per the algorithm's recommendation.
  , extraData  :: a
  -- ^ arbitrary data. For @PlanetGlyphInfo@, this is a 'Planet'.
  } deriving (Show, Eq)

-- | @GlyphInfo@ specialized to carry 'Planet' names
-- as its @extraData@.
type PlanetGlyphInfo = GlyphInfo Planet

-- | Convenience alias for the 'extraData' accessor, get
-- the 'Planet' conveyed along a glyph info.
glyphPlanet :: PlanetGlyphInfo -> Planet
glyphPlanet = extraData

-- | Given dimensions, planet positions and "sectors" within which
-- the planets are meant to be drawn as glyphs, return a list
-- pairing each position with a 'PlanetGlyphInfo' that not only
-- remembers the position's planet, it's guaranteed to place it
-- in the same sector and sequence it started in, but moved as to
-- avoid colliding with other nearby planets or sector boundaries.
--
-- Note that "sectors" are usually cusps, but one must take that they're
-- sorted or "linearized": no sector should jump over 0/360, and the
-- last sector should mark the "end" of the circle. 
-- See 'gravGroupEasy' for an approach 
-- that incorporates the advice of Alois, from astro.com: https://groups.io/g/swisseph/message/5568
-- (in short: make all positions, planets and sectors, relative to the first sector,
-- to ensure there's no "jumping" the 360 degree mark.)
gravGroup
  :: HasEclipticLongitude  a
  => (Double, Double)
  -- ^ lwidth, rwidth
  -> [(Planet, a)]
  -- ^ list of pre-calculated positions
  -> [Double]
  -- ^ list of "sectors" (e.g. house cusps + end of last cusp)
  -> Either String [PlanetGlyphInfo]
gravGroup sz positions sectors =
  unsafePerformIO $ do
    withGrobs sz positions $ \grobs ->
      withArray (map realToFrac sectors) $ \sbdy ->
        allocaErrorMessage $ \serr -> do
          let nob = fromIntegral $ length positions
              nsectors = max 0 $ fromIntegral $ length sectors - 1
          retval <-
            c_grav_group grobs nob sbdy nsectors serr

          if retval < 0 then do
            msg <- peekCAString serr
            pure $ Left msg
          else do
            repositioned <- peekArray (fromIntegral nob) grobs
            glyphInfos <- mapM glyphInfo repositioned
            pure . Right $ glyphInfos

-- | /Easy/ version of 'gravGroup' that:
--
-- * Assums glyphs are square/symmetrical, so the left and right widths
-- are just half of the provided width, each.
-- * Will "linearize" all positions before processing by setting them to be
--   relative to the first cusp/sector, and correct them afterwards.
gravGroupEasy :: HasEclipticLongitude a
  => Double
  -> [(Planet, a)]
  -> [HouseCusp]
  -> Either String [PlanetGlyphInfo]
gravGroupEasy = gravGroupEasy' gravGroup

-- | Same semantics and warnings as 'gravGroup', but allows a couple of things for
-- more advanced (or crowded) applications:
--
-- * Can send an empty list of sectors, to indicate that there's no subdivisions
-- in the circle.
-- * Can specify if planets can be pushed to an "inner" level if they're too
-- crowded in their assigned sector. Useful when drawing several objects in a
-- chart with many/tight sectors.
--
-- With a non-empty list of sectors, and not allowing shifting, this is essentially
-- a slightly slower version of 'gravGroup'.
gravGroup2
  :: HasEclipticLongitude a
  => (Double, Double)
  -- ^ lwidth, rwidth
  -> [(Planet, a)]
  -- ^ list of pre-calculated positions
  -> [Double]
  -- ^ list of "sectors" (e.g. house cusps + end of last cusp)
  -- (can be empty, indicating that we're working in a non-subdivided circle.)
  -> Bool
  -- ^ allow planets to be moved up or down a level?
  -> Either String [PlanetGlyphInfo]
gravGroup2 sz positions sectors allowShift =
  -- for empty sectors, we need to add an artificial "whole-circle" sector.
  let sectors' = if null sectors then [0, 360.0] else sectors
  in unsafePerformIO $ do
    withGrobs sz positions $ \grobs ->
      withArray (map realToFrac sectors') $ \sbdy ->
        allocaErrorMessage $ \serr -> do
          let nob = fromIntegral $ length positions
              -- empty sector lists are allowed:
              nsectors = max 0 $ fromIntegral $ length sectors - 1
              mayShift = fromBool allowShift
          retval <-
            c_grav_group2 grobs nob sbdy nsectors mayShift serr

          if retval < 0 then do
            msg <- peekCAString serr
            pure $ Left msg
          else do
            repositioned <- peekArray (fromIntegral nob) grobs
            glyphInfos <- mapM glyphInfo repositioned
            pure . Right $ glyphInfos


-- | /Easy/ version of 'gravGroup2', same provisions as 'gravGroupEasy'
gravGroup2Easy :: HasEclipticLongitude a
  => Double
  -> [(Planet, a)]
  -> [HouseCusp]
  -> Bool
  -> Either String [PlanetGlyphInfo]
gravGroup2Easy w' ps' hs' shift' = 
  gravGroupEasy' (\w ps hs -> gravGroup2 w ps hs shift') w' ps' hs'

-- | Given glyph dimensions and a list of ecliptic positions for planets,
-- execute the given computation with an array of @GravityObject@s,
-- ensuring that no pointers escape scope.
withGrobs
  :: HasEclipticLongitude a
  => (Double, Double)
  -> [(Planet, a)]
  -> (Ptr PlanetGlyph -> IO b)
  -> IO b
withGrobs (lwidth, rwidth) positions f = do
  -- we're using the least sophisticated of the strategies
  -- here:
  -- https://ro-che.info/articles/2017-08-06-manage-allocated-memory-haskell
  -- or here: https://wiki.haskell.org/Bracket_pattern
  -- but it seems to do the job
  bracket
    mkGrobList
    freePlanetPtrs
    (`withArray` f)
  where
    mkGrobList = forM positions $ \(planet, pos) -> do
      planetPtr <- new planet
      pure $
       GravityObject {
         pos =   realToFrac . getEclipticLongitude $ pos
       , lsize = realToFrac lwidth
       , rsize = realToFrac rwidth
       -- fields that will be changed by the functions
       , ppos = 0.0
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
    freePlanetPtrs = mapM_ (free . dp)



glyphInfo :: PlanetGlyph -> IO PlanetGlyphInfo
glyphInfo GravityObject{pos, lsize, rsize, ppos, sector_no, sequence_no, level_no, scale, dp} = do
  planet' <- peek dp
  pure $
    GlyphInfo {
      originalPosition = realToFrac  pos
    , glyphSize = (realToFrac lsize, realToFrac rsize)
    , placedPosition = realToFrac ppos
    , sectorNumber = fromIntegral  sector_no
    , sequenceNumber = fromIntegral sequence_no
    , levelNumber = fromIntegral level_no
    , glyphScale = realToFrac scale
    , extraData = planet'
    }

gravGroupEasy' :: HasEclipticLongitude c =>
  ((Double, Double) -> [(Planet, c)] -> [Double] -> Either String [PlanetGlyphInfo])
  -> Double
  -> [(Planet, c)]
  -> [Double]
  -> Either String [PlanetGlyphInfo]
gravGroupEasy' gravGroupF w ps s = do
  -- NOTE(luis) Only using `(++)` to please base <= 4.10,
  -- I'm partial to treating all Semigroups uniformly!
  glyphs <- gravGroupF (w/2,w/2) ps' (s' ++ coda)
  pure $ map (recenterGlyph s1) glyphs
  where
    coda =
      if null s' then mempty else [head s' + 360]
    s1 = listToMaybe s
    s' = map (relativeTo s1) s
    ps' = map (second (\p -> setEclipticLongitude p (relativeTo s1 (getEclipticLongitude p)))) ps


recenterGlyph :: Maybe Double -> GlyphInfo a -> GlyphInfo a
recenterGlyph s1 g@GlyphInfo{originalPosition, placedPosition} =
  g{
    originalPosition = unrelativeTo s1 originalPosition,
    placedPosition   = unrelativeTo s1 placedPosition
  }

relativeTo :: Maybe Double -> Double -> Double
relativeTo Nothing pos = pos
relativeTo (Just s1) pos =
  let corrected = pos - s1
  in if corrected < 0 then
    corrected + 360
  else
    corrected

unrelativeTo :: Maybe Double -> Double -> Double
unrelativeTo Nothing pos = pos
unrelativeTo (Just s1) pos =
  let undone = pos + s1
  in if undone >= 360 then
    undone - 360
  else
    undone
