{-# LANGUAGE CPP, ForeignFunctionInterface, RecordWildCards #-}
{-| 
Module: Foreign.SwissEphemerisExtras
Description: Declarations of bindings to the underlying C library's non-essential
functionality.

Import at your own risk!

Exposes very low-level FFI bindings to the C library. Use the @SwissEphemeris.*@ modules and their more
Haskell-friendly exports.
-}


module Foreign.SwissEphemerisExtras where

import Foreign
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String

#include <swegrav.h>

-- | Equivalent to GROB in @swegrav.h@
data GravityObject a = GravityObject
  { pos :: CInt
  -- ^ position of object in /centiseconds/.
  , lsize :: CInt
  -- ^ glyph size in your chosen graphics env, left side.
  , rsize :: CInt
  -- ^ glyph size in your chosen graphics env, right side.
  , ppos :: CInt
  -- ^ corrected ("placed") position after we're done; can set as zero.
  , sector_no :: CInt
  -- ^ assigned sector number, will be set as objects are
  -- rearranged.
  , sequence_no :: CInt
  -- ^ number in the sequence of objects, won't be re-assigned,
  -- but setting it is optional
  , level_no :: CInt
  -- ^ when allowing for multi-level arrangement, the level
  -- an object ends up.
  , scale :: CDouble
  -- ^ if any resizing is done, percentage of full size.
  , dp :: Ptr a
  -- ^ pointer to additional data.
  }

instance Storable a => Storable (GravityObject a) where
  alignment _ = #{alignment GROB}
  sizeOf _ = #{size GROB}
  peek ptr = do
    pos <- #{peek GROB, pos} ptr
    lsize <- #{peek GROB, lsize} ptr
    rsize <- #{peek GROB, rsize} ptr
    ppos <- #{peek GROB, ppos} ptr
    sector_no <- #{peek GROB, sector_no} ptr
    sequence_no <- #{peek GROB, sequence_no} ptr
    level_no <- #{peek GROB, level_no} ptr
    scale <- #{peek GROB, scale} ptr
    dp <- #{peek GROB, dp} ptr
    return $ GravityObject{..}
  poke ptr (GravityObject{..})= do
    #{poke GROB, pos} ptr pos
    #{poke GROB, lsize} ptr lsize
    #{poke GROB, rsize} ptr rsize
    #{poke GROB, ppos} ptr ppos
    #{poke GROB, sector_no} ptr sector_no
    #{poke GROB, sequence_no} ptr sequence_no
    #{poke GROB, level_no} ptr level_no
    #{poke GROB, scale} ptr scale
    #{poke GROB, dp} ptr dp
    

-- | Simple helper to recalculate positions for planets/bodies
-- for drawing in a circular chart without collisions/
-- overlaps, keeping them inside their assigned "sectors"
-- (e.g. houses.) See @GravityObject@.
foreign import ccall unsafe "swegrav.h grav_group"
  c_grav_group :: Ptr (GravityObject a)
               -- ^ array of "GROB"s
               -> CInt
               -- ^ nob
               -> Ptr CInt
               -- ^ sectors; must include an extra, final sector.
               -> CInt
               -- ^ nsectors
               -> CString
               -- ^ char* err
               -> (IO CInt)

-- | More advanced version of @grav_group@ that also allows:
-- * Sending in zero sectors (i.e. just drawing in a circle)
-- * Shifting between "levels" (closer to the center) as a way
-- of resolving collisions in glyphs.
foreign import ccall unsafe "swegrav.h grav_group2"
  c_grav_group2 :: Ptr (GravityObject a)
               -- ^ array of "GROB"s
               -> CInt
               -- ^ nob
               -> Ptr CInt
               -- ^ sectors; must include an extra, final sector.
               -> CInt
               -- ^ nsectors
               -> CBool
               -- ^ allow planets to "shift" levels?
               -> CString
               -- ^ char* err
               -> (IO CInt)
