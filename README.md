# swiss-ephemeris

![build](https://github.com/lfborjas/swiss-ephemeris/workflows/Haskell%20CI/badge.svg)


Haskell bindings for the [Swiss Ephemeris](https://www.astro.com/swisseph/swephinfo_e.htm) library.

See the tests in the `spec` folder for thorough example usage, but here's a simple "main" that demonstrates the current abilities, inspired by the [sample program in the official library](https://www.astro.com/swisseph/swephprg.htm#_Toc46406771):

```haskell
import SwissEphemeris

main :: IO
main = do 
  -- location of your ephemeris directory. We bundle a sample one in `swedist`.
  setEphemeridesPath "./swedist/sweph_18"

  let time = julianDay 1989 1 6 0.0
      place = mkCoordinates{lat = 14.0839053, lng = -87.2750137}

  -- locate all bodies between the Sun and Chiron (further asteroids currently not supported, but they're an enum entry away)
  -- use the Placidus house system, which is the most traditional.
  forM_ [Sun .. Chiron] $ \planet -> do
    -- if no ephemerides data is available for the given planetary body, a `Left` value
    -- will be returned.
    coord <- calculateCoordinates time planet
    putStrLn $ show planet <> ": " <> show coord
  -- Calculate cusps for the given time and place, preferring the `Placidus` system.
  -- note that the underlying library may decide to use a different system if it can't
  -- calculate cusps (happens for the Placidus and Koch systems in locations near the poles.)
  cusps <- calculateCusps time place Placidus
  putStrLn $ "Cusps: " <> show cusps
  -- the underlying library, sadly, allocates some memory/file descriptors, you can free it with:
  closeEphemerides
```
The above should print the latitude and longitude (plus some velocities) for all planets, and the cusps and other major angles.

There's also `withEphemerides` and `withoutEphemerides` bracket-style functions that take care of closing the files for you.

To see actual results and more advanced usage, check out the tests. For some more advanced examples, see `swetest.c` and `swemini.c` in the `csrc` directory: they're the test/example programs provided by the original authors.


## Notes

All the code in the `csrc` folder comes directly from the [latest official tarball, v2.09.03](https://www.astro.com/ftp/swisseph/). 

The `swedist` folder includes the original documentation from the tarball in PDF (see the `doc`) folder, and a copy of ephemeris data files.

For other formats of the original documentation, see: https://www.astro.com/ftp/swisseph/doc/

The authors also host HTML versions of the manuals. Two are provided, a general reference and a programming reference. Both are very useful to get
acquainted with the functionality and implementation details.

* [General Reference](https://www.astro.com/swisseph/swisseph.htm)
* [Programmer's Reference](https://www.astro.com/swisseph/swephprg.htm)

### Ephemerides files

As noted in the [original documentation](https://www.astro.com/swisseph/swisseph.htm) you can omit the `setEphePath` call and calculations will use a built-in analytical
ephemeris which:

> provides "only" a precision of 0.1 arc seconds for the planets and 3" for the Moon. No asteroids will be available, and no barycentric option can be used.

For convenience, we bundle a few files for the time range `1800 AD – 2399 AD`. If you were born before that, or plan to code e.g. transits for after that (!) or 
you'd prefer even more precision, you can [download more ephemerides files from the astro.com downloads page](https://www.astro.com/ftp/swisseph/ephe/)

I chose the bundled files due to this comment in the official docs:

> If the [JPL] file is too big, then you can download the files sepl_18.se1 and semo_18.se1 from here: http://www.astro.com/ftp/swisseph/ephe/

For a full explanation of the files available, see the [Description of the Ephemerides](https://www.astro.com/swisseph/swisseph.htm#_Toc46391649) section of the original manual, also of
interest is the [comparison between the Swiss Ephemeris and the raw NASA JPL
data](https://www.astro.com/swisseph/swisseph.htm#_Toc46391741).

## Contributing

I've only made available the types and functions that are useful for my own, traditional, horoscope calculations.
Feel free to add more! See the [astro.com documentation](https://www.astro.com/swisseph/swisseph.htm) for ideas.
