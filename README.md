# swiss-ephemeris

Haskell bindings for the [Swiss Ephemeris](https://www.astro.com/swisseph/swephinfo_e.htm) library.

See the tests in the `spec` folder for example usage, but here's a simple "main" that demonstrates the current abilities, inspired by the [sample program in the official library](https://www.astro.com/swisseph/swephprg.htm#_Toc46406771):

```haskell
main :: IO
main = do 
  -- location of your ephemeris directory, must be absolute. We bundle a sample one in `swedist`.
  setEphemeridesPath "/Users/luis/code/swiss-ephemeris/swedist/sweph_18"
  let time = julianDay 1989 1 6 0.0
  -- locate all bodies between the Sun and Chiron (further asteroids currently not supported, but they're an enum entry away)
  let coords = map (\p -> (p, (calculateCoordinates time p))) [Sun .. Chiron]
  -- use the Placidus house system, which is the most traditional.
  let cusps  = calculateCusps time (basicCoords (14.0839053, -87.2750137)) Placidus
  forM_ coords $ \(planet, coord)->
    putStrLn $ show planet ++ ": " ++ show coord
  putStrLn $ "Cusps: " ++ show cusps
```

Should print the latitude and longitude (plus some velocities) for all planets, and the cusps and other major angles.

To see actual results, check out the tests.

## Notes

All the code in the `csrc` folder comes from directly from the [latest official tarball, v2.09.01](https://www.astro.com/ftp/swisseph/). The `swemini.c` and `swetest.c` files are commented out since they both define a `main` which conflicted upon compilation, and we don't
need it for this library anyway.

The `swedist` folder includes the original documentation (see the `doc`) folder, and a copy of the actual ephemeris data files.
As noted in the [original documentation](https://www.astro.com/swisseph/swisseph.htm), you can omit the `setEphePath` call and calculations will use a built-in analytical
ephemeris which:

> provides "only" a precision of 0.1 arc seconds for the planets and 3" for the Moon.No asteroids will be available, and no barycentric option can be used.

## Contributing

I've only made available the types and functions that are useful for my own, traditional, horoscope calculations.
Feel free to add more! See the [astro.com documentation](https://www.astro.com/swisseph/swisseph.htm) for ideas!
