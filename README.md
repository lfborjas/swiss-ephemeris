# swiss-ephemeris

Haskell bindings for the [Swiss Ephemeris](https://www.astro.com/swisseph/swephinfo_e.htm) library.

## Notes

All the code in the `csrc` folder comes from directly from the [latest official tarball](https://www.astro.com/ftp/swisseph/). The `swemini.c` and `swetest.c` files are commented out since they both define a `main` which conflicted upon compilation, and we don't
need it for this library anyway.

The `swedist` folder includes the original documentation (see the `doc`) folder, and a copy of the actual ephemeris data files.
As noted in the [original documentation](https://www.astro.com/swisseph/swisseph.htm), you can omit the `setEphePath` call and calculations will use a built-in analytical
ephemeris which:

> provides "only" a precision of 0.1 arc seconds for the planets and 3" for the Moon.No asteroids will be available, and no barycentric option can be used.

## Contributing

I've only made available the types and functions that are useful for my own, traditional, horoscope calculations.
Feel free to add more! See the [astro.com documentation](https://www.astro.com/swisseph/swisseph.htm) for ideas!
