# Changelog for swiss-ephemeris

## v0.3.0.0

** Breaking fixes to `calculateCusps` **

* More closely reflects the underlying behavior for the underlying library: it _may_ return
  a cusps in the `Porphyrius` system if given a point for which the chosen system fails.
* Error conditions are now limited to null pointers.

## v0.2.0.0

* Introduces `withEphemerides` for bracketed access to the ephemeris directory.
* Changes the signature of `calculateCusps` to return a `Left` value if the underlying library
  is unable to calculate the cusps.
* Introduces "monadic" versions of the calculations that work with instances of `MonadFail`:
  `calculateCuspsM` and `calculateCoordinatesM`
* Improves test coverage with property testing.

## v0.1.0.0 - 0.1.0.2(2020-08-12)

* Bundles the C code for v2.09.01 of [Swiss
  Ephemerides](https://www.astro.com/swisseph/swephinfo_e.htm) -- refer to that
  page and related documentation for other possible additions to this package! 
* First release with basic bindings to calculate the coordinates of bodies
  between the Sun and Chiron, plus cusps and major angles --
`calculateCoordinates` and `calculateCusps`, respectively.
* The functions `setEphemeridesPath` and `closeEphemerides` are provided to
  initialize (important) and release (less important) resources related to
  caching calculations and locating the data files for ephemerides.
