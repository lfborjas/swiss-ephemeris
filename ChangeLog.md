# Changelog for swiss-ephemeris

## v0.1.0.0 (2020-08-12)

* Bundles the C code for v2.09.01 of [Swiss
  Ephemerides](https://www.astro.com/swisseph/swephinfo_e.htm) -- refer to that
  page and related documentation for other possible additions to this package! 
* First release with basic bindings to calculate the coordinates of bodies
  between the Sun and Chiron, plus cusps and major angles --
`calculateCoordinates` and `calculateCusps`, respectively.
* The functions `setEphemeridesPath` and `closeEphemerides` are provided to
  initialize (important) and release (less important) resources related to
  caching calculations and locating the data files for ephemerides.
