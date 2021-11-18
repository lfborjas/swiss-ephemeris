# Changelog for swiss-ephemeris

## UPCOMING

* Export `utcToJulianDays`, to obtain a product of `(TT, UT1)` Julian Days from a `UTCTime` value --
  saves you one IO trip vs. getting them separately.

## v1.4.0.0 (2021-11-11)

**BREAKING CHANGE:** Major refactoring of time values: `JulianDay` is no longer an alias for `Double`,
and is now a type that carries a witness of its provenance -- which enables us to work with functions that
transact in both Terrestrial (Ephemeris) Time, and Universal Time, without mixing them up.

* Upgrades to version `2.10.02` of the C library.
* Adds the `Precalculated` namespace with functions to read and write pre-calculated
  ephemeris from a file on disk. Useful when examining an interval of time for ecliptic phenomena.
* Introduces the `SwissEphemeris.Time` module, with various conversions between time standards
  and some Haskell time values.
* Introduces functions that find moments of exactitude for longitude crossings (heliocentric and geocentric,) 
  and moments of exactitude for lunar phases. The geocentric interpolation, as well as lunar phases,
  use the Brent-Dekker algorithm for root finding, the heliocentric functions are able to do faster
  parabolic approximation. I need to study the C sources a bit more to see if something similar is possible
  for interpolation in general (see the `events` files in the C sources for ideas.)
* Adds functions for calculating planetary phenomena as visible from earth, as well as solar and lunar eclipses.
* [dev] adds a `Dockerfile` and `NOTES.md` to aid in diagnosing memory leaks.
* [dev] adds some basic Nix derivations for producing documentations and a release tarball.

## v1.3.0.2

* More memory safety paranoia: ensure that `gravGroup` functions have appropriately
  scoped "extraData" pointers; was using an unsafe function before that somewhat inexplicably never failed, except for one time _maybe_ while doing unrelated testing.
* Fix empty sector handling for `gravGroup`: it now correctly reports that zero-length
  sectors are not supported.

## v1.3.0.1 
 
A couple of memory safety patches:

* Attempt to rein in memory unsafety by keeping all pointer peeking in IO for gravGroup fns.
* Always allocate 256 chars for error messages. 
* [dev] Bundle test ephemeris into the hackage tarball, to allow hackage CI and nixOS to
  successfully run tests.

## v1.3.0.0 (2021-06-18)

* **Drops support for base < 4.10**, which effectively excludes GHC versions less
  than or equal to 8.0.2. Supporting older haskells gets more cumbersome with each
  addition. Please submit a patch with all the preprocessor magic if you _really_
  want old haskell support.
* Add `ChartUtils` namespace, with a couple of convenience functions for chart drawing.
* Upgrade to swiss ephemeris 2.10.01.
* **Update LICENSE to AGPL** -- it changed in the C library, too.
* [dev] Fix QuickCheck test dependency, to hopefully be copacetic in NixOS builds.
* [dev] add optional `nix` scripts.
* [dev] a whole buncha hlint + autoformat fixes.

## v1.2.1.1 (2021-03-14)

* Bump upper bound for `base`, to work with newer haskells.

## v1.2.1.0 (2020-11-14)

* Introduces `gregorianDateTime`, which is the reverse of `julianDay`: given a JD, return
  a tuple with `(year, month, day, decimalHour)`.

## v1.2.0.0 (2020-09-14)

**BREAKING CHANGE:** `splitDegrees` now takes options that reflect the options in the underlying library.

* Constructors for `JulianTime` and `SiderealTime` are now exposed.
* Introduce `SplitDegreesOption` enum for all options one can split degrees with; amends `splitDegrees` to take
  said options as the first argument.
* `splitDegreesZodiac` is unchanged, though a mere veneer for the now more powerful `splitDegrees`.
* Since `splitDegreesZodiac` goes the extra enum-mile to provide human-readable zodiac names, and the underlying
  library can also split on Nakshatras, we now include the `NakshatraName` enum. Names are from wikipedia
  and I saw some variants, so please forgive any mispellings!

## v1.1.0.0 (2020-09-12)

**BREAKING CHANGE:** the `Coordinates` type has been retired, in favor of the more specific
`GeographicPosition` and `EclipticPosition`. `calculateCoordinates` is now `calculateEclipticPosition`,
and the `calculateCusps*` family now takes a `GeographicPosition` as part of its inputs.

* Introduces an `Internal` module with types and helpers that this library introduces,
  which are not native to the underlying C library. Import at your own risk! (the "curated"/
  "stable" ones are re-exported by the main module.)
  - Deprecates the `Coordinates` type, in favor of `EclipticPosition`.
* Introduces functions to `calculateEquatorialPosition` and `calculateObliquity` at a given time,
  as well as types that better convey the different types of positions (`EquatorialPosition`, `ObliquityInformation`).
* Some astrology helpers: convert between equatorial and ecliptic (and vice-versa,)
  obtain the Delta Time effective at a given moment, obtain the house position of a given body.
  (**Note:** the `calculateHousePosition` function is more useful for working near the polar circles or for bodies
  off of the ecliptic -- the ARMC and obliquity need to be calculated or provided, it's simpler
  if you already have the cusps: just check which cusps a given longitude falls between -- no need for
  this function!)


## v1.0.0.0 (2020-09-07)

* Refactor the `calculateCusps` function:
  - Return a simple list of cusps. This allows for future implementations of exotic systems
    that have more (or fewer?) cusps, and hews closer to regular usage (which iterates over the cusps.)
  - The house system comes first, to allow for more ergonomic partial application for uses where one system is
    the "default" (e.g. `traditionalCusps = calculateCusps Placidus`.)
* Cleans up haddocks, adds many links to the original docs (and notes the headings, since updates to those
  seem to break hyperlinking?)

## v0.3.1.0

* Fixes occasional segmentation fault (caught most often in the more memory-strapped CI server than in my computer,)
  caused by using [`alloca`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Foreign-Marshal-Alloc.html#v:alloca) for the error string and, when no error string was populated, ending with undefined
  behavior. Now we explicitly allocate the 256 `char`s that the documentation and C sources recommend, which seems to be always
  freed by Haskell, vs. leaving a hole somewhere when the underlying library fails to terminate the string.

## v0.3.0.0

**Breaking fixes to `calculateCusps` and `calculateCoordinates`**

* Upgrades to v2.09.03 of the C library, to incorporate some bug fixes that seem marginally related
  to random breakage I've seen; read more at: https://www.astro.com/swisseph/swephprg.htm#_Toc49847971
* Introduces `withoutEphemerides` which sets the ephe path to `NULL` (via the also new `setNoEphemeridesPath`)
  and takes care of calling `closeEphemerides`. Use this or `withEphemerides` for memory safety,
  only call the functions directly if you _really_ know what you're doing (i.e setting/closing ephemerides
  in some other manner.)
* Both calculation functions are now `IO` computations, to reflect the fact that they may interact
  with ephemeris data and allocate memory that `closeEphemerides` _has_ to free.
* More closely reflects the underlying behavior for calculating cusps: it _may_ return
  cusps in the `Porphyrius` system if given a point for which the chosen system fails. To
  more explicitly reflect this, we now have `calculateCuspsStrict` which returns a `Left` value
  if the requested house system couldn't be used. `calculateCuspsLenient`  always returns a calculation,
  and is aliased to `calculateCusps` as the "default" method.
* Since the calculation may have changed the house system, we now return a `systemUsed` entry
  in the `CuspsCalculation` record.


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
