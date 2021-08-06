## Swiss Ephemeris C sources

All the files provided by astrodienst are here; for the purposes
of the Haskell library, I've added the following derived files:

* `dgravgroup.h/dgravgroup.c`: `double`-capable versions of `gravgroup.{c,h}`;
  they do everything the same, except that they accept positions in `double` degrees,
  instead of integer centiseconds.
* `configurable_sweephe4.{h,c}`: same as `sweephe4.{h,c}`, except that:
  * It allows setting the base path for the precalculated ephemeris via the
    `EP4_PATH` environment variable.
  * It uses a thread-safe global `struct` to track its current file pointer,
    and the aforementioned path.
* `configurable_swephgen.c`: includes a `main` function that can either generate a `sep4_` file, or run interactively to output ephemerides for specific dates. I've modified it to _only_ work with precalculated ephemerides -- the `*read` functions can be told to fall back to `swecalc` if the requested date is not precalculated. It also uses the `double`-capable `dephread2` function, instead of the `centisecond` one that the non-`configurable_` version invokes.
* The target `swegen` has been added to the `Makefile`, to be able to generate precalculated ephemeris, and test existing precalculated ephemeris files.


## Precalculated ephemeris

Example usage of `swegen` (244 is the Julian range that includes ~1968-1995):

```sh
 ~/c/l/s/csrc > SE_EPHE_PATH="../swedist/sweph_18/" EP4_PATH=(PWD) ./swegen -f244 -t
date ?6
1
1989
ephgen test d=   2447532.5  dmy 6.1.1989 greg
        ecliptic   23 26'34.39" nutation    0  0' 7.01"
 0     285 38'47.68"
 1     262 28'20.43"
 2     304 18'48.84"
 3     264  2'52.36"
 4      22 47' 4.17"
 5      56 26'29.72"
 6     276 10'54.96"
 7     272  3' 6.04"
 8     280  6'39.67"
 9     224 40'54.17"
10     337 31'24.66"
11     336  5'38.87"
12      93 32'14.34"
```

Omitting the `-t` flag will generate the `sep4_` file for the given starting year (`-f`). Each file contains 10,000 days of data; so `sep4_244` spans ephemeris from May 23 1968 to
October 9 1995 (got that by converting both `2440000` and `2450000` in the [nasa tool](https://ssd.jpl.nasa.gov/tc.cgi#top)). The `-n` flag will generate N-1 additional files for the subsequent years (e.g. 
`-n 4` would generate roughly a century of data.) In my architecture, a single file
occupies about `384K` on disk.

**N.B. (June 26, 2021)** I've updated the code to generate, and process, ephemerides for
the moon's mean apogee (dark moon Lilith), as the 14th element in the array; here's an example run:

```sh
> SE_EPHE_PATH="/c/swiss-ephemeris/swedist/sweph_18/" EP4_PATH="/c/swiss-ephemeris/swedist/precalc/" ./swegen -f245 -t
date ?26      
6
2021
ephgen test d=   2459391.5  dmy 26.6.2021 greg
        ecliptic   23 26'13.98" nutation -  0  0'15.54"
 0      94 37'37.20"
 1     291 27'43.93"
 2      76 29'52.26"
 3     118 33'38.54"
 4     128 54'41.25"
 5     332  8'18.59"
 6     312 38'30.19"
 7      43 36' 2.55"
 8     353 11'59.27"
 9     296  4'20.21"
10      69 32'18.99"
11      70 37'57.56"
12      12 45'42.86"
13      57 31'24.12"
```

See commit [ed0f92b](https://github.com/lfborjas/swiss-ephemeris/commit/ed0f92be747949f92d02a8919f11566d07504b88) for this change, which can be useful guidance if one wants to
add other bodies.
