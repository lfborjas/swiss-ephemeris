## Swiss Ephemeris C sources

All files here are the ones provided by astrodienst. For the purposes
of the Haskell library, I've added the following derived files:

* `dgravgroup.h/dgravgroup.c`: `double`-capable versions of `gravgroup.{c,h}`;
  they do everything the same, except that they accept positions in `double` degrees,
  instead of integer centiseconds.
* `configurable_sweephe4.{h,c}`: same as `sweephe4.{h,c}`, except that:
  * It allows setting the base path for the precalculated ephemeris via the
    `EP4_PATH` environment variable.
  * It uses a thread-safe global `struct` to track its current file pointer,
    and the aforementioned path.
* `configurable_swephgen.c`
* The target `swegen` has been added to the `Makefile`, to generate precalculated ephemeris, and test existing precalculated ephemeris files.

Example usage of the latter (244 is the Julian range that includes the 21st century):

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
