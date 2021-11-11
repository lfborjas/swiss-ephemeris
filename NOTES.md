## Reproducing tests failures

When a quickcheck prop fails, one can re-run with the same seed:

```sh
stack test --test-arguments="--seed=1152638838"
```

(odd syntax bemoned at: https://github.com/commercialhaskell/stack/issues/2210)

## Detecting memory leaks/corruption

First of all, macOS is a terrible environment for this: between having enough
RAM and a strict-enough OS, it makes memory leaks astronomically improbable,
and file pointer issues impossible to reproduce. To have a chance at diagnosing,
use Linux.

For that purpose, a very basic `Dockerfile` guaranteed to provide a workable haskell
env + valgrind is provided; what I do is build it, then enter it to run stuff locally.

To build:

```sh
docker build -t memory-test:latest .
```

To enter the container and run arbitrary commands (assuming the PWD is
this repository -- I'm also using `fish` here, instead of `bash`):

```sh
~/c/l/swiss-ephemeris (ephe4-precalculated)> docker run -it -v $PWD:(PWD) memory-test:latest /bin/bash
```

For the latest memory chickanery, I was able to first detect the segfault and then
run into the causing situation (once I'd added a NULL check) by building
the test target and then running the produced executable via `valgrind`:

```sh
stack test ; valgrind --leak-check=full --show-leak-kinds=all .stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test
```

Some example output from when there was an issue:

```sh
root@61003c6d859e:/Users/luis/code/lfborjas/swiss-ephemeris# valgrind --leak-check=full --show-leak-kinds=all .stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test
==3017== Memcheck, a memory error detector
==3017== Copyright (C) 2002-2017, and GNU GPL'd, by Julian Seward et al.
==3017== Using Valgrind-3.14.0 and LibVEX; rerun with -h for copyright info
==3017== Command: .stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test
==3017== 
==3017== Warning: set address range perms: large range [0x4200000000, 0x14200100000) (noaccess)

ChartUtils
  gravGroupEasy
    rejects empty sectors
    downscales glyphs in narrow sectors
    returns planets in corrected positions, when applicable
  gravGroup2Easy
    accepts empty sectors
    shifts glyphs in narrow sectors to different levels, keeps the scale
    returns planets in corrected positions, when applicable
Precalculated
  readEphemerisRaw
    with stored ephemeris, but no fallback ephemeris
      it is unable to read ephemeris for out-of-range days
        +++ OK, passed 10 tests.
      it is able to read ephemeris for in-range days
        +++ OK, passed 100 tests.
    with stored ephemeris, and fallback ephemeris
==3017== Thread 8:
==3017== Invalid read of size 4
==3017==    at 0x4AFCC91: fseek (fseek.c:35)
==3017==    by 0x4658E1: eph4_posit (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==3017==    by 0x465A45: ephe4_unpack_d.constprop.1 (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==3017==    by 0x465E2F: dephread2 (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==3017==    by 0x45DB9C: ??? (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==3017==  Address 0x0 is not stack'd, malloc'd or (recently) free'd
==3017== 
==3017== 
==3017== Process terminating with default action of signal 11 (SIGSEGV)
==3017==  Access not within mapped region at address 0x0
==3017==    at 0x4AFCC91: fseek (fseek.c:35)
==3017==    by 0x4658E1: eph4_posit (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==3017==    by 0x465A45: ephe4_unpack_d.constprop.1 (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==3017==    by 0x465E2F: dephread2 (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==3017==    by 0x45DB9C: ??? (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==3017==  If you believe this happened as a result of a stack
==3017==  overflow in your program's main thread (unlikely but
==3017==  possible), you can try to increase the size of the
==3017==  main thread stack using the --main-stacksize= flag.
==3017==  The main thread stack size used in this run was 8388608.
==3017== 
```

Then, with a NULL check and some judicious `Debug.trace` in tests where it
_shouldn't_ have failed, were it not for the incorrect static variable (in this case)

```sh
diff --git a/csrc/configurable_sweephe4.c b/csrc/configurable_sweephe4.c
index f4351f9..3510c4a 100644
--- a/csrc/configurable_sweephe4.c
+++ b/csrc/configurable_sweephe4.c
@@ -575,7 +575,7 @@ int eph4_posit(int jlong, AS_BOOL writeflag, char *errtext)
     }
     open_filenr = filenr;
   }
-  if (fseek(ephe4d.ephfp, posit, 0) == 0 && ftell(ephe4d.ephfp) == posit)
+  if (ephe4d.ephfp != NULL && fseek(ephe4d.ephfp, posit, 0) == 0 && ftell(ephe4d.ephfp) == posit)
   {
     return (OK);
   }

Linking .stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test ...
swiss-ephemeris> copy/register
Installing library in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/install/x86_64-linux/d5e432041a154a2d555d2b4cc1abffd8972c9da14f6b950749d04de4c7ddb715/8.8.3/lib/x86_64-linux-ghc-8.8.3/swiss-ephemeris-1.3.0.1-IFR4iB83CJCCocYYFuhW9O
Registering library for swiss-ephemeris-1.3.0.1..
swiss-ephemeris> test (suite: swiss-ephemeris-test)
                             

ChartUtils
  gravGroupEasy
    rejects empty sectors
    downscales glyphs in narrow sectors
    returns planets in corrected positions, when applicable
  gravGroup2Easy
    accepts empty sectors
    shifts glyphs in narrow sectors to different levels, keeps the scale
    returns planets in corrected positions, when applicable
Precalculated
  readEphemerisRaw
    with stored ephemeris, but no fallback ephemeris
      it is unable to read ephemeris for out-of-range days
        +++ OK, passed 10 tests.
eph4_posit: fseek(52716) of file nr 244 failed

      it is able to read ephemeris for in-range days FAILED [1]
    with stored ephemeris, and fallback ephemeris
      it is able to read ephemeris for in-range days
        +++ OK, passed 100 tests.
      it is also able to read ephemeris for out-of-range days
        +++ OK, passed 100 tests.
```

One interesting thing to note is that the various `fopen` calls that swecalc or
the pre-stored ephemeris do cause leaks that are "okay" only inasmuch as the host
OS will clean up after the process:


```sh
Finished in 33.1102 seconds
33 examples, 0 failures
==4572== 
==4572== HEAP SUMMARY:
==4572==     in use at exit: 6,565 bytes in 14 blocks
==4572==   total heap usage: 9,190 allocs, 9,176 frees, 9,062,994 bytes allocated
==4572== 
==4572== 112 bytes in 1 blocks are definitely lost in loss record 1 of 10
==4572==    at 0x483577F: malloc (vg_replace_malloc.c:299)
==4572==    by 0x9220F0: stgMallocBytes (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x928068: stat_exit (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x91C84D: hs_exit_ (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x91CC44: shutdownHaskellAndExit (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x92A830: hs_main (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x421499: main (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572== 
==4572== 181 bytes in 1 blocks are still reachable in loss record 2 of 10
==4572==    at 0x483577F: malloc (vg_replace_malloc.c:299)
==4572==    by 0x84E233: ??? (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572== 
==4572== 464 bytes in 1 blocks are definitely lost in loss record 3 of 10
==4572==    at 0x483577F: malloc (vg_replace_malloc.c:299)
==4572==    by 0x4786DE: sweph (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x47928A: sweplan (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x484D31: swecalc (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x486F41: swe_calc (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x487619: swe_set_ephe_path (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x4961D6: ??? (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572== 
==4572== 552 bytes in 1 blocks are still reachable in loss record 4 of 10
==4572==    at 0x483577F: malloc (vg_replace_malloc.c:299)
==4572==    by 0x4AF50FA: __fopen_internal (iofopen.c:65)
==4572==    by 0x4761AD: swi_fopen (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x4771D3: sweph (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x478FF9: sweplan (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x484D31: swecalc (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x486F41: swe_calc (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x487619: swe_set_ephe_path (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x4961D6: ??? (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572== 
==4572== 552 bytes in 1 blocks are still reachable in loss record 5 of 10
==4572==    at 0x483577F: malloc (vg_replace_malloc.c:299)
==4572==    by 0x4AF50FA: __fopen_internal (iofopen.c:65)
==4572==    by 0x4761AD: swi_fopen (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x4771D3: sweph (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x47928A: sweplan (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x484D31: swecalc (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x486F41: swe_calc (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x487619: swe_set_ephe_path (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x4961D6: ??? (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572== 
==4572== 552 bytes in 1 blocks are still reachable in loss record 6 of 10
==4572==    at 0x483577F: malloc (vg_replace_malloc.c:299)
==4572==    by 0x4AF50FA: __fopen_internal (iofopen.c:65)
==4572==    by 0x4658BE: eph4_posit (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x465A65: ephe4_unpack_d.constprop.1 (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x465E4F: dephread2 (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x45DB9C: ??? (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572== 
==4572== 624 bytes in 1 blocks are definitely lost in loss record 7 of 10
==4572==    at 0x483577F: malloc (vg_replace_malloc.c:299)
==4572==    by 0x477BEB: sweph (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x478FF9: sweplan (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x484D31: swecalc (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x486F41: swe_calc (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x487619: swe_set_ephe_path (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x4961D6: ??? (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572== 
==4572== 624 bytes in 1 blocks are definitely lost in loss record 8 of 10
==4572==    at 0x483577F: malloc (vg_replace_malloc.c:299)
==4572==    by 0x477BEB: sweph (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x477393: sweph (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x478FF9: sweplan (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x484D31: swecalc (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x486F41: swe_calc (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x487619: swe_set_ephe_path (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x4961D6: ??? (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572== 
==4572== 696 bytes in 1 blocks are definitely lost in loss record 9 of 10
==4572==    at 0x483577F: malloc (vg_replace_malloc.c:299)
==4572==    by 0x477BEB: sweph (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x47928A: sweplan (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x484D31: swecalc (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x486F41: swe_calc (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x487619: swe_set_ephe_path (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x4961D6: ??? (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572== 
==4572== 2,208 bytes in 5 blocks are definitely lost in loss record 10 of 10
==4572==    at 0x483577F: malloc (vg_replace_malloc.c:299)
==4572==    by 0x4786DE: sweph (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x478FF9: sweplan (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x484D31: swecalc (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x486F41: swe_calc (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x487619: swe_set_ephe_path (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572==    by 0x4961D6: ??? (in /Users/luis/code/lfborjas/swiss-ephemeris/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/swiss-ephemeris-test/swiss-ephemeris-test)
==4572== 
==4572== LEAK SUMMARY:
==4572==    definitely lost: 4,728 bytes in 10 blocks
==4572==    indirectly lost: 0 bytes in 0 blocks
==4572==      possibly lost: 0 bytes in 0 blocks
==4572==    still reachable: 1,837 bytes in 4 blocks
==4572==         suppressed: 0 bytes in 0 blocks
==4572== 
==4572== For counts of detected and suppressed errors, rerun with: -v
==4572== ERROR SUMMARY: 6 errors from 6 contexts (suppressed: 0 from 0)
```
