/*
Shared by Alois in the swiss ephemeris forum:
https://groups.io/g/swisseph/message/7781

Stated to be in the public domain in the aforementioned listing, i.e.
// This code is in the public domain, no warranty!
*/

#include "swephexp.h"
int swe_next_direction_change(double jd0, int ipl, int iflag, double *jdx, int *idir, char *serr);
int swe_next_direction_change_between(double jd0, double jd_end, int ipl, int iflag, double *jdx, int *idir, char *serr);
int swe_next_direction_change_ut(double jd0, int ipl, int iflag, double *jdx, int *idir, char *serr);
int swe_next_direction_change_ut_between(double jd0, double jd_end, int ipl, int iflag, double *jdx, int *idir, char *serr);
