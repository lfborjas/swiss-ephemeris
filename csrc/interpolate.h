/*
Shared by Alois in the swiss ephemeris forum:
https://groups.io/g/swisseph/message/7781

Stated to be in the public domain in the aforementioned listing, i.e.
// This code is in the public domain, no warranty!
*/

#include "swephexp.h"

/* callbacks for interpolation; always return an OK/ERR int, take a double and return
   a pointer to a double result; optional additional data can be passed through a void
   pointer; an error string can be provided. */
typedef int (*callback_fn)(double, double*, void*, char*);

typedef struct crossing_target{
  double x2cross; /* longitude we're seeking to cross */
  int iflag; /* ephemeris flag */
  int crossing_planet; /* planet number */ 
} crossing_data;

int swe_next_direction_change(double jd0, int ipl, int iflag, double *jdx, int *idir, char *serr);
int swe_next_direction_change_between(double jd0, double jd_end, int ipl, int iflag, double *jdx, int *idir, char *serr);
int swe_next_direction_change_ut(double jd0, int ipl, int iflag, double *jdx, int *idir, char *serr);
int swe_next_direction_change_ut_between(double jd0, double jd_end, int ipl, int iflag, double *jdx, int *idir, char *serr);
int swe_interpolate(int ipl, double x2cross, double jd0, double jd_end, int iflag, double *jdx, char *serr);
int swe_interpolate_ut(int ipl, double x2cross, double jd0, double jd_end, int iflag, double *jdx, char *serr);
//helpers
static int crosses(double t, double *xt, void *data, char *serr);
static int brent_dekker(callback_fn f, double start, double end, double epsilon, int max_iter, double *root, void *f_data, char *serr);
