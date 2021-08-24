#include "swephexp.h"
#include "interpolate.h"

// jdx   must be pointer to double, returns moment of direction change
// idir  must be pointer to integer, returns -1 if objects gets retrograde, 1 if direct
// The start moment jd0_ut must be at least 30 minutes before the direction change.
int swe_next_direction_change_ut(double jd0, int ipl, int iflag, double *jdx, int *idir, char *serr)
{
  double jd_end = jd0 + 1000; // default to looking within three years from the start date
  return swe_next_direction_change_ut_between(
    jd0,
    jd_end,
    ipl,
    iflag,
    jdx,
    idir,
    serr
  );
}

int swe_next_direction_change_ut_between(double jd0_ut, double jd_ut_end, int ipl, int iflag, double *jdx, int *idir, char *serr)
{
  double jd0 = jd0_ut + swe_deltat(jd0_ut);
  double jd_end = jd_ut_end + swe_deltat(jd_ut_end);
  double tx;
  int rval = swe_next_direction_change_between(jd0, jd_end, ipl, iflag, &tx, idir, serr);
  if (rval >= 0)
    *jdx = tx - swe_deltat(tx);
  return rval;
}

int swe_next_direction_change(double jd0, int ipl, int iflag, double *jdx, int *idir, char *serr)
{
  double jd_end = jd0 + 1000; // default to looking within three years from now
  return swe_next_direction_change_between(
    jd0,
    jd_end,
    ipl,
    iflag,
    jdx,
    idir,
    serr
  );
}

int swe_next_direction_change_between(double jd0, double jd_end, int ipl, int iflag, double *jdx, int *idir, char *serr)
{
  double jd_step = 1;
  double xx[6], d1, d2, y0, y1, y2, a, b, jd, tx;
  int rval, is;
  if (jd_step <= 0) jd_step = 1.0;
  // NOTE(luis) adding a couple of days to the end of the search since 3 positions
  // are needed for interpolation, at least.
  double orig_end = jd_end;
  if (fabs(jd_end - jd0) < 3){
    jd_end += 3;
  }
  rval = swe_calc(jd0, ipl, iflag, xx, serr);
  if (rval < 0) return rval; 
  y0 = xx[0];
  for (is = 0, jd = jd0 + jd_step; jd <= jd_end; is++, jd += jd_step) {
    rval = swe_calc(jd, ipl, iflag, xx, serr);
    if (rval < 0) return rval; 
    if (is == 0) {	// we need at least 3 steps
      y1 = xx[0];
      continue;
    }
    y2 = xx[0];
    // get parabola y = ax^2  + bx + c  and derivative y' = 2ax + b
    d1 = swe_difdeg2n(y1, y0);
    d2 = swe_difdeg2n(y2, y1);
    y0 = y1;	// for next step
    y1 = y2;
    b = (d1 + d2) / 2;
    a = (d2 - d1) / 2;
    if (a == 0) continue;	// curve is flat
    tx = - b / a / 2.0;		// time when derivative is zer0
    if (tx < -1 || tx > 1) continue;
    *jdx = jd - jd_step + tx * jd_step;
    if (*jdx - jd0 < 30.0 / 1440) continue;	// ignore if within 30 minutes of start moment
    while (jd_step > 2 / 1440.0) {
      jd_step = jd_step / 2;
      double t1 = *jdx;
      double t0 = t1 - jd_step;
      double t2 = t1 + jd_step;
      rval = swe_calc(t0 , ipl, iflag, xx, serr);
      if (rval < 0) return rval;
      y0 = xx[0];
      rval = swe_calc(t1 , ipl, iflag, xx, serr);
      if (rval < 0) return rval; 
      y1 = xx[0];
      rval = swe_calc(t2 , ipl, iflag, xx, serr);
      if (rval < 0) return rval; 
      y2 = xx[0];
      d1 = swe_difdeg2n(y1, y0);
      d2 = swe_difdeg2n(y2, y1);
      b = (d1 + d2) / 2;
      a = (d2 - d1) / 2;
      if (a == 0) continue;	// curve is flat
      tx = - b / a / 2.0;		// time when derivative is zer0
      if (tx < -1 || tx > 1) continue;
      *jdx = t1 + tx * jd_step;
      double tdiff = fabs(*jdx - t1);
      if (tdiff < 1 / 86400.0) break;
    }
    if (a > 0)
      *idir = 1;
    else
      *idir = -1;
    if (*jdx > orig_end){
      sprintf(serr, "swe_next_direction_change: no change within %lf days",  (orig_end - jd0));
      return ERR;
    }
    return rval;
  }
  // come here only if no change found in loop
  if (serr != NULL)
    sprintf(serr, "swe_next_direction_change: no change within %lf days",  (orig_end - jd0));
  return ERR;
}

#define CROSS_PRECISION (1 / 3600000.0) 	// one milliarc sec


int swe_interpolate_ut(int ipl, double x2cross, double jd0_ut, double jd_ut_end, int iflag, double *jdx, char *serr)
{
  double jd0 = jd0_ut + swe_deltat(jd0_ut);
  double jd_end = jd_ut_end + swe_deltat(jd_ut_end);
  double tx;
  int rval = swe_interpolate(ipl, x2cross, jd0, jd_end, iflag, &tx, serr);
  if (rval >= 0)
    *jdx = tx - swe_deltat(tx);
  return rval;
}

/* Given two time values that bracket a planet's crossing, use the Brent-Dekker algorithm to find the exact moment of crossing
   from: https://en.wikipedia.org/wiki/Brent%27s_method.
   Defaults to 100 iterations, with a tolerance of one milliarcsecond. It is recommended that you provide values
   that are known to contain a crossing; if a crossing isn't contained, it'll fail fast with a 'not bracketed'
   error; and if the interval is too long, it will run out of iterations.

   Note that in rare cases, a planet may cross a given longitude more than once in any given interval of time.
   We only find the first such crossing here.
*/
int swe_interpolate(int ipl, double x2cross, double jd0, double jd_end, int iflag, double *jdx, char *serr)
{
  double jd_step = 1;
  double epsilon = CROSS_PRECISION; // we're done if close by a milliarcsecond
  double xx[6], a, b, c, d, s, pa, pb, pc, ps, fa, fb, fc, fs, tmp;
  int i;
  AS_BOOL mflag;
  int rval, is, max_iter;
  max_iter = 100;
  if (jd_step <= 0) jd_step = 1.0;
  a = jd0;
  b = jd_end;
  
  rval = swe_calc(a, ipl, iflag, xx, serr);
  if (rval < 0) return rval;
  pa = xx[0];
  fa = swe_difdeg2n(pa, x2cross);

  rval = swe_calc(b, ipl, iflag, xx, serr);
  if (rval < 0) return rval;
  pb = xx[0];
  fb = swe_difdeg2n(pb, x2cross);

  if ((fa * fb) >= 0){
    sprintf(serr, "swe_interpolate: not bracketed: %lf - %lf", a, b);
    return ERR;
  }

  if(fabs(fa) < fabs(fb)){
    tmp = a;
    a = b;
    b = tmp;

    tmp = fa;
    fa = fb;
    fb = tmp;
  }
  
  c = a;
  fc = fa;
  s = b;
  fs = fb;
  mflag = TRUE;
  
  for(i=0; i < max_iter; i++){
    //printf("iteration: %d, s: %lf", i, s);
    if(fabs(b-a) < epsilon){
      //printf("ab epsilon? %lf", fabs(b-a));
      break;
    }
    
    if(fabs(fb) <= epsilon || fabs(fs) <= epsilon){
      //printf("breaks epsilon? %lf | %lf", fb, fs);
      break;
    }
    
    if (fa != fc && fb != fc){
      //printf("USING qUAD");
      // inverse quadratic method
      s = ((a*fb*fc)/(fa - fb)*(fa - fc))
        + ((b*fa*fc)/(fb - fa)*(fb - fc))
        + ((c*fa*fb)/(fc - fa)*(fc - fb));
    } else {
      // secant method
      //printf("USING sECANT");
      s = b - (fb*((b-a)/(fb-fa)));
    }

    if(!((3 * a + b)/4 < s && s < b)
      || (mflag  && fabs(s-b) >= fabs(b-c)/2)
      || (!mflag && fabs(s-b) >= fabs(c-d)/2)
      || (mflag  && fabs(b-c) < epsilon)
      || (!mflag && fabs(c-d) < epsilon)){
        // bisection method
        //printf("USING bISECT");
        s = (a+b)/2;
        mflag = TRUE;
    } else {
      mflag = FALSE;
    }
    
    rval = swe_calc(s, ipl, iflag, xx, serr);
    if (rval < 0) return rval;
    ps = xx[0];
    fs = swe_difdeg2n(ps, x2cross);

    // first assignment of d; won't be used on the
    // first iteration because mflag is set
    d = c;

    c = b;
    fc = fb;

    if (fa * fs < 0){
      b = s;
      fb = fs;
    } else {
      a = s;
      fa = fs;
    }

    if(fabs(fa) < fabs(fb)){
      tmp = a;
      a = b;
      b = tmp;

      tmp = fa;
      fa = fb;
      fb = tmp;
    }
  } // done iterating

  if (fabs(fs) <= epsilon){
    *jdx = s;
  } else if (fabs(fb) <= epsilon) {
    *jdx = b;
  } else {
    sprintf(serr, "swe_interpolate: no root found within %d iterations, and epsilon %lf", i, epsilon);
    return ERR;
  }
  return rval;

}
