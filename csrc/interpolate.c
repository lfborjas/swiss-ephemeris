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
  crossing_data cross;
  cross.crossing_planet = ipl;
  cross.iflag = iflag;
  cross.x2cross = x2cross;

  return brent_dekker(&crosses, jd0, jd_end, CROSS_PRECISION, 100, jdx, &cross, serr);
}

int swe_interpolate_moon_phase_ut(double phase, double jd0_ut, double jd_end_ut, int iflag, double *jdx, char *serr)
{
  double jd0 = jd0_ut + swe_deltat(jd0_ut);
  double jd_end = jd_end_ut + swe_deltat(jd_end_ut);
  double tx;
  int rval = swe_interpolate_moon_phase(phase, jd0, jd_end, iflag, &tx, serr);
  if (rval >= 0)
    *jdx = tx - swe_deltat(tx);
  return rval;
 
}
/* Given a phase (angle between moon and sun positions) and a timeframe during which
   the phase happens, try to find the moment of exactitude. */
int swe_interpolate_moon_phase(double phase, double jd0, double jd_end, int iflag, double *jdx, char *serr)
{
  moon_phase_data phase_data;
  phase_data.phase_angle = phase;
  phase_data.iflag = iflag;

  return brent_dekker(&moon_phase_matches, jd0, jd_end, CROSS_PRECISION, 100, jdx, &phase_data, serr);
}

static int moon_phase_matches(double t, double *phase, void *vdata, char *serr)
{
  int rval;
  double xm[6], xs[6], d, dx;

  moon_phase_data *data;
  data = (moon_phase_data*)vdata;

  rval = swe_calc(t, (int)SE_MOON, data->iflag, xm, serr); 
  if (rval < 0) return rval;
  
  rval = swe_calc(t, (int)SE_SUN, data->iflag, xs, serr); 
  if (rval < 0) return rval;
  
  d = swe_difdegn(xm[0], xs[0]);
  dx = swe_difdeg2n(d, data->phase_angle);
  
  *phase = dx;
  return rval;
}

/* given time `t`, return distance between a planet's position at that time and a given x to cross;
   which planet to calculate, the longitude to cross and flags are given as part of `data`*/
static int crosses(double t, double *xt, void *vdata, char *serr)
{
  int rval;
  double xx[6];
  crossing_data *data;
  data = (crossing_data*)vdata;

  rval = swe_calc(t, data->crossing_planet, data->iflag, xx, serr);
  if (rval < 0) return rval;

  *xt = swe_difdeg2n(xx[0], data->x2cross);
  return rval;
}

/* adaptation of the brent-dekker algorithm for root finding, can find a root
   for a function that takes a double and produces a double; to make it compatible
   with most swiss ephemeris routines, it expects the function to return an integer
   indicating success or failure, and to work with an error string. Any additional
   data necessary for calculation can be sent to the function through `f_data` */
static int brent_dekker(callback_fn f, double start, double end, double epsilon, int max_iter, double *root, void *f_data, char *serr)
{
  double xx[6], a, b, c, d, s, pa, pb, pc, ps, fa, fb, fc, fs, tmp;
  int i;
  AS_BOOL mflag;
  int rval, is;
  a = start;
  b = end;
  
  rval = (*f)(a, &fa, f_data, serr);
  if (rval < 0) return rval;

  rval = (*f)(b, &fb, f_data, serr);
  if (rval < 0) return rval;

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
    
    rval = (*f)(s, &fs, f_data, serr);
    if (rval < 0) return rval;

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
    *root = s;
  } else if (fabs(fb) <= epsilon) {
    *root = b;
  } else if (fabs(b-a) < epsilon) {
    *root = b;
  } else {
    sprintf(serr, "swe_interpolate: no root found within %d iterations, and epsilon %lf", i, epsilon);
    return ERR;
  }
  return rval;

}
