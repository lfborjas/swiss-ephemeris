/********************************************************************
sweephe4.c
access structures and functions for ephemeris file ep4_
a fast precomputed ephemeris
*********************************************************************/
/* Copyright (C) 1997 - 2021 Astrodienst AG, Switzerland.  All rights reserved.

  License conditions
  ------------------

  This file is part of Swiss Ephemeris.

  Swiss Ephemeris is distributed with NO WARRANTY OF ANY KIND.  No author
  or distributor accepts any responsibility for the consequences of using it,
  or for whether it serves any particular purpose or works at all, unless he
  or she says so in writing.  

  Swiss Ephemeris is made available by its authors under a dual licensing
  system. The software developer, who uses any part of Swiss Ephemeris
  in his or her software, must choose between one of the two license models,
  which are
  a) GNU Affero General Public License (AGPL)
  b) Swiss Ephemeris Professional License

  The choice must be made before the software developer distributes software
  containing parts of Swiss Ephemeris to others, and before any public
  service using the developed software is activated.

  If the developer choses the AGPL software license, he or she must fulfill
  the conditions of that license, which includes the obligation to place his
  or her whole software project under the AGPL or a compatible license.
  See https://www.gnu.org/licenses/agpl-3.0.html

  If the developer choses the Swiss Ephemeris Professional license,
  he must follow the instructions as found in http://www.astro.com/swisseph/ 
  and purchase the Swiss Ephemeris Professional Edition from Astrodienst
  and sign the corresponding license contract.

  The License grants you the right to use, copy, modify and redistribute
  Swiss Ephemeris, but only under certain conditions described in the License.
  Among other things, the License requires that the copyright notices and
  this notice be preserved on all copies.

  Authors of the Swiss Ephemeris: Dieter Koch and Alois Treindl

  The authors of Swiss Ephemeris have no control or influence over any of
  the derived works, i.e. over software or services created by other
  programmers which use Swiss Ephemeris functions.

  The names of the authors or of the copyright holder (Astrodienst) must not
  be used for promoting any software, product or service which uses or contains
  the Swiss Ephemeris. This copyright notice is the ONLY place where the
  names of the authors can legally appear, except in cases where they have
  given special permission in writing.

  The trademarks 'Swiss Ephemeris' and 'Swiss Ephemeris inside' may be used
  for promoting such software, products or services.
*/

#include "swephexp.h"
#include "configurable_sweephe4.h"
#include <string.h>

#define INVALID_BASE 2000000000L
#define EPBS (2 * NDB)       /* buffer size is 20 days */
#define EP_MIN_IX 2          /* load buffer when index below this */
#define EP_MAX_IX (EPBS - 4) /* load buffer when index above this */

TLS struct ephe4_data ephe4d = {
    FALSE, // the base path is not yet set
    NULL,  // no ephfp yet
    ""     // no base path yet
};
const int qod[EP_NP] = {5, 5, 5, 5, 5, 3, 3, 3, 3, 3, 3, 5, 3, 3, 3, 3};

static void inpolq_l(int n, int o, double p, centisec *x,
                     centisec *axu, centisec *adxu);
static int inpolq(int n, int o, double p, double *x,
                  double *axu, double *adxu);
static int ephe4_unpack(int jdl, int pflag, centisec lon[][EPBS], int i0,
                        char *errs);
static int ephe4_unpack_d(int jdl, int pflag, double lon[][EPBS], int i0,
                          char *errs);
static char *my_makepath(char *d, char *s);

#ifdef INTEL_BYTE_ORDER
/********************************************************************/
void shortreorder(UCHAR *p, int n)
/* p points to memory filled with 16-bit values; for
                           each of the values the seqeuence of the two bytes
                           has to be reversed, to translate HP-UX and VAX
			   ordering to MSDOS/Turboc ordering */
{
  int i;
  unsigned char c0;
  for (i = 0; i < n; i += 2, p += 2)
  {
    c0 = *p;
    *p = *(p + 1);
    *(p + 1) = c0;
  }
}
#endif

/****************************************************
  read ephe file and return a pointer to normalized positions
  for planets specified by pflag at julday jdl;
  If the reading failes, NULL is returned and
  an error text of max. 79 char in errtext, except when errtext = NULL.
  If calc is used and succeeds, a message is put into errtext, otherwise
  errtext is empty.
  Attention: jd is an absolute Julian date
  flag bits defined in sweephe4.h, onl
****************************************************/
centisec *ephread(double jd, int plalist, int flag, char *errtext)
{
  static TLS int jdbase = INVALID_BASE;
  static TLS int lastplalist = 0;
  static TLS centisec lon[EP_NP][EPBS]; /* buffer for 20 days unpacked ephe */
  static TLS centisec out[2 * EP_NP];   /* buffer for return longitude
					   and return speed */
  int p, pf;
  int ix, jdlong, iflagret;
  centisec clp;
  double jfract;
  double x[6];
  if (errtext != NULL)
    *errtext = '\0';
  if (plalist == 0)
    plalist = EP_ALL_BITS; /* default: all logitudes, no speeds */
  /*
   * we must determine when to reload the lon buffer: if the contents do
   * not allow immediate interpolation or if the plalist selection has
   * changed since the last call.
   */
  if ((plalist & lastplalist) != plalist)
  { /* new set is not contained in old */
    jdbase = INVALID_BASE;
  }
  lastplalist = plalist;
  jdlong = floor(jd - 0.5);
  ix = jdlong - jdbase;
  if (ix < EP_MIN_IX || ix >= EPBS)
  {                                              /* must reload full buffer */
    jdbase = ((jdlong - EP_MIN_IX) / NDB) * NDB; /* new base */
    if (jdbase > jdlong - EP_MIN_IX)
      jdbase -= NDB; /* fix bug for neg. */
    if (ephe4_unpack(jdbase, plalist, lon, 0, errtext) != OK)
      goto err_exit;
    if (ephe4_unpack(jdbase + NDB, plalist, lon, 0 + NDB, errtext) != OK)
      goto err_exit;
    ix = jdlong - jdbase;
  }
  else if (ix > EP_MAX_IX)
  {                /* must shift upper half down
					   and reload upper half of buffer */
    jdbase += NDB; /* new base */
    for (p = 0; p < EP_NP; p++)
      memcpy(&lon[p][0], &lon[p][NDB], NDB * sizeof(centisec));
    if (ephe4_unpack(jdbase + NDB, plalist, lon, 0 + NDB, errtext) != OK)
      goto err_exit;
    ix = jdlong - jdbase;
  }
  jfract = jd - 0.5 - jdlong;
  /*
   * we use the interpolator even for jfract = 0, because it delivers
   * the speed term. The computation overhead is unimportant
   * in any case.
   */
  for (p = 0, pf = 1; p < EP_NP; p++, pf = pf << 1)
    if ((plalist & pf) != 0)
    {
      inpolq_l((int)ix, qod[p], jfract, &(lon[p][0]), &(out[p]), &clp);
      if (p <= PLACALC_LILITH)
      { /* normalize all except ecl and nut */
        if (out[p] < 0)
          out[p] += DEG360;
        else if (out[p] >= DEG360)
          out[p] -= DEG360;
      }
#ifdef DEBUG
      fprintf(stderr, "ephread p=%d, lon=%.3lf\n", p, out[p] * CS2DEG);
#endif
      if (flag & EP_BIT_SPEED)
        out[p + EP_NP] = clp;
    }
  return out;
err_exit:
  jdbase = INVALID_BASE;
  lastplalist = 0;
  if ((flag & EP_BIT_MUST_USE_EPHE) == 0)
  { /* try using calc */
    int sweflag = 0;
    char serr[AS_MAXCH];
    if (flag & EP_BIT_SPEED)
      sweflag = SEFLG_SPEED;
    if (errtext != NULL)
      sprintf(errtext, "ephread failed for jd=%f; used swe_calc().", jd);
    for (p = 0, pf = 1; p < PLACALC_CALC_N; p++, pf = pf << 1)
    {
      if ((plalist & pf) != 0)
      {
        if ((iflagret = swe_calc(jd, ephe_plac2swe(p), sweflag, x, serr)) != ERR)
        {
          out[p] = swe_d2l(x[0] * DEG);
          if (flag & EP_BIT_SPEED)
            out[p + EP_NP] = swe_d2l(x[3] * DEG);
          if (out[p] < 0)
            out[p] += DEG360;
          else if (out[p] >= DEG360)
            out[p] -= DEG360;
        }
        else
        {
          swe_close();
          if (errtext != NULL)
            strcat(errtext, " calc failed too.");
          return NULL;
        }
      }
    }
    if ((iflagret = swe_calc(jd, SE_ECL_NUT, 0, x, serr)) == ERR)
    {
      swe_close();
      sprintf(errtext, "error in swe_calc() %s\n", serr);
      return NULL;
    }
    out[EP_ECL_INDEX] = swe_d2l(x[0] * DEG); /* true ecliptic */
    out[EP_NUT_INDEX] = swe_d2l(x[2] * DEG); /* nutation */
    out[EP_ECL_INDEX + EP_NP] = 0;
    out[EP_NUT_INDEX + EP_NP] = 0;
    return out;
  }
  return NULL;
} /* ephread */

// same in double
double *dephread2(double jd, int plalist, int flag, char *errtext)
{
  static TLS int jdbase = INVALID_BASE;
  static TLS int lastplalist = 0;
  static TLS double lon[EP_NP][EPBS]; // buffer for 20 days unpacked ephe
  static TLS double out[2 * EP_NP];   // buffer for return longitude and return speed
  int p, pf;
  int ix, jdlong, iflagret;
  double lp;
  double jfract;
  double x[6];
  if (errtext != NULL)
    *errtext = '\0';
  if (plalist == 0)
    plalist = EP_ALL_BITS; /* default: all logitudes, no speeds */
  /*
   * we must determine when to reload the lon buffer: if the contents do
   * not allow immediate interpolation or if the plalist selection has
   * changed since the last call.
   */
  if ((plalist & lastplalist) != plalist)
  { /* new set is not contained in old */
    jdbase = INVALID_BASE;
  }
  lastplalist = plalist;
  jdlong = floor(jd - 0.5);
  ix = jdlong - jdbase;
  if (ix < EP_MIN_IX || ix >= EPBS)
  {                                              /* must reload full buffer */
    jdbase = ((jdlong - EP_MIN_IX) / NDB) * NDB; /* new base */
    if (jdbase > jdlong - EP_MIN_IX)
      jdbase -= NDB; /* fix bug for neg. */
    if (ephe4_unpack_d(jdbase, plalist, lon, 0, errtext) != OK)
      goto err_exit;
    if (ephe4_unpack_d(jdbase + NDB, plalist, lon, 0 + NDB, errtext) != OK)
      goto err_exit;
    ix = jdlong - jdbase;
  }
  else if (ix > EP_MAX_IX)
  {                /* must shift upper half down
					   and reload upper half of buffer */
    jdbase += NDB; /* new base */
    for (p = 0; p < EP_NP; p++)
      memcpy(&lon[p][0], &lon[p][NDB], NDB * sizeof(double));
    if (ephe4_unpack_d(jdbase + NDB, plalist, lon, 0 + NDB, errtext) != OK)
      goto err_exit;
    ix = jdlong - jdbase;
  }
  jfract = jd - 0.5 - jdlong;
  /*
   * we use the interpolator even for jfract = 0, because it delivers
   * the speed term. The computation overhead is unimportant
   * in any case.
   */
  for (p = 0, pf = 1; p < EP_NP; p++, pf = pf << 1)
    if ((plalist & pf) != 0)
    {
      inpolq((int)ix, qod[p], jfract, &(lon[p][0]), &(out[p]), &lp);
      if (p <= PLACALC_LILITH)
      { /* normalize all except ecl and nut */
        if (out[p] < 0)
          out[p] += 360.0;
        else if (out[p] >= 360.0)
          out[p] -= 360.0;
      }
#ifdef DEBUG
      fprintf(stderr, "ephread p=%d, lon=%.3lf\n", p, out[p]);
#endif
      if (flag & EP_BIT_SPEED)
        out[p + EP_NP] = lp;
    }
  return out;
err_exit:
  jdbase = INVALID_BASE;
  lastplalist = 0;
  if ((flag & EP_BIT_MUST_USE_EPHE) == 0)
  { /* try using calc */
    int sweflag = 0;
    char serr[AS_MAXCH];
    if (flag & EP_BIT_SPEED)
      sweflag = SEFLG_SPEED;
    if (errtext != NULL)
      sprintf(errtext, "ephread failed for jd=%f; used swe_calc().", jd);
    for (p = 0, pf = 1; p < PLACALC_CALC_N; p++, pf = pf << 1)
    {
      if ((plalist & pf) != 0)
      {
        if ((iflagret = swe_calc(jd, ephe_plac2swe(p), sweflag, x, serr)) != ERR)
        {
          out[p] = x[0];
          if (flag & EP_BIT_SPEED)
            out[p + EP_NP] = x[3];
          if (out[p] < 0)
            out[p] += 360.0;
          else if (out[p] >= 360.0)
            out[p] -= 360.0;
        }
        else
        {
          swe_close();
          if (errtext != NULL)
            strcat(errtext, " calc failed too.");
          return NULL;
        }
      }
    }
    if ((iflagret = swe_calc(jd, SE_ECL_NUT, 0, x, serr)) == ERR)
    {
      swe_close();
      sprintf(errtext, "error in swe_calc() %s\n", serr);
      return NULL;
    }
    out[EP_ECL_INDEX] = x[0]; /* true ecliptic */
    out[EP_NUT_INDEX] = x[2]; /* nutation */
    out[EP_ECL_INDEX + EP_NP] = 0;
    out[EP_NUT_INDEX + EP_NP] = 0;
    return out;
  }
  return NULL;
}

/****************************************************
  unpack an ephe file block specified by jlong 
  and the planets specified by pflag into
  the array lon[p][EPBS], starting at index i0.
  jdl is (long) floor(full julian date);
  If the reading failes, ERR is returned and
  an error text of max. 79 char in errs, except when errs = NULL.
****************************************************/
static int ephe4_unpack(int jdl, int plalist, centisec lon[][EPBS], int i0, char *errs)
{
  int p, i, pf;
  centisec l_ret, d_ret;
  struct ep4 e;
  if (eph4_posit(jdl, FALSE, errs) != OK)
    return (ERR);
  if (fread(&e, sizeof(struct ep4), 1, ephe4d.ephfp) != 1)
  {
    if (errs != NULL)
      sprintf(errs, "ephe4_unpack: fread for jd=%d failed", jdl);
    return (ERR);
  }
#ifdef INTEL_BYTE_ORDER
  shortreorder((UCHAR *)&e, sizeof(struct ep4));
#endif
  for (p = PLACALC_SUN, pf = 1; p <= PLACALC_LILITH; p++, pf = pf << 1)
  {
    if ((plalist & pf) == 0)
      continue;
    l_ret = e.elo[p].p0m * 6000L + e.elo[p].p0s;   /* csec */
    d_ret = e.elo[p].pd1m * 6000L + e.elo[p].pd1s; /* csec */
    lon[p][i0] = l_ret;
    l_ret += d_ret;
    if (l_ret < 0)
    {
      lon[p][i0 + 1] = l_ret + DEG360;
    }
    else if (l_ret >= DEG360)
    {
      lon[p][i0 + 1] = l_ret - DEG360;
    }
    else
    {
      lon[p][i0 + 1] = l_ret;
    }
    for (i = 2; i < NDB; i++)
    {
      if (p == PLACALC_MOON || p == PLACALC_MERCURY)
        d_ret += e.elo[p].pd2[i - 2] * 10L;
      else
        d_ret += e.elo[p].pd2[i - 2];
      l_ret += d_ret;
      if (l_ret < 0)
      {
        lon[p][i0 + i] = l_ret + DEG360;
      }
      else if (l_ret >= DEG360)
      {
        lon[p][i0 + i] = l_ret - DEG360;
      }
      else
      {
        lon[p][i0 + i] = l_ret;
      }
    }
  } /* for p */
  if (plalist & EP_ECL_BIT)
  { /* unpack ecl */
    l_ret = e.ecl0m * 6000L + e.ecl0s;
    lon[EP_ECL_INDEX][i0] = l_ret;
    for (i = 1; i < NDB; i++)
      lon[EP_ECL_INDEX][i0 + i] = l_ret + e.ecld1[i - 1];
  }
  if (plalist & EP_NUT_BIT)
  { /* unpack nut */
    for (i = 0; i < NDB; i++)
      lon[EP_NUT_INDEX][i0 + i] = e.nuts[i];
  }
  return OK;
} /* ephe4_unpack */

// same in double
static int ephe4_unpack_d(int jdl, int plalist, double lon[][EPBS], int i0, char *errs)
{
  int p, i, pf;
  double l_ret, d_ret;
  struct ep4 e;
  if (eph4_posit(jdl, FALSE, errs) != OK)
    return (ERR);
  if (fread(&e, sizeof(struct ep4), 1, ephe4d.ephfp) != 1)
  {
    if (errs != NULL)
      sprintf(errs, "ephe4_unpack: fread for jd=%d failed", jdl);
    return (ERR);
  }
#ifdef INTEL_BYTE_ORDER
  shortreorder((UCHAR *)&e, sizeof(struct ep4));
#endif
  for (p = PLACALC_SUN, pf = 1; p <= PLACALC_LILITH; p++, pf = pf << 1)
  {
    if ((plalist & pf) == 0)
      continue;
    l_ret = (e.elo[p].p0m * 6000 + e.elo[p].p0s) * CS2DEG;
    d_ret = (e.elo[p].pd1m * 6000 + e.elo[p].pd1s) * CS2DEG;
    lon[p][i0] = l_ret;
    l_ret += d_ret;
    if (l_ret < 0)
    {
      lon[p][i0 + 1] = l_ret + 360.0;
    }
    else if (l_ret >= 360.0)
    {
      lon[p][i0 + 1] = l_ret - 360.0;
    }
    else
    {
      lon[p][i0 + 1] = l_ret;
    }
    for (i = 2; i < NDB; i++)
    {
      if (p == PLACALC_MOON || p == PLACALC_MERCURY)
        d_ret += (e.elo[p].pd2[i - 2] * 10 * CS2DEG);
      else
        d_ret += (e.elo[p].pd2[i - 2] * CS2DEG);
      l_ret += d_ret;
      if (l_ret < 0)
      {
        lon[p][i0 + i] = l_ret + 360.0;
      }
      else if (l_ret >= 360.0)
      {
        lon[p][i0 + i] = l_ret - 360.0;
      }
      else
      {
        lon[p][i0 + i] = l_ret;
      }
    }
  } /* for p */
  if (plalist & EP_ECL_BIT)
  { /* unpack ecl */
    l_ret = (e.ecl0m * 6000L + e.ecl0s) * CS2DEG;
    lon[EP_ECL_INDEX][i0] = l_ret;
    for (i = 1; i < NDB; i++)
      lon[EP_ECL_INDEX][i0 + i] = l_ret + e.ecld1[i - 1] * CS2DEG;
  }
  if (plalist & EP_NUT_BIT)
  { /* unpack nut */
    for (i = 0; i < NDB; i++)
      lon[EP_NUT_INDEX][i0 + i] = e.nuts[i] * CS2DEG;
  }
  return OK;
}

/****************************************************
  position ephe file at proper position for julian 
  date jd; if writeflag = TRUE (write mode), create file
  if required. Return OK or ERR.
  globals used: ephe4d.ephfp.
*****************************************************/
int eph4_posit(int jlong, AS_BOOL writeflag, char *errtext)
{
  int filenr;
  long posit;
  /* NOTE(luis): making this `__thread` (thread-local)
    since it would otherwise mislead `fseek`: if it is set
    as a valid file number, but the global file pointer
    isn't set yet (e.g. another thread set it,) then
    it would try to `fseek` on a NULL file pointer.
    I could put this in `ephe4d`, but didn't want to mess
    with the logic here _too_ much.
  */
  static TLS int open_filenr = -10000;
  char fname[AS_MAXCH], s[AS_MAXCH], *sp;
  int nchars;

  /* ensure that we've set the base path. */
  if (!ephe4d.ephe4_path_is_set)
  {
    ephe4_set_ephe_path(NULL);
  }

  filenr = jlong / EP4_NDAYS;
  if (jlong < 0 && filenr * EP4_NDAYS != jlong)
    filenr--;
  posit = jlong - filenr * EP4_NDAYS;
  posit = posit / NDB * sizeof(struct ep4);
  if (open_filenr != filenr)
  {
    if (ephe4d.ephfp != NULL)
    {
      fclose(ephe4d.ephfp);
      open_filenr = -10000;
    }
    if (filenr >= 0)
      sprintf(s, "%s%s%d", ephe4d.ephe4path, EP4_FILE, filenr);
    else
      sprintf(s, "%s%sM%d", ephe4d.ephe4path, EP4_FILE, -filenr);
    my_makepath(fname, s);
    if (writeflag)
      sp = BFILE_W_CREATE;
    else
      sp = BFILE_R_ACCESS;
    ephe4d.ephfp = fopen(fname, sp);
    if (ephe4d.ephfp == NULL)
    {
      if (errtext != NULL)
      {

        if (!writeflag)
        {
          nchars = snprintf(errtext, AS_MAXCH, "eph4_posit: file %s does not exist\n", fname);
          if (nchars >= AS_MAXCH)
          {
            sprintf(errtext, "eph4_posit: file does not exist\n");
          }
        }
        else
        {
          nchars = snprintf(errtext, AS_MAXCH, "eph4_posit: could not create file %s\n", fname);
          if (nchars >= AS_MAXCH)
          {
            sprintf(errtext, "eph4_posit: file does not exist\n");
          }
        }
      }
      return (ERR);
    }
    open_filenr = filenr;
  }
  if (fseek(ephe4d.ephfp, posit, 0) == 0 && ftell(ephe4d.ephfp) == posit)
  {
    return (OK);
  }
  else
  {
    if (errtext != NULL)
      sprintf(errtext, "eph4_posit: fseek(%ld) of file nr %d failed\n",
              posit, open_filenr);
    return (ERR);
  }
} /* end eph4_posit */

/*****************************************************
quicker Everett interpolation, after Pottenger
version  for long, 17.7.91 by Alois Treindl
*****************************************************/
static void inpolq_l(int n, int o, double p, centisec *x, centisec *axu, centisec *adxu)
/* 
 * interpolate between x[n] and x[n-1], at argument n+p 
 * o = order of interpolation, maximum 5 
 * p = argument in [0..1] 
 * x[] array of function values, x[n-2]..x[n+3] must exist 
 * axu pointer for storage of result 
 * adxu pointer for storage of dx/dt  
 */
{
  static TLS double q, q2, q3, q4, q5,
      p2, p3, p4, p5,
      u, u0, u1, u2;
  static TLS double lastp = 9999;
  double rl, rlp;
  centisec dm2, dm1, d0, dp1, dp2,
      d2m1, d20, d2p1, d2p2,
      d30, d3p1, d3p2,
      d4p1, d4p2;
  centisec offset = 0;
  if (lastp != p)
  { /* recompute the interpolator factors */
    q = 1.0 - p;
    q2 = q * q;
    q3 = (q + 1.0) * q * (q - 1.0) / 6.0; /* q - 1  over 3; u5 */
    p2 = p * p;
    p3 = (p + 1.0) * p * (p - 1.0) / 6.0; /* p - 1  over 3; u8 */
    u = (3.0 * p2 - 1.0) / 6.0;
    u0 = (3.0 * q2 - 1.0) / 6.0;
    q4 = q2 * q2;                              /* f5 */
    p4 = p2 * p2;                              /* f4 */
    u1 = (5.0 * p4 - 15.0 * p2 + 4.0) / 120.0; /* u1 */
    u2 = (5.0 * q4 - 15.0 * q2 + 4.0) / 120.0; /* u2 */
    q5 = q3 * (q + 2.0) * (q - 2.0) / 20.0;    /* q - 2  over 5; u6 */
    p5 = (p + 2.0) * p3 * (p - 2.0) / 20.0;    /* p - 2  over 5; u9 */
    lastp = p;
  }
  dm1 = x[n] - x[n - 1];
  if (dm1 >= DEG180)
    dm1 -= DEG360;
  else if (dm1 < -DEG180)
    dm1 += DEG360;
  d0 = x[n + 1] - x[n];
  if (d0 >= DEG180)
  {
    d0 -= DEG360;
    offset = DEG360;
  }
  else if (d0 < -DEG180)
  {
    d0 += DEG360;
    offset = -DEG360;
  }
  dp1 = x[n + 2] - x[n + 1];
  if (dp1 >= DEG180)
    dp1 -= DEG360;
  else if (dp1 < -DEG180)
    dp1 += DEG360;
  d20 = d0 - dm1;  /* f8 */
  d2p1 = dp1 - d0; /* f9 */
  /*
   * Everett interpolation 3rd order
   */
  rl = q * (x[n] + offset) + q3 * d20 + p * x[n + 1] + p3 * d2p1;
  rlp = d0 + u * d2p1 - u0 * d20;
  if (o > 3)
  { /* 5th order */
    dm2 = x[n - 1] - x[n - 2];
    if (dm2 >= DEG180)
      dm2 -= DEG360;
    else if (dm2 < -DEG180)
      dm2 += DEG360;
    dp2 = x[n + 3] - x[n + 2];
    if (dp2 >= DEG180)
      dp2 -= DEG360;
    else if (dp2 < -DEG180)
      dp2 += DEG360;
    d2m1 = dm1 - dm2;
    d2p2 = dp2 - dp1;
    d30 = d20 - d2m1;
    d3p1 = d2p1 - d20;
    d3p2 = d2p2 - d2p1;
    d4p1 = d3p1 - d30;  /* f7 */
    d4p2 = d3p2 - d3p1; /* f */
    rl += p5 * d4p2 + q5 * d4p1;
    rlp += u1 * d4p2 - u2 * d4p1;
  }
  *axu = swe_d2l(rl);
  *adxu = swe_d2l(rlp);
} /* end inpolq_l() */

/*****************************************************
quicker Everett interpolation, after Pottenger
version for double  9 Jul 1988    by Alois Treindl
return OK, no error checking
Was used in Placalc to interpolate 80-day stored ephe for outer planets.
*****************************************************/
static int inpolq(int n, int o, double p, double *x, double *axu, double *adxu)
// n	interpolate between x[n] and x[n-1], at argument n+p
// o	order of interpolation, maximum 5
// p,	argument , intervall [0..1]
// x[]	array of function values, x[n-o]..x[n+o] must exist
// *axu	pointer for storage of result
// *adxu pointer for storage of dx/dt
{
  static TLS double q, q2, q3, q4, q5, p2, p3, p4, p5, u, u0, u1, u2;
  static TLS double lastp = 9999.0;
  double dm2, dm1, d0, dp1, dp2,
      d2m1, d20, d2p1, d2p2,
      d30, d3p1, d3p2,
      d4p1, d4p2;
  double offset = 0.0;
  if (lastp != p)
  {
    q = 1.0 - p;
    q2 = q * q;
    q3 = (q + 1.0) * q * (q - 1.0) / 6.0; /* q - 1  over 3; u5 */
    p2 = p * p;
    p3 = (p + 1.0) * p * (p - 1.0) / 6.0; /* p - 1  over 3; u8 */
    u = (3.0 * p2 - 1.0) / 6.0;
    u0 = (3.0 * q2 - 1.0) / 6.0;
    q4 = q2 * q2;                              /* f5 */
    p4 = p2 * p2;                              /* f4 */
    u1 = (5.0 * p4 - 15.0 * p2 + 4.0) / 120.0; /* u1 */
    u2 = (5.0 * q4 - 15.0 * q2 + 4.0) / 120.0; /* u2 */
    q5 = q3 * (q + 2.0) * (q - 2.0) / 20.0;    /* q - 2  over 5; u6 */
    p5 = (p + 2.0) * p3 * (p - 2.0) / 20.0;    /* p - 2  over 5; u9 */
    lastp = p;
  }
  dm1 = x[n] - x[n - 1];
  if (dm1 > 180.0)
    dm1 -= 360.0;
  if (dm1 < -180.0)
    dm1 += 360.0;
  d0 = x[n + 1] - x[n];
  if (d0 > 180.0)
  {
    d0 -= 360.0;
    offset = 360.0;
  }
  if (d0 < -180.0)
  {
    d0 += 360.0;
    offset = -360.0;
  }
  dp1 = x[n + 2] - x[n + 1];
  if (dp1 > 180.0)
    dp1 -= 360.0;
  if (dp1 < -180.0)
    dp1 += 360.0;
  d20 = d0 - dm1;  /* f8 */
  d2p1 = dp1 - d0; /* f9 */
  /* Everett interpolation 3rd order */
  *axu = q * (x[n] + offset) + q3 * d20 + p * x[n + 1] + p3 * d2p1;
  *adxu = d0 + u * d2p1 - u0 * d20;
  if (o > 3)
  { /* 5th order */
    dm2 = x[n - 1] - x[n - 2];
    if (dm2 > 180.0)
      dm2 -= 360.0;
    if (dm2 < -180.0)
      dm2 += 360.0;
    dp2 = x[n + 3] - x[n + 2];
    if (dp2 > 180.0)
      dp2 -= 360.0;
    if (dp2 < -180.0)
      dp2 += 360.0;
    d2m1 = dm1 - dm2;
    d2p2 = dp2 - dp1;
    d30 = d20 - d2m1;
    d3p1 = d2p1 - d20;
    d3p2 = d2p2 - d2p1;
    d4p1 = d3p1 - d30;  /* f7 */
    d4p2 = d3p2 - d3p1; /* f */
    *axu += p5 * d4p2 + q5 * d4p1;
    *adxu += u1 * d4p2 - u2 * d4p1;
  }
  return (OK);
} /* end inpolq() */

static char *my_makepath(char *d, char *s)
{
  char *getenv();
  if (*s == *DIR_GLUE || *s == '/' || strchr(s, ':') != NULL)
  {
    strcpy(d, s); /* s is absolute path name */
  }
#if MSDOS
  while ((p = strchr(d, '/')) != NULL)
    *p = '\\';
#endif
  return (d);
}

int ephe_plac2swe(int p)
{
  if (p >= PLACALC_SUN && p <= PLACALC_TRUE_NODE)
    return p;
  if (p == PLACALC_CHIRON)
    return SE_CHIRON;
  if (p == PLACALC_LILITH)
    return SE_MEAN_APOG;
  if (p == PLACALC_CERES)
    return SE_CERES;
  if (p == PLACALC_PALLAS)
    return SE_PALLAS;
  if (p == PLACALC_JUNO)
    return SE_JUNO;
  if (p == PLACALC_VESTA)
    return SE_VESTA;
  if (p == PLACALC_EARTHHEL)
    return SE_EARTH;
  return -1;
}

void CALL_CONV ephe4_set_ephe_path(char *path)
{
  int i;
  char s[AS_MAXCH];
  char *sp;

  /* Attempt to set the path from:
     * The EP4_PATH environment variable.
     * The provided path.
     * If invalid, or NULL, from the EP4_PATH macro
  */
  if ((sp = getenv("EP4_PATH")) != NULL && strlen(sp) != 0 && strlen(sp) <= AS_MAXCH - 1 - 13)
  {
    strcpy(s, sp);
  }
  else if (path == NULL || *path == '\0')
  {
    strcpy(s, EP4_PATH);
  }
  else if (strlen(path) <= AS_MAXCH - 1 - 13)
  {
    strcpy(s, path);
  }
  else
  {
    strcpy(s, EP4_PATH);
  }

  /* ensure that the path separator (dir glue)
   * appears at the end of the string.
  */
  i = (int)strlen(s);
  if (*(s + i - 1) != *DIR_GLUE && *s != '\0')
  {
    strcat(s, DIR_GLUE);
  }

  /* assert that the path is set, 
     copy to the thread-global state: */
  ephe4d.ephe4_path_is_set = TRUE;
  strcpy(ephe4d.ephe4path, s);
}

static int split(w, m, min, sec)
    int32 w; /* position in seconds/m */
int m;       /* factor for seconds */
short *min,  /* storage for degrees and minutes */
    *sec;    /* storage for seconds * m */
{
  if (w >= 0)
  {
    *sec = w % (60 * m);
    *min = w / (60 * m);
  }
  else
  {
    *sec = -(-w % (60 * m));
    *min = -(-w / (60 * m));
  }
  return OK;
}

/*************************************************************
Pack positions of 10 days and write to file
The longitude is packed with second differences in such a way,
that the accumulating rounding erros do not exceed half of
the last stored digit, i.e. 0.05" moon, 0.005" other planets
**************************************************************/
static int eph4_pack_opt(int32 jd, double (*l)[NDB], double ecliptic[],
                  double nutation[], AS_BOOL verbose)
{
  int i, p, ps;
  int32 d1, d2, dd, d_ret, w0, w_ret;
  double err;
  struct ep4 e;
  static TLS int32 max_dd[EP_CALC_N];
  static TLS double max_err[EP_CALC_N];

  e.j_10000 = jd / 10000.0;
  e.j_rest = jd - 10000.0 * e.j_10000;
  w0 = swe_d2l(ecliptic[0] * DEG);
  split(w0, 100, &e.ecl0m, &e.ecl0s);
  for (i = 1; i < NDB; i++)
    e.ecld1[i - 1] = swe_d2l(ecliptic[i] * DEG - w0);
  for (i = 0; i < NDB; i++)
    e.nuts[i] = swe_d2l(nutation[i] * DEG); /* int32 casted into short */
  for (p = PLACALC_SUN; p <= PLACALC_LILITH; p++)
  {
    ps = p;
    w0 = swe_d2l(l[ps][0] * DEG);
    d1 = swe_d2l(l[ps][1] * DEG - w0);
    if (d1 >= DEG180)
      d1 -= DEG360;
    else if (d1 <= -DEG180)
      d1 += DEG360;
    split(w0, 100, &e.elo[p].p0m, &e.elo[p].p0s);
    split(d1, 100, &e.elo[p].pd1m, &e.elo[p].pd1s);
    d_ret = d1;         /* recalculated diff */
    w_ret = w0 + d_ret; /* recalculated position */
    for (i = 2; i < NDB; i++)
    {
      d2 = swe_d2l(l[ps][i] * DEG - w_ret);
      if (d2 >= DEG180)
        d2 -= DEG360;
      else if (d2 <= -DEG180)
        d2 += DEG360;
      dd = d2 - d_ret; /* second difference */
      if (p == PLACALC_MOON || p == PLACALC_MERCURY)
        dd = swe_d2l(dd / 10.0); /* moon only 0.1" */
      if (verbose && abs(dd) > abs(max_dd[ps]))
        max_dd[ps] = dd;
      e.elo[p].pd2[i - 2] = dd;
      if (p == PLACALC_MOON || p == PLACALC_MERCURY)
        d_ret += e.elo[p].pd2[i - 2] * 10L;
      else
        d_ret += e.elo[p].pd2[i - 2];
      w_ret += d_ret;
      if (verbose)
      {
        err = swe_difdeg2n(w_ret / 360000.0, l[ps][i]); /* error */
        if (fabs(err) > fabs(max_err[ps]))
          max_err[ps] = err;
      }
    }
  } /* for p */
#ifdef INTEL_BYTE_ORDER
  shortreorder((UCHAR *)&e, sizeof(struct ep4));
#endif
  fwrite(&e, sizeof(struct ep4), 1, ephe4d.ephfp);
  return (OK);
}

int CALL_CONV ephe4_write_file(int fnr, char *errtext)
{
  int day, n, p;
  char serr[AS_MAXCH];
  double l[EP_CALC_N][NDB], ecliptic[NDB], nutation[NDB];
  double jd0, jd;
  double x[6];
  int32 jlong;
  //int fnr = -10000;
  int32 iflagret;

  if (errtext != NULL)
  {
    *errtext = '\0';
  }
  
  if (fnr < -20 || fnr > 300)
  {
    snprintf(errtext, AS_MAXCH, "invalid starting file number %d", fnr);
    return (ERR);
  }

  jd0 = EP4_NDAYS * fnr + 0.5;
  jlong = floor(jd0);
  if (eph4_posit(jlong, TRUE, errtext) != OK)
  {
    return (ERR);
  }
  for (n = 0; n < EP4_NDAYS; n += NDB, jd0 += NDB)
  {
    for (day = 0; day < NDB; day++)
    { /* compute positions for 10 days */
      jd = jd0 + day;
      for (p = PLACALC_SUN; p <= EP_CALC_N; p++)
      {
        if ((iflagret = swe_calc(jd, ephe_plac2swe(p), 0, x, serr)) == ERR)
        {
          swe_close();
          snprintf(errtext, AS_MAXCH, "error in swe_calc(): %s", serr);
          return (ERR);
        }
        l[p][day] = x[0];
      }
      if ((iflagret = swe_calc(jd, SE_ECL_NUT, 0, x, serr)) == ERR)
      {
        swe_close();
        snprintf(errtext, AS_MAXCH, "error in swe_calc(): %s", serr);
        return (ERR);
      }
      ecliptic[day] = x[0];
      nutation[day] = x[2];
    }
    jlong = floor(jd0);
    // NOTE(luis) not sure if the `verbose` option is even useful here
    eph4_pack_opt(jlong, l, ecliptic, nutation, FALSE);
  }
  fclose(ephe4d.ephfp);
  ephe4d.ephfp = NULL;
  //swe_close();
  return (OK);
}
