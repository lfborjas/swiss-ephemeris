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

/* NOTE(luis) this file was shared in the forum by Alois, but not yet
   made officially part of the repo. I reproduce here with the current license
   disclaimer (above) and the disclaimer in the forum (below)

   retrieved from:

   https://groups.io/g/swisseph/message/5564 
   and
   https://groups.io/g/swisseph/message/5566 

   on June 18, 2021.
*/



/*
All copyright Alois Treindl and Astrodienst AG.
Published under GNU public license version 2 or later.

If a developer uses this code or the techniques expressed in it, he or she must fulfill the conditions of that license, which includes the obligation to place his or her whole software project under the GNU GPL or a compatible license.
See http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
This notice must not removed.
The license applies also to translation of this code into another language than C.
*/

/* the units for positions are usually centiseconds, i.e.
   degrees * 360000.
   This allows for integer calculation, a speed advantage in the year 1998, when the code was originally written */
#include "sweodef.h"

#define MAX_OBJ_SHAKE   11
#define VERT_FACT       (2 / (3.0 * 3.0))
#define INF_WEIGHT      0.001
#define PENALTY         1000


typedef int   int32;
typedef int32    centisec;       /* centiseconds used for angles and times */
#define CS      (centisec)      /* use for casting */
#define CSEC    centisec        /* use for typing */


#define DEG     360000  /* degree expressed in centiseconds */
#define DEG7_30 (2700000)       /* 7.5 degrees */
#define DEG3_45 (1350000)       /* 3.75 degrees */
#define DEG15   (15 * DEG)
#define DEG24   (24 * DEG)
#define DEG30   (30 * DEG)
#define DEG60   (60 * DEG)
#define DEG90   (90 * DEG)
#define DEG120  (120 * DEG)
#define DEG150  (150 * DEG)
#define DEG180  (180 * DEG)
#define DEG270  (270 * DEG)
#define DEG360  (360 * DEG)


#define DEG2MSEC 3600000.0      /* degree to milliseconds */
#define DEG2CSEC 360000.0       /* degree to centiseconds */

#define SEC2CSEC        100     /* seconds to centiseconds */

#define CS2DEG  (1.0/360000.0)  /* centisec to degree */
#define CS2CIRCLE (CS2DEG/360.0)        /* centisec to circle */
#define AU2INT   1e7            /* factor for long storage of A.U. */

#define CSMIN   6000
#define CSSEC   100

# define SINDEG(x)      sin(DEGTORAD * (x))
# define COSDEG(x)      cos(DEGTORAD * (x))
# define TANDEG(x)      tan(DEGTORAD * (x))
# define SINCS(x)       sin((double)(CSTORAD * (x)))
# define COSCS(x)       cos((double)(CSTORAD * (x)))
# define TANCS(x)       tan((double)(CSTORAD * (x)))


typedef struct grav_object {
        int32 pos;      /* original position */
        int32 lsize;    /* left size of element */
        int32 rsize;    /* right size of element */
        int32 ppos;     /* placed position, output */
        int  sector_no; /* in which sector is the element */
        int  sequence_no;       /* original sequence number */
        int  level_no;  /* for two_level sorting, level nr. 0 or 1 */
        double  scale;  /* gives acceptable size in % of full size */
        void *dp;       /* pointer to other data */
        } GROB; /* gravity object */

typedef struct gravity_group {
        GROB *fp;
        int  n, n_on_level;
        int32 width;
        int32 cgrav;
        AS_BOOL calc_levels;
        } group;

static int grav_group_sector(GROB *grobs, int nob, int32 *sbdy, AS_BOOL may_shift, int nlevels, char *err);
static int sort_sector(GROB *grobs, int nob);
static int grav_group_shaker(GROB *grobs, int nob, int32 *sbdy, AS_BOOL may_shift, char *err);


int grob_compare(const GROB *g1, const GROB *g2);
int grav_group(GROB *grobs, int nob, int32 *sbdy, int nsectors, char *err);
int grav_group2(GROB *grobs, int nob, int32 *sbdy, int nsectors, AS_BOOL may_shift, char *err);
