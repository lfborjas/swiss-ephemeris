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

// As published in:
// https://groups.io/g/swisseph/message/10076

/* the units for positions are usually centiseconds, i.e.
   degrees * 360000.
   This allows for integer calculation, a speed advantage in the year 1998, when the code was originally written */

#include "swephexp.h"
#include "swephlib.h"
#include "dgravgroup.h"

int grob_compare(const GROB *g1, const GROB *g2)
{
  return (int)(g1->pos - g2->pos);
}

/*
 * The elements are placed in a sector if they have lpos >= lbdy
 * and lpos < rbdy[sector].
 * If an element does not fit into a sector, it is placed at the lowest
 * or highest boundary.
 * sector_no is set to -1 and ERR is returned from the call.
 *
 * The elements are placed at sbdy + lsize;
 * There must be nsector+1 sbdy values.
 *
 * If we need some more empty space at a sector edge, we just introduce
 * a dummy element at the sector edge which holds this space.
 *
 * Elements in a circle may have different sizes dependent on the angle
 * where they are put. The caller must set these sizes approximately
 * correct.
 *
 * In case of error, an error message is put into err and ERR is returned.
 */
int grav_group(GROB *grobs, int nob, double *sbdy, int nsectors, char *err)
{
  int i, no, ns, n, nr_in_sec, ng;
  int retval = OK;
  double width_in_sec, pos;
  double scale, x;
  GROB *op, *first_op, *cop;
  group *grp, *gp;
  first_op = grobs;     /* to silence gcc */
  if (nob < 1 ) {
    sprintf(err,"grav_group: %d elements.", nob);
    return ERR;
  }
  if (nsectors < 1 ) {
    sprintf(err,"grav_group: %d sectors.", nsectors);
    return ERR;
  }
  /*
   * put the original sequence numbers into the elements, then
   * sort the elements by their position;
   * It is really expected that sectors are presorted correctly.
   */
  for (i = 0, op = grobs; i < nob; i++, op++) {
    if (op->pos < sbdy[0]) {
      op->pos = sbdy[0];
      sprintf(err,"element %d is below first sector.", i);
      retval = ERR;
    } else if (op->pos > sbdy[nsectors]) {
     op->pos = sbdy[nsectors] - 1;
      sprintf(err,"element %d is above last sector.", i);
      retval = ERR;
    } else if (op->pos == sbdy[nsectors]) {
      op->pos = sbdy[nsectors] - 1;     /* force it inside */
    }
    /*
     * find the sector for each element
     */
    for (ns = 1; ns <= nsectors; ns++)
      if (op->pos < sbdy[ns]) {
        op->sector_no = ns - 1;
        break;
      }
    op->sequence_no = i;
    op->scale = 1.0;
  }
  qsort(grobs, (size_t) nob, sizeof(GROB),
    (int (*)(const void *, const void *))(grob_compare));
  /* now we have a sorted array of objects */
  for (ns = 0; ns < nsectors; ns++) {
    /*
     * count the objects in this sector,
     * add their total width,
     * find the first element in this sector.
     */
    nr_in_sec = 0;
    width_in_sec = 0.0;
    for (no = 0, op = grobs; no < nob; no++, op++) {
      if (op->sector_no < ns) continue;
      if (op->sector_no > ns) break;
      if (nr_in_sec == 0)
        first_op = op;  /* first object in sector */
      nr_in_sec++;
      width_in_sec += op->lsize + op->rsize;
    }
    if (nr_in_sec == 0) continue;       /* empty sector */
    /*
     * if the objects don't fit into the sector, adjust their scale.
     */
    if (width_in_sec > sbdy[ns+1] - sbdy[ns]) {
      scale = 1.0 * (sbdy[ns+1] - sbdy[ns]) / width_in_sec;
      for (no = 0, op = first_op; no < nr_in_sec; no++, op++)
        op->scale = scale;
    }
    /*
     * allocate space for the maximum number of possible groups
     */
    grp = calloc(nr_in_sec, sizeof(group));
    /*
     * go through all objects in sector, start with group 0
     */
    for (ng = 0, gp = grp, cop = first_op, no = 0; no < nr_in_sec; ng++, gp++, cop++, no++) {
      gp->n = 1;        /* start new group, one object in it */
      gp->fp =cop;
      /*
       * compute width and sector of gravity of the group
       */
compute_gravity:
      gp->width = gp->cgrav = 0.0;
      x = 0.0;
      for (i = 0, op = gp->fp; i < gp->n; i++, op++) {
        x += op->pos;   /* use double */
        gp->width += (op->lsize + op->rsize) * op->scale;
      }
      gp->cgrav = x / gp->n;
      /*
       * now check that the group does not go over the lower or upper boundary
       */
      if (sbdy[ns] > gp->cgrav - gp->width / 2)
        gp->cgrav = sbdy[ns] + gp->width / 2;
      else if (sbdy[ns + 1] < gp->cgrav + gp->width / 2)
        gp->cgrav = sbdy[ns + 1] - gp->width / 2;
      /*
       * if there is another object in the sector, check it;
       * if it is to close to the upper edge of the group, add it to it.
       */
      if (no < nr_in_sec - 1) {
        if ((cop+1)->pos - (cop+1)->lsize < gp->cgrav + gp->width / 2) {
          cop++;
          no++;
          gp->n++;
          goto compute_gravity;
        }
      }
      /*
       * now test overlap of this group with previous group;
       * if lower edge of current group <= upper edge of previous, unite them.
       */
      if (ng > 0 && gp->cgrav - gp->width/2 <= (gp-1)->cgrav + (gp-1)->width/2) {
        (gp - 1)->n += gp->n;
        gp--;
        ng--;
        goto compute_gravity;
      }
    }   /* for ng */
    /*
     * compute final positions for all groups in the sector
     */
    for (i = 0, gp = grp; i < ng; i++, gp++) {
      pos = gp->cgrav - gp->width / 2;
      for (n = 0, op = gp->fp; n < gp->n; n++, op++) {
        pos += op->lsize * op->scale;
        op->ppos = pos;
        pos += op->rsize * op->scale;
      }
    }
    free(grp);
  } /* for ns */
  return retval;
}

/*
 * NEW grav_group2(), with circular grouping and two-level handling.
 *
 * The elements are placed in a sector if they have lpos >= lbdy[sector]
 * and lpos < rbdy[sector].
 * If an element does not fit into a sector, it is placed at the lowest
 * or highest boundary.
 * sector_no is set to -1 and ERR is returned from the call.
 *
 * For circular grouping set nsectors = 0, and define maximum value
 * (360 degrees or 129600000 csec, etc.) in sectors[1].
 *
 * The elements are placed at sbdy + lsize;
 * There must be nsector+1 sbdy values. (except if nsector = 0)
 *
 * If we need some more empty space at a sector edge, we just introduce
 * a dummy element at the sector edge which holds this space.
 * If we need an empty space on the second level, we set
 * level_no = 1
 *
 * Elements in a circle may have different sizes dependent on the angle
 * where they are put. The caller must set these sizes approximately
 * correct.
 *
 * In case of error, an error message is put into err and ERR is returned.
 */
int grav_group2(GROB *grobs, int nob, double *sbdy, int nsectors, AS_BOOL may_shift, char *err)
{
  int i, no, ns;
  int retval = OK;
  int sect_prev;
  int *nob_sect;
  GROB *op;
  GROB **psect = NULL;  /* silence gcc */
  AS_BOOL do_circular_grouping = FALSE;
  double d, dsave, pos0 = 0;
  if (nsectors == 0) {
    do_circular_grouping = TRUE;
    nsectors = 1;
  }
  if (nob < 1 ) {
    sprintf(err,"grav_group: %d elements.", nob);
    return ERR;
  }
  if (nsectors < 0 ) {
    sprintf(err,"grav_group: %d sectors.", nsectors);
    return ERR;
  }
  /*
   * put the original sequence numbers into the elements, then
   * sort the elements by their position;
   * It is really expected that sectors are presorted correctly.
   */
  for (i = 0, op = grobs; i < nob; i++, op++) {
    if (op->pos < sbdy[0]) {
      op->pos = sbdy[0];
      sprintf(err,"element %d is below first sector.", i);
      retval = ERR;
    } else if (op->pos > sbdy[nsectors]) {
      op->pos = sbdy[nsectors] - 1;
      sprintf(err,"element %d is above last sector.", i);
      retval = ERR;
    } else if (op->pos == sbdy[nsectors]) {
      op->pos = sbdy[nsectors] - 1;     /* force it inside */
    }
    /*
     * find the sector for each element
     */
    for (ns = 1; ns <= nsectors; ns++)
      if (op->pos < sbdy[ns] && op->pos >= sbdy[ns-1]) {
        op->sector_no = ns - 1;
        break;
      }
    op->sequence_no = i;
    op->scale = 1.0;
  }
  qsort(grobs, nob, sizeof(GROB),
    (int (*)(const void *, const void *))(grob_compare));
  if ((nob_sect = calloc(nsectors, sizeof(int))) == 0) {
    if (err != NULL)
      strcpy(err, "grav_group(): calloc nob_sect failed.");
    goto end_grav_group2;
  }
  if ((psect = calloc(nsectors, sizeof(GROB *))) == 0) {
    if (err != NULL)
      strcpy(err, "grav_group(): calloc psect failed.");
    goto end_grav_group2;
  }
  /*
   * if no sectors, prepare for circular grouping
   */
  if (do_circular_grouping) {
    /* find greatest space in circle */
    for (op = grobs, i = dsave = pos0 = 0; i < nob; op++, i++) {
      if (i == 0)
        d = op->pos - (op+nob-1)->pos + sbdy[1];
      else
        d = op->pos - (op-1)->pos;
      if (d > dsave) {
        dsave = d;
        pos0 = op->pos - d / 2;
        if (pos0 < 0)
          pos0 += sbdy[1];
      }
    }
    /* refer all positions to pos0, will be undone after grouping */
    for (op = grobs, i = 0; i < nob; op++, i++) {
      op->pos -= pos0;
      if (op->pos < 0)
        op->pos += sbdy[1];
    }
    qsort(grobs, nob, sizeof(GROB),
      (int (*)(const void *, const void *))(grob_compare));
  }
  /* find begin of each sector in grob array and count objects in it */
  for (no = 0, psect[0] = op = grobs, sect_prev = -1, ns = 0;
       no < nob;
       no++, op++, nob_sect[ns]++) {
     if (op->sector_no != sect_prev) {
       sect_prev = op->sector_no;
       ns = op->sector_no;              /* next sector */
       psect[ns] = op;  /* pointer to it */
     }
  }
  /*
   * for each sector, group objects
   */
  for (ns = 0; ns < nsectors; ns++) {
    if ((retval = grav_group_shaker(psect[ns], nob_sect[ns], sbdy + ns, may_shift, err)) != OK)
      goto end_grav_group2;
  } /* for ns */
  /* if circular grouping, return to original reference system */
  if (do_circular_grouping && retval == OK) {
    for (op = grobs, i = 0; i < nob; op++, i++) {
      op->pos += pos0;
      op->ppos += pos0;
      if (op->pos >= sbdy[1])
        op->pos -= sbdy[1];
      if (op->ppos >= sbdy[1])
        op->ppos -= sbdy[1];
    }
  }
end_grav_group2:
  if (nob_sect != NULL)
    free(nob_sect);
  if (psect != NULL)
    free(psect);
  return retval;
}

static int grav_group_shaker(GROB *grobs, int nob, double *sbdy, AS_BOOL may_shift, char *err)
{
  int i;
  int mpre, mpat, mpatmax, fpat;
  double fprice, price, hprice, vprice,pprice;
  int nlevels = 1;
  int retval;
  if (nob == 0)
    return OK;
  /*
   * find out, whether one or two levels are being worked on
   */
  for (i = 0; i < nob; i++) {
    if (grobs[i].level_no == 1)
      nlevels = 2;
  }
  /*********************************************************************
   * simple call of grav_group_sector() in following cases:
   * - if only one object in grobs
   * - if more than 20 objects in grobs
   * - if shifting is not allowed
   *********************************************************************/
  if (!may_shift || nob > MAX_OBJ_SHAKE || nob == 1)
    return grav_group_sector(grobs, nob, sbdy, may_shift, nlevels, err);
  /*********************************************************************
   * otherwise try all possible shift patterns
   * and choose cheapest one.
   * the following factors are considered as costs:
   * - horizontal stress
   * - vertical stress
   * - scaling stress
   * - destruction of objects order
   *********************************************************************/
  /*
   * predefined distribution pattern mpre:
   * every object on level 1 gets a bit.
   */
  for (i = 0, mpre = 0; i < nob; i++)
    mpre = mpre * 2 + (grobs[i].level_no > 0);
  mpatmax = (1 << nob) - 1;
  /*
   * loop through all possible patterns
   */
  for (fpat = mpat = mpre, fprice = 1e308; mpat <= mpatmax; mpat++) {
    /* patterns that conflict with mpre are forbidden */
    if ((mpat & mpre) != mpre)
      continue;
    /*
     * set all levels in grobs according to mpat
     */
    for  (i = 0; i < nob; i++) {
      if (mpat & (1 << (nob-i-1))) {
        grobs[i].level_no = 1;
      } else {
        grobs[i].level_no = 0;
      }
    }
    /*
     * group objects with this pattern
     */
    if ((retval = grav_group_sector(grobs, nob, sbdy, FALSE, 2, err)) != OK) {
      return retval;
    }
    /*
     * measure horizontal stress
     */
    hprice = 0;
    for (i = 0; i < nob; i++) {
      /* we reset scale because we don't need it here */
      hprice += ((grobs[i].ppos - grobs[i].pos) * CS2DEG)
      * ((grobs[i].ppos - grobs[i].pos) * CS2DEG) / grobs[i].scale / grobs[i].scale;
    }
    /*
     * measure vertical stress
     */
    vprice = 0;
    for (i = 0; i < nob; i++) {
      if (grobs[i].level_no > 0) {
        vprice += (grobs[i].lsize + grobs[i].rsize) * CS2DEG
        * (grobs[i].lsize + grobs[i].rsize) * CS2DEG * VERT_FACT;
        /* leveling of later objects is cheaper */
        vprice += (nob - i) * INF_WEIGHT;
      }
    }
    /*
     * measure undercut and scale penalties
     * if first is predefined in level 1, second may undercut without penalty
     */
    pprice = 0;
    for (i = 0; i < nob; i++) {
      if (grobs[i].scale < 1)
        pprice += (1 - grobs[i].scale) * 40; /* scale 0.9 costs like 2 deg shift */
      grobs[i].scale = 1;
      if (i == 0 && grobs[i].level_no > 0) continue;    /* allowed undercut */
      if (i < nob - 1 && grobs[i+1].ppos < grobs[i].ppos)
        pprice += PENALTY;
    }
    /*
     * if this pattern is cheaper, save it
     */
    price = hprice + vprice + pprice;
    if (price < fprice) {
      fprice = price;
      fpat = mpat;
    }
  }
  /*
   * now best pattern is found.
   * redo grouping with this pattern.
   */
  /*
   * set all levels in grobs according to mpat
   */
  for  (i = 0; i < nob; i++) {
    if (fpat & (1 << (nob-i-1)))
      grobs[i].level_no = 1;
    else
      grobs[i].level_no = 0;
  }
  /*
   * group objects with this pattern
   */
  if ((retval = grav_group_sector(grobs, nob, sbdy, FALSE, 2, err)) != OK) {
    return retval;
  }
  return OK;
}

/*
 * This function groups objects in a sector.
 * It works as follows:
 * -if two level grouping:
 *      -allocate GROB array (grobs1) for level 1
 *      -move level 1 objects to grobs1
 * -define object groups of level 0 objects
 * -compute new positions of level 0 objects
 * -find out whether or not shifting is required
 * -if shifting required:
 *      -add every second object to level 1 array
 *      -sort level 1 array
 *      -self-call of function for level 0 without shifting
 *      -self-call of function for level 1 without shifting
 * -if two level grouping:
 *      -reunite the two levels in input GROB array
 *      -sort array
 *      -shifting may have caused groups falling in pieces;
 *         make sure that none of new group begins on level one
 */
static int grav_group_sector(GROB *grobs, int nob, double *sbdy, AS_BOOL may_shift, int nlevels, char *err)
{
  int i, retval = OK, io, ig, n;
  int nob0, nob1 = 0;        /* silence gcc */
  double width_sum = 0;
  double pos;
  GROB *op, *opx, *opy, *op0, *op1, *grobs1 = NULL, *cop;
  group *grp, *gp;
  double x, scale;
  AS_BOOL calc_levels = FALSE;
  /*
   * if sector empty, return
   */
  if (nob == 0)
    return OK;
  if (may_shift || nlevels > 1) {
    /*
     * if shifting is allowed, allocate objects array for
     * level 1 (= second level).
     */
    if ((grobs1 = calloc(nob, sizeof(GROB))) == NULL) {
      sprintf(err,"grav_group_sector(): calloc grobs1 failed.");
      return ERR;
    }
    /*
     * if there are objects that MUST be on level 1
     * (i.e. that have been given level one by program
     * calling grav_group2), move these objects into
     * level one array
     */
    for (op = op0 = grobs, op1 = grobs1, nob0 = 0, nob1 = 0, io = 0;
         io < nob;
         op++, io++) {
      /* copy level 1 objects to grobs1 array */
      if (op->level_no == 1) {
        memcpy(op1, op, sizeof(GROB));
        op1++;
        nob1++;
      /* throw them out of grobs array (i.e. overwrite them
       * by following objects) */
      } else {
        if (op != op0)
          memcpy(op0, op, sizeof(GROB));
        op0++;
        nob0++;
      }
      }
    nob = nob0;
  }
  /*
   * sum widths of objects,
   */
  width_sum = 0;
  for (io = 0, op = grobs; io < nob; io++, op++)
    width_sum += op->lsize + op->rsize;
  /*
   * if the objects don't fit into the sector, adjust their scale.
   * if level shifting will be required, the scaling factor will
   * be reset to 1.
   */
  if (width_sum > sbdy[1] - sbdy[0]) {
    scale = 1.0 * (sbdy[1] - sbdy[0]) / width_sum;
    for (io = 0, op = grobs; io < nob; io++, op++)
      op->scale = scale;
  }
  /*
   * build groups:
   * allocate space for maximum number of groups
   */
  if ((grp = calloc(nob, sizeof(group))) == NULL) {
    sprintf(err,"grav_group_sect(): calloc grp failed.");
    retval = ERR;
  }
  /*
   * to form groups,
   * go through all objects in sector, start with group 0
   */
  for (ig = 0, gp = grp, cop = grobs, io = 0; io < nob; ig++, gp++, cop++, io++) {
    gp->n = 1;  /* start new group, one object in it */
    gp->fp =cop;
    /*
     * compute width and sector of gravity of the group
     */
compute_gravity:
    gp->width = gp->cgrav = 0;
    x = 0.0;
    for (i = 0, op = gp->fp; i < gp->n; i++, op++) {
      x += op->pos;   /* use double */
      gp->width += (op->lsize + op->rsize) * op->scale;
    }
    gp->cgrav = x / gp->n;
    /*
     * now make sure that the group does not go beyond
     * lower or upper boundary
     */
    if (sbdy[0] > gp->cgrav - gp->width / 2)
      gp->cgrav = sbdy[0] + gp->width / 2;
    else if (sbdy[1] < gp->cgrav + gp->width / 2)
      gp->cgrav = sbdy[1] - gp->width / 2;
    /*
     * if there is another object in the sector;
     * if it is too close to upper edge of group, add it to it.
     */
    if (io < nob - 1) {
      if ((cop+1)->pos - (cop+1)->lsize < gp->cgrav + gp->width / 2) {
        cop++;
        io++;
        gp->n++;
        goto compute_gravity;
      }
    }
    /*
     * now test overlap of this group with previous group;
     * if lower edge of current group <= upper edge of previous,
     * unite them.
     */
    if (ig > 0 && gp->cgrav - gp->width/2 <= (gp-1)->cgrav + (gp-1)->width/2) {
      (gp - 1)->n += gp->n;
      gp--;
      ig--;
      goto compute_gravity;
    }
  }     /* for ig */
  /*
   * compute positions for all groups in the sector
   */
  calc_levels = FALSE;
  for (i = 0, gp = grp; i < ig; i++, gp++) {
    pos = gp->cgrav - gp->width / 2;
    gp->calc_levels = FALSE;
    for (n = 0, op = gp->fp; n < gp->n; n++, op++) {
      pos += op->lsize * op->scale;
      op->ppos = pos;
      pos += op->rsize * op->scale;
      /*
       * find out whether or not shifting is required
       * for this group
       */
      /* Alois 18.11.98: if this shift is mostly due to the boundary
         repulsion, it is no reason to turn calc_levels on!
         Therefore, it should not be applied to the first and last in the
         sector.
         */
      while (may_shift > 0) {
        if (fabs(op->ppos - op->pos) < (op->lsize + op->rsize) / 3) break;
        if (op == grobs && op->ppos - op->pos > 0) break;
        if (op == grobs + nob - 1 && op->ppos - op->pos < 0) break;
        gp->calc_levels = TRUE;
        calc_levels = TRUE;
        break;
      }
    }
  }
  if (calc_levels) {
    /*
     * if shift is necessary reset scaling factor
     */
    for (io = 0, op = grobs; io < nob; io++, op++)
      op->scale = 1;
    /*
     * move every second member of group to level 1
     */
    for (i = 0, gp = grp; i < ig; i++, gp++) {
      if (gp->calc_levels) {
        for (n = 1, op = gp->fp + 1; n < gp->n; n++, op++) {
          if (n % 2 == 1)
            op->level_no = 1;
        }
      }
    }
    /*
     * move level 1 objects to grobs1 array
     */
    for (op = op0 = grobs, op1 = grobs1 + nob1, nob0 = 0, i = 0;
         i < nob;
         i++, op++) {
      /* copy level 1 objects to grobs1 array */
      if (op->level_no == 1) {
        memcpy(op1, op, sizeof(GROB));
        op1++;
        nob1++;
      /* throw them out of grobs array */
      } else {
        if (op != op0)
          memcpy(op0, op, sizeof(GROB));
        op0++;
        nob0++;
      }
    }
    nob = nob0;
    /* level 1 array must be sorted for self-call of
     * grav_group_sector */
    sort_sector(grobs1, nob1);
  }
  /*
   * if shifting has been done,
   * self-call of this function without shifting for level 0
   */
  if (calc_levels) {
    if ((retval = grav_group_sector(grobs, nob, sbdy, FALSE, 1, err)) != OK) {
      free(grp);
      return retval;
    }
  }
  /*
   * if either shifting has been done or two levels must be grouped,
   * self-call of this function without shifting for level 1
   */
  if (calc_levels || nlevels > 1) {
    if ((retval = grav_group_sector(grobs1, nob1, sbdy, FALSE, 1, err)) != OK) {
      free(grp);
      return retval;
    }
  }
  /*
   * reunite levels, add level 1 to in grobs again
   */
  if (may_shift || nlevels > 1) {
    for (op = grobs + nob, op1 = grobs1, i = 0;
         i < nob1;
         i++, op++, op1++, nob++)
      memcpy(op, op1, sizeof(GROB));
  }
  /*
   * sort of grobs, not really necessary unless
   * array is worked on further
   */
  sort_sector(grobs, nob);
  opy = grobs + 1;
  io = 1;
  if (grobs->level_no == 1) {
    opy = grobs + 2;
    io = 2;
  }
  /*
   * with level shifting, groups may have broken apart.
   * as a result, groups may appear on the chart that
   * begin on level 1 instead of 0, because we have
   * moved every second member of previous group into
   * level 1 in a strict way.
   * this can be corrected if we look for level one
   * objects after gaps and invert levels of cluster
   * following such objects:
   */
  if (calc_levels) {
    for (op = opy; io < nob;) {
      opx = op - 1;
      if (opx->level_no == 1)
        opx = op - 2;
      if (/* if level 1 follows level 0 */
          op->level_no == 1
          /* if difference between labels > minimum distance */
          && op->ppos - opx->ppos > (op->lsize + opx->rsize)) {
        op->level_no = 0;
        op++;
        io++;
        while (io < nob
            && op->ppos - (op-1)->ppos <= (op->lsize + (op-1)->rsize)) {
          if (op->level_no == 1)
            op->level_no = 0;
          else
            op->level_no = 1;
          op++;
          io++;
        }
      } else {
        op++;
        io++;
      }
    }
  }
  free(grp);
  if (grobs1 != NULL)
    free(grobs1);
  return retval;
}

static int sort_sector(GROB *grobs, int nob)
{
  qsort(grobs, nob, sizeof(GROB),
    (int (*)(const void *, const void *))(grob_compare));
  return OK;
}
