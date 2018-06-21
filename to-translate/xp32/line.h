/*
 * line.h

  * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Burt Samograd
 * Copywrite 1997
 */

#ifndef _LINE_H
#define _LINE_H

#include "window.h"

typedef unsigned long COOR;

/* Initialization */
void InitLine(void);

/* Line colors */
void SetLineColor(unsigned char Index);
void GetLineColor(unsigned char *Index); 

/* Draw a line */
void Line(WINDOW Win, COOR x1, COOR y1, COOR x2, COOR y2);

/* Cursor (logo) based line drawing */
void GetCursor(COOR *x, COOR *y);
void MoveTo(COOR x, COOR y);
void LineTo(WINDOW Win, COOR x, COOR y);

#endif





