/*
 * smoke.h
 *
 * Smoke routine header file.
 *
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

#include "window.h"

#ifndef _SMOKE_H
#define _SMOKE_H

/* Module interface functions */
int InitSmoke(unsigned long Arg);
void UpdateSmoke(unsigned long Arg);
int MoveSmoke(WINDOW Win, int dx, int dy); 
void SeedSmoke(unsigned long Arg);

#endif



