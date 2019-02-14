/*
 * angle.h
 *
 * Angle defintion and trigonometric functions
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
 * Copyright 1997
 */

#include <math.h>

#ifndef _ANGLE_H
#define _ANGLE_H

/* Define floats as sclars for now */
typedef double ANGLE;

#define DEG_TO_ANGLE(x) (((x)*M_PI)/180.0)

/* Soon to be lookup tables, but this is fine for now */
#define SIN(x) sin(x)
#define COS(x) cos(x)
#define TAN(x) tan(x)

#endif







