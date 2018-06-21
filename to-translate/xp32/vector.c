/*
 * vector.c
 *
 * Vector operations implementation.
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

/* C Includes */
#include <string.h>
#include <math.h>

/* Local includes */
#include "vector.h"


void ZeroVector(VECTOR *v)
{
  memset(v, 0, sizeof(*v));
  v->w = 1.0;
}

void SetVector(VECTOR *v, SCALAR x, SCALAR y, SCALAR z)
{
  v->x = x;
  v->y = y;
  v->z = z;
  v->w = 1.0;
}

void ScaleVector(VECTOR *v, SCALAR s)
{
  v->x *= s;
  v->y *= s;
  v->z *= s;
}

/* Add vector b to vector a, leaving b unchanged */
void AddVector(VECTOR *a, VECTOR *b)
{
  a->x += b->x;
  a->y += b->y;
  a->z += b->z;
}

SCALAR AbsVector(VECTOR *v)
{
  double Temp;

  Temp = v->x*v->x;
  Temp += v->y*v->y;
  Temp += v->z*v->z;

  return sqrt(Temp); 
}

/* Perform a normal a.b dor product */
SCALAR DotVector(VECTOR *a, VECTOR *b)
{
  double Temp;

  Temp = a->x*b->x;
  Temp += a->y*b->y;
  Temp += a->z*b->z;
  Temp += a->w*b->w;

  return Temp;
}

/* Perform a normal axb cross product */
void CrossVector(VECTOR *result, VECTOR *a, VECTOR *b)
{
  result->x = a->y*b->z - a->z*b->y;
  result->y = a->z*b->x - a->x*b->z;
  result->z = a->x*b->y - a->y*b->x;
}

/* Normalize a vector */
void NormVector(VECTOR *v)
{
  SCALAR Temp;

  Temp = AbsVector(v);
  Temp = 1/Temp;
  v->x *= Temp;
  v->y *= Temp;
  v->z *= Temp;
}






