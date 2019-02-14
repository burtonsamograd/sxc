/*
 * vector.h
 *
 * Vector library routines.
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

#include "scalar.h"

#ifndef _VECTOR_H
#define _VECTOR_H

/* Vector type */
typedef struct tagVECTOR
{
  SCALAR x, y, z, w;
} VECTOR;

/* Vector operatons */
void ZeroVector(VECTOR *v);
void SetVector(VECTOR *v, SCALAR x, SCALAR y, SCALAR z);
void ScaleVector(VECTOR *v, SCALAR s);
void AddVector(VECTOR *a, VECTOR *b);
SCALAR AbsVector(VECTOR *v);
SCALAR DotVector(VECTOR *a, VECTOR *b);
void CrossVector(VECTOR *result, VECTOR *a, VECTOR *b);
void NormVector(VECTOR *v);

#define NegVector(v) ScaleVector(v, -1.0)

#endif







