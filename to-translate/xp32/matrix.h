/*
 * matrix.h
 *
 * Matrix library data structure and routines.
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
#include "vector.h"
#include "angle.h"

#ifndef _MATRIX_H
#define _MATRIX_H

/* 4x4 matrix structure */
typedef struct tagMATRIX
{
  union
  {
  SCALAR a[16];
  VECTOR v[4];  /* Row vectors form matrix */
  }D;
} MATRIX;

/* Matrix operations */
void UnitMatrix(MATRIX *m);
void ScaleMatrix(MATRIX *m, SCALAR s);
void MultMatrix(MATRIX *result, MATRIX *a, MATRIX *b);
void InvMatrix(MATRIX *m);
void MatrixMultVec(VECTOR *result, MATRIX *m, VECTOR *v);

/* Matrix building functions */
void TransMatrix(MATRIX *m, VECTOR *v);
void XRotMatrix(MATRIX *m, ANGLE a);
void YRotMatrix(MATRIX *m, ANGLE a);
void ZRotMatrix(MATRIX *m, ANGLE a);

#define MATRIXMULTVEC(result, mat, vec) \
  result.x = \
	mat.D.v[0].x * vec.x + \
	mat.D.v[0].y * vec.y +\
	mat.D.v[0].z * vec.z +\
	mat.D.v[0].w * vec.w;\
  \
  result.y = \
	mat.D.v[1].x * vec.x +\
	mat.D.v[1].y * vec.y +\
	mat.D.v[1].z * vec.z +\
	mat.D.v[1].w * vec.w;\
  \
  result.z = \
	mat.D.v[2].x * vec.x +\
	mat.D.v[2].y * vec.y +\
	mat.D.v[2].z * vec.z +\
	mat.D.v[2].w * vec.w;\
  

#endif


