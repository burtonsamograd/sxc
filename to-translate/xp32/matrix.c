/*
 * matrix.c
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

/* C Includes */
#include <string.h>

/* Local includes */
#include "vector.h"
#include "matrix.h"

/* Create a unit matrix */
void UnitMatrix(MATRIX *m)
{
  memset(m, 0, sizeof(MATRIX));
  m->D.a[0] = m->D.a[5] = m->D.a[10] = m->D.a[15] = 1.0;
  return;
}

/* Scale the values of a matrix by s */
void ScaleMatrix(MATRIX *m, SCALAR s)
{
  ScaleVector(m->D.v+0, s);
  ScaleVector(m->D.v+1, s);
  ScaleVector(m->D.v+2, s);
  ScaleVector(m->D.v+3, s);
  return;
}

/* Multiply two matrices, result = a*b */
void MultMatrix(MATRIX *result, MATRIX *a, MATRIX *b)
{
#if 0
  int i, temp1, temp2;
  

  for(i=0;i<4;i++)
    {
      temp1 = i*4;
      result->D.a[i] = a->D.a[temp1] * b->D.a[i];

      temp2 = i+4;
      result->D.a[temp2] = a->D.a[temp1+1] * b->D.a[temp2];

      temp2 = i+8;
      result->D.a[temp2] = a->D.a[temp1+2] * b->D.a[temp2];

      temp2 = i+12;
      result->D.a[temp2] = a->D.a[temp1+3] * b->D.a[temp2];
    }
#endif
  int i;

  for(i=0;i<4;i++)
	{
	  result->D.v[i].x = 
		a->D.v[i].x * b->D.v[0].x +
		a->D.v[i].y * b->D.v[1].x +
		a->D.v[i].z * b->D.v[2].x +
		a->D.v[i].w * b->D.v[3].x;
	  
	  result->D.v[i].y = 
		a->D.v[i].x * b->D.v[0].y +
		a->D.v[i].y * b->D.v[1].y +
		a->D.v[i].z * b->D.v[2].y +
		a->D.v[i].w * b->D.v[3].y;
	  
	  result->D.v[i].z = 
		a->D.v[i].x * b->D.v[0].z +
		a->D.v[i].y * b->D.v[1].z +
		a->D.v[i].z * b->D.v[2].z +
		a->D.v[i].w * b->D.v[3].z;
	  
	  result->D.v[i].w = 
		a->D.v[i].x * b->D.v[0].w +
		a->D.v[i].y * b->D.v[1].w +
		a->D.v[i].z * b->D.v[2].w +
		a->D.v[i].w * b->D.v[3].w;
	}


}

void InvMatrix(MATRIX *m)
{
  return;
}

/* Multiply a vector by a matrix */
void MatrixMultVec(VECTOR *result, MATRIX *m, VECTOR *v)
{
  result->x = 
	m->D.v[0].x * v->x +
	m->D.v[0].y * v->y +
	m->D.v[0].z * v->z +
	m->D.v[0].w * v->w;
  
  result->y = 
	m->D.v[1].x * v->x +
	m->D.v[1].y * v->y +
	m->D.v[1].z * v->z +
	m->D.v[1].w * v->w;
  
  result->z = 
	m->D.v[2].x * v->x +
	m->D.v[2].y * v->y +
	m->D.v[2].z * v->z +
	m->D.v[2].w * v->w;
}

/* Build a translation matrix from the given vector */
void TransMatrix(MATRIX *m, VECTOR *v)
{
  m->D.v[0].w = v->x;
  m->D.v[1].w = v->y;
  m->D.v[2].w = v->z;
}

/* Build a Xrotation matrix */
void XRotMatrix(MATRIX *m, ANGLE a)
{
  SCALAR CosA, SinA;

  CosA = COS(a);
  SinA = SIN(a);

  m->D.v[1].y = CosA;
  m->D.v[1].z = -SinA;
  m->D.v[2].y = SinA;
  m->D.v[2].z = CosA;
}

/* Build a Y rotation matrix */
void YRotMatrix(MATRIX *m, ANGLE a)
{
  SCALAR CosA, SinA;

  CosA = COS(a);
  SinA = SIN(a);

  m->D.v[0].x = CosA;
  m->D.v[0].z = SinA;
  m->D.v[2].x = -SinA;
  m->D.v[2].z = CosA;
}

/* Build a Z rotation matrix */
void ZRotMatrix(MATRIX *m, ANGLE a)
{
  SCALAR CosA, SinA;

  CosA = COS(a);
  SinA = SIN(a);

  m->D.v[0].x = CosA;
  m->D.v[0].y = -SinA;
  m->D.v[1].x = SinA;
  m->D.v[1].y = CosA;
}
	 




