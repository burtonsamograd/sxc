/*
 * camera.h
 *
 * View volume and projection matrix functions.
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
#include "matrix.h"
#include "vector.h"
#include "window.h"

#ifndef _CAMERA_H
#define _CAMERA_H

/* Camera structure */
typedef struct tagCAMERA
{
  SCALAR NearZ, FarZ;
  VECTOR Origin;
  VECTOR Direction;
  VECTOR Up;
  SCALAR FocalLength;
  ANGLE FOV;
  WINDOW Window;
  SCALAR ProjectionRatio;
  SCALAR HalfScreenWidth;
  SCALAR HalfScreenHeight;
  SCALAR WorldWidth;
  SCALAR WorldHeight;
  SCALAR AspectRatio;
} CAMERA;

/* Externs */
extern CAMERA Camera;
extern MATRIX CameraMatrix;

/* Initialize the camera system */
void InitCamera(void);

/* Projection view volume setting functions */
void SetCameraNearZ(SCALAR z);
void SetCameraFarZ(SCALAR z);
void SetCameraOrigin(VECTOR *v);
void SetCameraViewPoint(VECTOR *v);
void SetCameraDirection(VECTOR *v);
void SetCameraUp(VECTOR *v);
void SetCameraFocalLength(SCALAR fl);
void SetCameraFieldOfView(ANGLE fov);
void SetCameraWindow(WINDOW Window);

void UpdateCameraVariables(void);

/* Create the projection matrix */
void CreateCameraMatrix(void);

/* View volume clipping functions */
int ClipPoint(VECTOR *o, VECTOR *v);
int ClipLine(VECTOR *v1, VECTOR *v2);

/* Rendering projection functions */
void ProjectPoint(VECTOR *o, VECTOR *v);
void Project(VECTOR *v, long Num);

/* Get a pointer to the camera matrix */
MATRIX *GetCameraMatrix(void);

#endif







