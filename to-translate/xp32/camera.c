/*
 * camera.c
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

/* C Includes */
#include <string.h>
#include <assert.h>

/* Local includes */
#include "camera.h"
#include "scalar.h"
#include "angle.h"
#include "matrix.h"
#include "vector.h"
#include "window.h"

#define PROJ_RATIO 2.0f

CAMERA Camera;
MATRIX CameraMatrix;

/* Initialize the camera system variables */
void InitCamera(void)
{
  Camera.NearZ = 0.0;
  Camera.FarZ = 50.0;

  Camera.Origin.x = 0.0;
  Camera.Origin.y = 0.0;
  Camera.Origin.z = 0.0;

  Camera.Direction.x = 0.0;
  Camera.Direction.y = 0.0;
  Camera.Direction.z = 1.0;

  Camera.Up.x = 0.0;
  Camera.Up.y = -1.0;
  Camera.Up.z = 0.0;
  
  Camera.FOV = DEG_TO_ANGLE(60);
  SetCameraFocalLength(5);
}

/* Return a pointer to the camera matrix */
MATRIX *GetCameraMatrix(void)
{
  return &CameraMatrix;
}

/* Set the near Z clipping plane */
void SetCameraNearZ(SCALAR z)
{
  Camera.NearZ = z;
}

/* Set the far Z clipping plane */
void SetCameraFarZ(SCALAR z)
{
  Camera.FarZ = z;
}

/* Set the origin (view point) of the camera */
void SetCameraOrigin(VECTOR *v)
{
  Camera.Origin = *v;
}

/* Set the look at point for the vector */
/* This is an alternate for setting the direction
   vector for the camera */
void SetCameraViewPoint(VECTOR *v)
{
  VECTOR Temp;

  Temp = *v;

  /* dv = look at point - origin */
  AddVector(&Temp, &Camera.Origin);

  /* Set the direction vector */
  NormVector(&Temp);
  Camera.Direction = Temp;
}

/* Set the direction vector for the camera */
void SetCameraDirection(VECTOR *v)
{
  NormVector(v);
  Camera.Direction = *v;
}

/* Set the up vector for the camera */
void SetCameraUp(VECTOR *v)
{
  Camera.Up = *v;
}

/* Set the focal lenght for the vector */
void SetCameraFocalLength(SCALAR fl)
{
  Camera.FocalLength = fl;
}

/* Set the field of view angle for the camera */
void SetCameraFieldOfView(ANGLE fov)
{
  Camera.FOV = fov;
}

/* Set the camera rendering window */
void SetCameraWindow(WINDOW Win)
{
  Camera.Window = Win;
}

/* Update dependant camera variables */
void UpdateCameraVariables(void)
{
  Camera.AspectRatio = (SCALAR)GetWinWidth(Camera.Window)/(SCALAR)GetWinHeight(Camera.Window);
  Camera.WorldWidth = Camera.FocalLength*TAN(Camera.FOV/2)*2;
  Camera.WorldHeight = Camera.WorldWidth*Camera.AspectRatio;
  Camera.HalfScreenWidth = ((SCALAR)GetWinWidth(Camera.Window))/2;
  Camera.HalfScreenHeight = ((SCALAR)GetWinHeight(Camera.Window))/2;
  Camera.ProjectionRatio = -Camera.HalfScreenWidth * PROJ_RATIO;
}

/* Create the camera matrix using the current view 
   volume settings */
void CreateCameraMatrix(void)
{
  UnitMatrix(&CameraMatrix);
  NegVector(&Camera.Origin);
  TransMatrix(&CameraMatrix, &Camera.Origin);
  NegVector(&Camera.Origin);
}

/**********************************/
/* View volume clipping functions */
/**********************************/

/* Clip the given point to the current view volume */
/* This will return TRUE if the point is in the view
   volume, or FALSE if it is not */
int ClipPoint(VECTOR *o, VECTOR *v)
{
  return -1;
}

/* Clip the given line to the current view volume */
/* This function will return TRUE if the the whole line
   is contained in the view volume.  If a portion of the
   line is contained in the view volume, the routine will
   modify v1 and v2, such that the resulting line is
   contained within the view volume, and will return TRUE.
   If niether point is contained in the view volume, the
   function will return FALSE */
int ClipLine(VECTOR *v1, VECTOR *v2)
{
  return -1;

}

/**********************************/
/* Rendering projection functions */
/**********************************/

/* Project a single point into screen space using the
   current view volume */
void ProjectPoint(VECTOR *o, VECTOR *v)
{
  SCALAR Temp = Camera.ProjectionRatio/v->z;

  /* Transform the point */
  /*  MatrixMultVec(o, &CameraMatrix, v);*/

  /* Perspective project the point to the view plane */
  o->x = (v->x*Temp) + Camera.HalfScreenWidth;
  o->y = (v->y*Temp) + Camera.HalfScreenHeight;
  o->z = v->z;

}

/* Project an array of points into screen space using
   the current view volume */
void Project(VECTOR *v, long Num)
{
}





