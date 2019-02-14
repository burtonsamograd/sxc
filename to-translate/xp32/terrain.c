/*
 * terrain.c
 *
 * Terrain generation engine using simulated fault
 * generation.
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

/* C includes */
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>

/* Local includes */
#include "terrain.h"
#include "random.h"

typedef struct
{
  int x, y;
} POINT;

/* Local function definitions */
#if 0
void ExtendLineToRect(int Width, int Height, POINT* p1, POINT* p2);
void FaultData(TERRAINDATA *TData, POINT *p1, POINT *p2);
#endif

/* Allocate new terrain data map */ 
TERRAINDATA *NewTerrainData(int Width, int Height, int Scale)
{
  TERRAINDATA *TData;

  /* Allocate the new terrain DATA structure */
  TData = (TERRAINDATA*)malloc(sizeof(*TData));

  /* Set the width and height of the terrain data */
  TData->Width = Width;
  TData->Height = Height;
  TData->Scale = Scale;

  /* Calculate the buffer size */
  TData->Size = TData->Width*TData->Height;

  /* Allocate the data buffer */
  TData->Data = (unsigned char *)malloc(TData->Size);
  assert(TData->Data);

  return TData;
}

int FreeTerrainData(TERRAINDATA **TData)
{
  /* Free the terrain data buffer */
  if((*TData)->Data == NULL)
	free((*TData)->Data);

  /* Free the terrain parameter block */
  free(*TData);

  /* Null the pointer */
  *TData = NULL;

  return 0;
}

/* Build a complete set of terrain data based on the parameters given */
int BuildTerrain(TERRAINDATA *TData)
{
  /* Perform all of the terrain building iterations */
  while(BuildTerrainIteration(TData) == 0);

  return 0;
}


/* Build a section of the terrain by generating fractal information */
int BuildTerrainIteration(TERRAINDATA *TData)
{
  int i, j;
  unsigned char *Temp;

  memset(TData->Data, 0, TData->Size);
  Temp = TData->Data+(1+TData->Width);
  for(i=1;i<TData->Height-1;i++)
	{
	  for(j=1;j<TData->Width-1;j++)
		{
		  *Temp = random()%128;
		  Temp++;
		}
	  Temp += 2;
	}

  /* Move the buffer pointer to the second scan line of the 
   window's surface */
  Temp = TData->Data+(1+TData->Width*1);

  /* Filter the window buffer */
  for(i=1;i<TData->Height-1;i++)
	{
	  for(j=1;j<TData->Width-1;j++)
		{
		  *Temp = (*(Temp-1)+
				   *(Temp-TData->Width)+
				   *(Temp+1)+
				   *(Temp+TData->Width) +
				   *(Temp-2)+
				   *(Temp-(TData->Width*2))+
				   *(Temp+2)+
				   *(Temp+(TData->Width*2)))/8/*  + (char)Random()%16;*/;
		  Temp++;
		}
	  Temp += 2; 
	}

  /* Move the buffer pointer to the second scan line of the 
   window's surface */
  Temp = TData->Data+(1+TData->Width*1);

  /* Filter the window buffer */
  for(i=1;i<TData->Height-1;i++)
	{
	  for(j=1;j<TData->Width-1;j++)
		{
		  *Temp = (*(Temp-1)+
				   *(Temp-TData->Width)+
				   *(Temp+1)+
				   *(Temp+TData->Width))/4;
		  Temp++;
		}
	  Temp += 2; 
	}
  
  return -1;
}

#if 0
/* Perform a single terrain building iteration based on the current
   parameters, and then update the parameters for the next iteration.
   If this was the last iteration, return non-zero. */
int BuildTerrainIteration(TERRAINDATA *TData)
{
  POINT p1, p2;

  /* Calculate the random points withing the square */
  p1.x = Random()%TData->Width;
  p1.y = Random()%TData->Height;
  p2.x = Random()%TData->Width;
  p2.y = Random()%TData->Height;

  /* Extend the point to the edge of the bouding rect */
  ExtendLineToRect(TData->Width, TData->Height, &p1, &p2);

  /* Update the data using the given fault line */
  FaultData(TData, &p1, &p2);
  
  return 0;
}

/* Extend a line defined by points p1 and p2 that are contained
   within a rectangle of width and height, to the edges of the
   rectangle */
void ExtendLineToRect(int Width, int Height, POINT* p1, POINT* p2)
{
  
}

/* Using the two points that lie on the outer edge of the terain
   data rectangle, generate a fault in the data */
void FaultData(TERRAINDATA *TData, POINT *p1, POINT *p2)
{
  
}
#endif


