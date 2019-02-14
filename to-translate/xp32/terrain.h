/*
 * terrain.h
 *
 * Terrain generation engine using simulated fault generation.
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

#ifndef _TERRAIN_H
#define _TERRAIN_H

/* Terrain data structure */
typedef struct tagTERRAINDATA
{
  int Width, Height;
  int Size;
  unsigned char *Data;
  int Seed;
  int Scale;
} TERRAINDATA;

/* Module function definitions */
TERRAINDATA *NewTerrainData(int Width, int Height, int Scale);
int FreeTerrainData(TERRAINDATA **TData);
int BuildTerrain(TERRAINDATA *TData);
int BuildTerrainIteration(TERRAINDATA *TData);

#endif
 
