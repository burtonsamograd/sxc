/*
 * smoke.c
 *
 * Simulation of smoke in a turbulent airflow
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

/* Local includes */
#include "screen.h"
#include "window.h"
#include "random.h"

/* Module globals */
static int SmokePosX;
static int SmokePosY;

void InitSmoke(unsigned long Arg)
{
  WINDOW Win = (WINDOW)Arg;
  RGBCOLOR Color;
  int i;

  /* Set the palette colors */
  for(i=0;i<128;i++)
	{
	  Color.r = i;
	  Color.g = i;
	  Color.b = i;

	  SetCurPalColor(i++, &Color);
	  SetCurPalColor(i, &Color);
	}
  memset(&Color, 0, sizeof(Color));

  Color.r = 63;
  Color.g = 63;
  Color.b = 0;
  for(i=0;i<63;i++)
	{
	  SetCurPalColor(i+64, &Color);
	  Color.g--;
	}

  SetWinBackgroundColor(Win, 0);
  ClearWindow(Win);

  SmokePosX = GetWinWidth(Win)/2;
  SmokePosY = GetWinHeight(Win)/2;
}

void UpdateSmoke(unsigned long Arg)
{
  WINDOW Win = (WINDOW)Arg;
  int i, j;
  unsigned char *SourceBuffer, *DestBuffer;
  char Val;

  SourceBuffer = GetWinSurface(Win) + GetWinPitch(Win);
  DestBuffer = GetWinSurface(Win);

  for(i=GetWinHeight(Win);i>2;i--)
	{
	  Val = Random();
	  Val /= 96;

	  SourceBuffer += GetWinPitch(Win);
	  DestBuffer += GetWinPitch(Win);	  

	  for(j=Val;j<GetWinWidth(Win);j++)
		{
		  if(SourceBuffer[j])
			{
			  SourceBuffer[j]--;
			  DestBuffer[j-Val] = (SourceBuffer[j] +
								   SourceBuffer[j+1] +
								   SourceBuffer[j-1] +
								   SourceBuffer[j+GetWinPitch(Win)])/4;
			}
		}
	}
}

void SeedSmoke(unsigned long Arg)
{
  WINDOW Win = (WINDOW)Arg;

  int Offset = SmokePosX + SmokePosY*GetWinPitch(Win);

  *(GetWinSurface(Win) + Offset) = Random()>>2;
  *(GetWinSurface(Win) + Offset-1) = Random()>>2;
  *(GetWinSurface(Win) + Offset + 1) = Random()>>2;
  *(GetWinSurface(Win) + Offset+GetWinPitch(Win)) = Random()>>2;
}

/* Update the smoke source */
/* Return -1 when the match hits the bottom of the screen. */
int MoveSmoke(WINDOW Win, int dx, int dy)
{
  SmokePosX += dx;
  SmokePosY += dy;

  if(SmokePosX > GetWinWidth(Win)-2)
	SmokePosX = GetWinWidth(Win)-2;

  if(SmokePosX < 0)
	  SmokePosX = 0;

  if(SmokePosY > GetWinHeight(Win)-2)
	{
	  SmokePosY = GetWinHeight(Win)-2;
	  return -1;
	}

  if(SmokePosY < 2)
	  SmokePosY = 0;

  return 0;
}

