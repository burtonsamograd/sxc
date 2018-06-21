/*
 * goo.c
 *
 * Gooey plasma effecct.
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

/* C Includes */
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/* Local includes */
#include "window.h"
#include "screen.h"
#include "random.h"

void InitGoo(WINDOW Win)
{
  long i;
  float fTemp, fAngle;
  unsigned char cTemp;
  RGBCOLOR Color;
  unsigned char *Surface = GetWinSurface(Win);
  int Width = GetWinWidth(Win);
  int Height = GetWinHeight(Win);
  
  /* Cover the surface of the window with random pixels */
  for(i=0;i<Width*Height;i++)
	*(Surface+i) = Random();

  fAngle = 0.0;
  /* Set the palette colors */
  for(i=0;i<256;i++)
	{

	  fTemp = (-0.003937)*(((float)(i*i))-((float)(i*255)));
	  cTemp = (unsigned char)fTemp;
	  Color.r = 0;
  	  Color.g = 0;
	  Color.b = cTemp;
	  SetCurPalColor(i, &Color);
	}
}

void FilterWindow(WINDOW Win, int Overflow)
{
  unsigned char *Temp;
  long i, j;
  unsigned char *Offscreen = GetWinSurface(Win);
  unsigned short Inc;
  int Pitch = GetWinPitch(Win);
  int Height = GetWinHeight(Win);
  int Width = GetWinWidth(Win);

  /* Calculate the increment value for the outer loop */
  Inc = Pitch - GetWinWidth(Win) + 4;

  /* Move the buffer pointer to the second scan line of the 
   window's surface */
  Temp = Offscreen+(2+GetWinPitch(Win)*2);

  /* Filter the window buffer */
  Height -= 2;
  Width -= 2;
  for(i=2;i<Height;i++)
	{
	  for(j=2;j<Width;j++)
		{
		  *Temp = (*(Temp-1)+
				   *(Temp+1)+
				   *(Temp+Pitch) +
				   *(Temp-Pitch))/4+Overflow;

		  Temp++;
		}
	  Temp += 4; 
	}
}
			   
/* Update the goo by filtering the window */
void UpdateGoo(WINDOW Win)
{
  FilterWindow(Win, 2);
}






