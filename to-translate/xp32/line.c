/*
 * line.c
 *
 * A set of cursor and line drawing routines.
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
#include <assert.h>
#include <string.h>

/* Local includes */
#include "window.h"
#include "line.h"

static unsigned char Color;
static COOR CurX, CurY;

/* Initialization */
void InitLine(void)
{
  CurX = CurY = 0;
  Color = 0;
}


/* Line color set/get */
void SetLineColor(unsigned char Index)
{
  Color = Index;
}

void GetLineColor(unsigned char *Index)
{
  *Index = Color;
}

/* Draw a line */
void Line(WINDOW Win, COOR x1, COOR y1, COOR x2, COOR y2)
{
  long dx, dy;
  unsigned char *Dest;
  short i;
  long XInc, YInc;

  assert(x1 < GetWinWidth(Win));
  assert(x2 < GetWinWidth(Win));
  assert(y1 < GetWinHeight(Win));
  assert(y2 < GetWinHeight(Win));

  /* Calculate deltas */
  dx = x2 - x1;
  dy = y2 - y1;

  /* Init the incriment values */
  XInc = 1;
  YInc = GetWinPitch(Win);
  
  /* Make sure dx and dy are > 0 and set the inc values 
     properly */
  if(dx < 0)
	{
	  dx *= -1;
	  XInc *= -1;
	}
  if(dy < 0)
	{
	  dy *= -1;
	  YInc *= -1;
	}

  /* Calculate the position in the buffer to start */
  Dest = GetWinSurface(Win) + x1 + (y1*GetWinPitch(Win));
  
  /* Special cases */
  if(dy == dx)
	/* Diagonal line */
	{
	  for(i=0; i<=dx; i++)
		{
		  *Dest = Color;
		  Dest += XInc + YInc;
		}
	}
  else if(dy == 0)
	/* Horizontal line */
	{
	  for(i=0;i<=dx;i++)
		{
		  *Dest = Color;
		  Dest += XInc;
		}
	}
  else if(dx == 0)
	/* Vertical line */
	{
	  for(i=0;i<=dy;i++)
		{
		  *Dest = Color;
		  Dest += YInc;
		}
	}
  else
	{
	  /* Non-special case line */
	  unsigned short Error;
	  long Length;

	  Error = 0;
	  if(dx > dy)
		{
		  Length = dx+1;
		  while(Length--)
			{
			  *Dest = Color;
			  Dest += XInc;

			  Error += dy;
			  if(Error > dx)
				{
				  Error -= dx;
				  Dest += YInc;
				}
			}
		}
	  else
		{
		  Length = dy+1;
		  while(Length--)
			{
			  *Dest = Color;
			  Dest += YInc;

			  Error += dx;
			  if(Error > dy)
				{
				  Error -= dy;
				  Dest += XInc;
				}
			}
		}
	}
}

/* Cursor (logo) based line drawing */
void MoveTo(COOR x, COOR y)
{
  CurX = x;
  CurY = y;
}

/* Draw a line from the current cursor position to a new one */
void LineTo(WINDOW Win, COOR x, COOR y)
{
  Line(Win, CurX, CurY, x, y);
  MoveTo(x, y);
}


void GetCursor(COOR *x, COOR *y)
{
  *x = CurX;
  *y = CurY;
}






