/*
 * fire.c
 *
 * Fire routine.
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
#include <string.h>
#include <assert.h>

/* Local includes */
#include "window.h"
#include "screen.h"
#include "random.h"

/* Module constant defines */
#define RANDLT_SIZE 1024

/* Local function definitions */
void InitFirePalette(void);
unsigned char* DoTripleLines(unsigned char *Screen, int Offset);
unsigned char* DoDoubleLines(unsigned char *Screen, int Offset);
unsigned char* DoSingleLines(unsigned char *Screen, int Offset);
void DoRandomLines(unsigned char *Screen, int Offset);
void FirePostBlt(WINDOW Win);
void FirePreBlt(WINDOW Win);

/* Module global variables */
static WINDOW Window;
static int Pitch, PitchTwo, PitchThree, PitchFour, PitchSix;
static int TripleLineValue, DoubleLineValue, SingleLineValue, RandomLineValue;
static int Offset;
static unsigned char *RandLT;

/* Initialize the fire system */
/* This routine expects an offscreen window structure */
void InitFire(unsigned long Arg)
{
  long DrawBufferPitch;
  unsigned long Temp, i;

  /* Save the pointer to the window structure */
  Window = (WINDOW)Arg;

  /* Set up the palette */
  InitFirePalette();

  /* Precalc the pitch multipliers */
  DrawBufferPitch = GetWinPitch(Window);
  Pitch = DrawBufferPitch;
  PitchTwo = DrawBufferPitch*2;
  PitchThree = DrawBufferPitch*3;
  PitchFour = DrawBufferPitch*4;
  PitchSix = DrawBufferPitch*6;

  /* Create the random look up table */
  RandLT = (unsigned char *)malloc(sizeof(unsigned char)*RANDLT_SIZE);
  assert(RandLT);
  for(i=0;i<RANDLT_SIZE/4;i++)
	{
	  /* Create a random lookup table that contains high values 
		 (>191 <255) with groups of four pixels */
	  Temp = Random()%64 + /*191;*/ GetWinHeight(Window);
	  Temp |= Temp<<8;
	  Temp |= Temp<<16;
		
	  ((long*)RandLT)[i] = Temp;		
	}


  TripleLineValue = ((double)GetWinHeight(Window)*57.0/200.0);
  DoubleLineValue = ((double)GetWinHeight(Window)*10.0/200.0);
  SingleLineValue = ((double)GetWinHeight(Window)*7.0/200.0);
  RandomLineValue = GetWinHeight(Window)-TripleLineValue*3 -DoubleLineValue*2-SingleLineValue;

  Offset = GetWinWidth(Window);

  return;
}

/* Cleanup memory */
void CleanupFire(unsigned long Arg)
{
  /* Free the lookup table memory */
  if(RandLT)
	free(RandLT);
  RandLT = NULL;
}

/* Update the fire screen */
/* This is done through a combination of scan tripling, doubling and
   regular evaluation of pixel averaging for the fire effect */
void UpdateFire(unsigned long Arg)
{
  unsigned char *Buffer;

  /* Left and right offset to center the fire 
	 horizontally on the screen */
  if(Offset > 0)
	Offset-=16;

  /* Set the screen pointer */
  Buffer = GetWinSurface(Window);

  /* Update the buffer */
  Buffer = DoTripleLines(Buffer, Offset);
  Buffer = DoDoubleLines(Buffer, Offset);
  Buffer = DoSingleLines(Buffer, Offset);
  DoRandomLines(Buffer, Offset);

#define VERT_SHIFT 15

  /* Copy all but the bottom scan lines to the screen */
  WaitForVerticalBlank(0);

  FirePreBlt(Window);
  StencilCopyToScreen(Window);
  FirePostBlt(Window);
} 

void FirePreBlt(WINDOW Window)
{
  SetWinPosY(Window, GetWinPosY(Window)+20);
  SetWinHeight(Window, GetWinHeight(Window)-20);
}


void FirePostBlt(WINDOW Window)
{
  SetWinPosY(Window, GetWinPosY(Window)-20);
  SetWinHeight(Window, GetWinHeight(Window)+20);
}

/* Probably want to make a palette and have it loaded at 
   this time or something */
void InitFirePalette(void)
{
  static RGBCOLOR FirePal[256] =
	{
	  {0, 0, 0},{8/4, 0, 0},{16/4, 0, 0},{24/4, 0, 0},
		{32/4, 0, 0}, {41/4, 0, 0},{49/4, 0, 0},{57/4, 0, 0},
		{65/4, 0, 0},	{74/4, 0, 0},	{82/4, 0, 0},	{90/4, 0, 0},
		{98/4, 0, 0},{106/4, 0, 0},{115/4, 0, 0},{123/4, 0, 0},
		{131/4, 0, 0},	{139/4, 0, 0},{148/4, 0, 0},{156/4, 0, 0},
		{164/4, 0, 0},{172/4, 0, 0},{180/4, 0, 0},{189/4, 0, 0},
		{197/4, 0, 0},	{205/4, 0, 0},{213/4, 0, 0},{222/4, 0, 0},
		{230/4, 0, 0},{238/4, 0, 0},	{246/4, 0, 0},{255/4, 0, 0},
		{254/4, 9/4, 0},{254/4, 19/4, 0},{254/4, 29/4, 0},{254/4, 39/4, 0},
		{253/4, 49/4, 0},{253/4, 59/4, 0},{253/4, 69/4, 0},{253/4, 79/4, 0},
		{252/4, 88/4, 0},{252/4, 98/4, 0},{252/4, 108/4, 0},
		{252/4, 118/4, 0},{251/4, 128/4, 0},{251/4, 138/4, 0},
		{251/4, 148/4, 0},{251/4, 158/4, 0},{250/4, 167/4, 0},
		{250/4, 177/4, 0},{250/4, 187/4, 0},	{250/4, 197/4, 0},
		{249/4, 207/4, 0},{249/4, 217/4, 0},{249/4, 227/4, 0},
		{249/4, 237/4, 0},{249/4, 247/4, 0},{249/4, 247/4, 0},
		{249/4, 237/4, 0},{249/4, 247/4, 0},{250/4, 247/4, 0},
		{252/4, 237/4, 0},{254/4, 247/4, 0},{255/4, 247/4, 0}};
  int i;

  for(i=0;i<16;i++)
	{
	  SetCurPalColorWithGamma(i, &FirePal[i*2]);
	}
  for(i=32;i<64;i++)
	{
	  SetCurPalColorWithGamma(i-16, &FirePal[i]);
	}
}

/* Screen update functions.  They are diveded into triple lines (top),
   double lines (middle), single lines (bottom) and random (last few
   rows). */
unsigned char* DoTripleLines(unsigned char *Screen, int Offset)
{
  int i,j;

  for(i=0;i<TripleLineValue;i++)
	{
	  /* Fill the x pixels */
	  for(j=Offset;j<GetWinWidth(Window)-Offset;j++)
		{
		  /* Set the pixel */
		  Screen[j] = (unsigned char)(((short)Screen[PitchThree+j] +
									   (short)Screen[PitchSix+j] +
									   (short)Screen[PitchSix+j-1] + 
									   (short)Screen[PitchSix+j+1]))>>2;
		
		  /* Decriment the value so that it will fade */
		  if(Screen[j] < 31 && Screen[j] > 0)
			Screen[j]--;
		}
	  /* Copy the row */
	  memcpy(Screen+Pitch, Screen, GetWinWidth(Window));
	  memcpy(Screen+PitchTwo, Screen, GetWinWidth(Window));
		

	  /* Incriment by the pitch of the surface 
		 (even though its in memory anyways) */
	  Screen += PitchThree;
	}

  return Screen;
}

unsigned char* DoDoubleLines(unsigned char *Screen, int Offset)
{
  int i,j;

  for(i=0;i<DoubleLineValue;i++)
	{
	  /* Fill the x pixels */
	  for(j=Offset;j<GetWinWidth(Window)-Offset;j++)
		{
		  /* Set the pixel */
		  Screen[j] = (unsigned char)(((short)Screen[PitchTwo+j] +
									   (short)Screen[PitchFour+j] +
									   (short)Screen[PitchTwo+j-1] +
									   (short)Screen[PitchTwo+j+1]))>>2;
		}
	  memcpy(Screen+Pitch, Screen, GetWinWidth(Window));

	  Screen += PitchTwo;
	}

return Screen;
}

unsigned char* DoSingleLines(unsigned char *Screen, int Offset)
{
  int i, j;

  /* Single line algo */
  for(i=0;i<SingleLineValue;i++)
	{
	  /* Fill the x pixels */
	  for(j=Offset;j<GetWinWidth(Window)-Offset;j++)
		{
		  /* Set the pixel */
		  Screen[j] = (unsigned char)((((short)Screen[Pitch+j] +
										(short)Screen[PitchTwo+j] +
										(short)Screen[PitchTwo+j-1]+ 
										(short)Screen[PitchTwo+j+1]))>>1);
		}

	  /* Incriment by the pitch of the surface 
			(even though its in memory anyways) */
	  Screen += Pitch;
	}

  return Screen;
}

void DoRandomLines(unsigned char *Screen, int Offset)
{
  int j;
  int Temp;

  /* Calculate the offset into the table */
  Temp = Random()%RANDLT_SIZE;

  /* Fill the two last scan lines */
  for(j=0;j<RandomLineValue;j++)
	{
	  /* Get a random row of pixels from the lookup table */
	  if(Temp + GetWinWidth(Window)< RANDLT_SIZE)
		/* If the offset wont overlap the edge of the lookup table... */
		memcpy(Screen+Offset, &RandLT[Temp], GetWinWidth(Window)-Offset);
	  else
		{
		  /* Else wrap the copy around to the beginning of the lookup table */
		  /* Calculate the number of entries to be copied before overlapping */
		  Temp = RANDLT_SIZE - Temp;

		  /* Copy the entries from the offset to the end of the table */
		  memcpy(Screen, &RandLT[RANDLT_SIZE-Temp], Temp);

		  /* Copy the rest from the beggining of the table */
		  memcpy(Screen+Temp, &RandLT[0], GetWinWidth(Window)- Temp);
		}

	  /* Move to another location in the table */
	  Temp += 10;

	  /* Go to the next scan line */
	  Screen += Pitch;
	}
}






